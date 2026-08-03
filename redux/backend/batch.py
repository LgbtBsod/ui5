"""OData v2 $batch multipart/mixed handling: parsing the outer batch and
inner changeset parts, dispatching each embedded request, and re-serializing
responses - including changeset all-or-nothing atomicity (snapshot/rollback
over state.store/state.draft_store on any sub-request failure).
"""
import copy
import json
import re

from . import config
from . import state
from . import odata_format
from . import dispatch


def split_on_boundary(text, boundary):
    parts = text.split("--" + boundary)
    result = []
    for p in parts:
        p = p.strip("\r\n")
        if p and p != "--" and not p.startswith("--"):
            result.append(p)
    return result


def parse_embedded_http(raw_part):
    normalized = raw_part.replace("\r\n", "\n")
    header_end = normalized.find("\n\n")
    if header_end == -1:
        return None
    http_msg = normalized[header_end + 2:]
    lines = http_msg.split("\n")
    request_line = lines[0] or ""
    m = re.match(r'^(GET|POST|PUT|PATCH|DELETE|MERGE)\s+(\S+)\s+HTTP', request_line, re.I)
    if not m:
        return None
    hdrs = {}
    i = 1
    for i in range(1, len(lines)):
        line = lines[i]
        if line.strip() == "":
            i += 1
            break
        colon = line.find(":")
        if colon > 0:
            hdrs[line[:colon].strip().lower()] = line[colon + 1:].strip()
    body_text = "\n".join(lines[i:]).strip()
    body = None
    if body_text:
        try:
            body = json.loads(body_text)
        except:
            body = body_text
    return {"method": m.group(1).upper(), "url": m.group(2), "body": body, "headers": hdrs}


def parse_batch_part(raw_part):
    normalized = raw_part.replace("\r\n", "\n")
    header_end = normalized.find("\n\n")
    outer_headers = normalized[:header_end] if header_end >= 0 else normalized
    cs_match = re.search(r'content-type:\s*multipart/mixed;\s*boundary=([^\s;]+)', outer_headers, re.I)
    if cs_match:
        inner_boundary = cs_match.group(1).strip('"')
        rest = normalized[header_end + 2:] if header_end >= 0 else ""
        inner_reqs = [r for r in (parse_embedded_http(p) for p in split_on_boundary(rest, inner_boundary)) if r]
        return {"type": "changeset", "requests": inner_reqs}
    req = parse_embedded_http(raw_part)
    return {"type": "single", "request": req} if req else None


def build_response_part(status, body, content_type="application/json", sap_message=None, etag=None):
    if status == 204 or body is None:
        # [Fix] ETag header on the 204 sub-response - see state._pending_etag's
        # comment. This is the path that actually matters in practice: Fiori
        # Elements' inline-edit autosave always goes through $batch
        # (useBatch:true), never a bare direct PATCH.
        etag_line = "ETag: %s\r\n" % etag if etag else ""
        return (
            "Content-Type: application/http\r\n"
            "Content-Transfer-Encoding: binary\r\n\r\n"
            "HTTP/1.1 204 No Content\r\n"
            "Content-Type: application/json\r\n"
            "%sContent-Length: 0\r\n\r\n" % etag_line
        )
    if isinstance(body, str):
        body_str = body
        ct = content_type
    else:
        body_str = json.dumps(body, ensure_ascii=False)
        ct = "application/json"
    body_bytes = body_str.encode("utf-8")
    # [Fix] Fiori Elements' Save always goes through $batch, so the
    # sap-message header (drives the standard "saved" toast) must be emitted
    # on the individual changeset sub-response here, not just the outer
    # do_POST /$batch response - see state._pop_sap_message().
    sap_message_line = "sap-message: %s\r\n" % sap_message if sap_message else ""
    return (
        "Content-Type: application/http\r\n"
        "Content-Transfer-Encoding: binary\r\n\r\n"
        "HTTP/1.1 %d OK\r\n"
        "Content-Type: %s\r\n"
        "%sContent-Length: %d\r\n\r\n%s"
        % (status, ct, sap_message_line, len(body_bytes), body_str)
    )


def handle_batch(raw_body, content_type_header):
    ct_match = re.search(r'boundary=([^\s;]+)', content_type_header or "")
    if not ct_match:
        return (400, json.dumps({"error": {"message": {"value": "Malformed $batch"}}}), "application/json")
    boundary = ct_match.group(1).strip('"')
    parts = [p for p in (parse_batch_part(p) for p in split_on_boundary(raw_body, boundary)) if p]
    resp_boundary = "batchresponse_" + odata_format.generate_uuid().replace("-", "")
    resp_parts = []

    for part in parts:
        if part["type"] == "single":
            req = part["request"]
            status, body, ct = dispatch.dispatch_request(req["method"], req["url"], req.get("body"), req.get("headers"))
            resp_parts.append(build_response_part(status, body, ct, state._pop_sap_message(), state._pop_pending_etag()))
        elif part["type"] == "changeset":
            # [Fix] Changeset atomicity. OData v2 $batch spec: "If any request
            # within a change set fails, the entire change set MUST be rolled
            # back... the response for the change set MUST be a single
            # response, and MUST be a response for the failed request" - this
            # mock previously had NEITHER property (confirmed live: a 2-request
            # changeset with one valid and one bogus DELETE came back as a
            # mixed 204+404 multipart, with the valid delete actually,
            # permanently applied - exactly the non-atomic behavior the spec
            # forbids). Snapshot store/draft_store before running the
            # changeset's requests; if ANY sub-request fails (status >= 400),
            # restore the snapshot (undoing every mutation from this
            # changeset, including ones that individually "succeeded") and
            # emit ONE flat error response for the whole changeset - not a
            # nested multipart - matching the spec's "single response" rule.
            snapshot_store = copy.deepcopy(state.store)
            snapshot_draft_store = copy.deepcopy(state.draft_store)
            inner_results = []
            failure = None
            for req in part["requests"]:
                status, body, ct = dispatch.dispatch_request(req["method"], req["url"], req.get("body"), req.get("headers"))
                sap_message, etag = state._pop_sap_message(), state._pop_pending_etag()
                inner_results.append((status, body, ct, sap_message, etag))
                if status >= 400 and failure is None:
                    failure = (status, body, ct)
            if failure is not None:
                state.store.clear()
                state.store.update(snapshot_store)
                state.draft_store.clear()
                state.draft_store.update(snapshot_draft_store)
                status, body, ct = failure
                resp_parts.append(build_response_part(status, body, ct))
            else:
                cs_boundary = "changesetresponse_" + odata_format.generate_uuid().replace("-", "")
                inner_parts = [
                    build_response_part(status, body, ct, sap_message, etag)
                    for status, body, ct, sap_message, etag in inner_results
                ]
                cs_body = "".join("--%s\r\n%s\r\n" % (cs_boundary, p) for p in inner_parts) + "--%s--" % cs_boundary
                resp_parts.append("Content-Type: multipart/mixed; boundary=%s\r\n\r\n%s" % (cs_boundary, cs_body))

    resp_body = "".join("--%s\r\n%s\r\n" % (resp_boundary, p) for p in resp_parts) + "--%s--" % resp_boundary
    return (202, resp_body, "multipart/mixed; boundary=%s" % resp_boundary)
