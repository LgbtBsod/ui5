#!/usr/bin/env python3
import argparse
import json
import sys
import time
from pathlib import Path

import requests

ROOT = Path(__file__).resolve().parents[1]
REPORT_PATH = ROOT / "docs" / "artifacts" / "gateway-lock-multisession-replay.json"


def fail(msg: str) -> int:
    print('gateway-lock-multisession-replay FAIL')
    print(msg)
    return 1


def write_report(payload: dict) -> None:
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")


def summarize_response(resp):
    text = ""
    try:
        text = resp.text[:1000]
    except Exception:  # noqa: BLE001
        text = ""
    return {
        "status": resp.status_code,
        "ok": bool(resp.ok),
        "url": resp.url,
        "body": text
    }


def main() -> int:
    p = argparse.ArgumentParser(description='Replay lock semantics with two sessions against a gateway endpoint.')
    p.add_argument('service_root', help='Service root URL, e.g. http://host/sap/opu/odata/sap/Z_EHS...')
    p.add_argument('root_id', help='Checklist root key')
    args = p.parse_args()

    base = args.service_root.rstrip('/')
    root = args.root_id

    s_a = requests.Session()
    s_b = requests.Session()

    token_resp_a = s_a.get(f'{base}/', headers={'X-CSRF-Token': 'Fetch'}, timeout=20)
    token_a = token_resp_a.headers.get('X-CSRF-Token', '')
    token_resp_b = s_b.get(f'{base}/', headers={'X-CSRF-Token': 'Fetch'}, timeout=20)
    token_b = token_resp_b.headers.get('X-CSRF-Token', '')
    token = token_a or token_b
    report = {
        "generatedAt": int(time.time()),
        "serviceRoot": base,
        "rootId": root,
        "ok": False,
        "checks": [],
        "responses": {}
    }
    if not token:
        write_report(report | {"checks": [{"name": "csrf", "ok": False, "detail": "missing CSRF token"}]})
        return fail('missing CSRF token')

    def post(session: requests.Session, session_guid: str, token_value: str, name: str):
        return session.post(
            f"{base}/{name}?DB_KEY=binary'{root}'&SessionGuid={session_guid}",
            headers={'X-CSRF-Token': token_value},
            timeout=20
        )

    acq_a = post(s_a, 'A', token_a or token, 'LockAcquire')
    report["responses"]["lockAcquireA"] = summarize_response(acq_a)
    if acq_a.status_code != 200:
        report["checks"].append({"name": "lockAcquireA", "ok": False, "detail": report["responses"]["lockAcquireA"]})
        write_report(report)
        return fail(f'LockAcquire A status={acq_a.status_code}')
    report["checks"].append({"name": "lockAcquireA", "ok": True, "detail": report["responses"]["lockAcquireA"]})

    acq_b = post(s_b, 'B', token_b or token, 'LockAcquire')
    report["responses"]["lockAcquireBBeforeRelease"] = summarize_response(acq_b)
    if acq_b.status_code not in (200, 409):
        report["checks"].append({"name": "lockAcquireBBeforeRelease", "ok": False, "detail": report["responses"]["lockAcquireBBeforeRelease"]})
        write_report(report)
        return fail(f'LockAcquire B unexpected status={acq_b.status_code}')
    report["checks"].append({
        "name": "lockAcquireBBeforeRelease",
        "ok": True,
        "detail": report["responses"]["lockAcquireBBeforeRelease"] | {"contentionObserved": acq_b.status_code == 409}
    })

    hb_a = post(s_a, 'A', token_a or token, 'LockHeartbeat')
    report["responses"]["lockHeartbeatA"] = summarize_response(hb_a)
    if hb_a.status_code != 200:
        report["checks"].append({"name": "lockHeartbeatA", "ok": False, "detail": report["responses"]["lockHeartbeatA"]})
        write_report(report)
        return fail(f'LockHeartbeat A status={hb_a.status_code}')
    report["checks"].append({"name": "lockHeartbeatA", "ok": True, "detail": report["responses"]["lockHeartbeatA"]})

    rel_a = post(s_a, 'A', token_a or token, 'LockRelease')
    report["responses"]["lockReleaseA"] = summarize_response(rel_a)
    if rel_a.status_code != 200:
        report["checks"].append({"name": "lockReleaseA", "ok": False, "detail": report["responses"]["lockReleaseA"]})
        write_report(report)
        return fail(f'LockRelease A status={rel_a.status_code}')
    report["checks"].append({"name": "lockReleaseA", "ok": True, "detail": report["responses"]["lockReleaseA"]})

    acq_b2 = post(s_b, 'B', token_b or token, 'LockAcquire')
    report["responses"]["lockAcquireBAfterRelease"] = summarize_response(acq_b2)
    if acq_b2.status_code != 200:
        report["checks"].append({"name": "lockAcquireBAfterRelease", "ok": False, "detail": report["responses"]["lockAcquireBAfterRelease"]})
        write_report(report)
        return fail(f'LockAcquire B after release status={acq_b2.status_code}')
    report["checks"].append({"name": "lockAcquireBAfterRelease", "ok": True, "detail": report["responses"]["lockAcquireBAfterRelease"]})

    hb_b = post(s_b, 'B', token_b or token, 'LockHeartbeat')
    report["responses"]["lockHeartbeatBAfterAcquire"] = summarize_response(hb_b)
    if hb_b.status_code != 200:
        report["checks"].append({"name": "lockHeartbeatBAfterAcquire", "ok": False, "detail": report["responses"]["lockHeartbeatBAfterAcquire"]})
        write_report(report)
        return fail(f'LockHeartbeat B after acquire status={hb_b.status_code}')
    report["checks"].append({"name": "lockHeartbeatBAfterAcquire", "ok": True, "detail": report["responses"]["lockHeartbeatBAfterAcquire"]})

    rel_b = post(s_b, 'B', token_b or token, 'LockRelease')
    report["responses"]["lockReleaseB"] = summarize_response(rel_b)
    report["checks"].append({"name": "lockReleaseB", "ok": rel_b.status_code == 200, "detail": report["responses"]["lockReleaseB"]})
    report["ok"] = all(item["ok"] for item in report["checks"])
    write_report(report)

    print('gateway-lock-multisession-replay PASS')
    return 0


if __name__ == '__main__':
    sys.exit(main())
