import os
import sys

import pytest
from fastapi import HTTPException

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from utils.odata import SERVICE_ROOT
from utils.odata_batch import extract_boundary, parse_batch_request, parse_http_part


@pytest.mark.parametrize(
    "raw_part",
    [
        (
            "Content-Type: application/http\n"
            "Content-Transfer-Encoding: binary\n\n"
            "Content-ID: 1\n"
            "PATCH ChecklistSet('1') HTTP/1.1\n"
            "Content-Type: application/json\n\n"
            '{"Name": "Updated"}'
        ),
        (
            "Content-Type: application/http\n"
            "Content-Transfer-Encoding: binary\n\n"
            "\n"
            "GET ChecklistSet?$top=1 HTTP/1.1\n"
            "Accept: application/json\n\n"
        ),
    ],
)
def test_parse_http_part_accepts_leading_part_headers_before_request_line(raw_part):
    operation = parse_http_part(raw_part)

    assert operation.method in {"PATCH", "GET"}
    assert operation.path.startswith("ChecklistSet")


def test_parse_http_part_rejects_missing_request_line():
    raw_part = (
        "Content-Type: application/http\n"
        "Content-Transfer-Encoding: binary\n\n"
        "Content-ID: 1\n"
        "Accept: application/json\n\n"
    )

    with pytest.raises(HTTPException) as exc_info:
        parse_http_part(raw_part)

    assert exc_info.value.status_code == 400
    assert exc_info.value.detail == "INVALID_BATCH_REQUEST_LINE"


def test_parse_http_part_accepts_lowercase_http_method():
    raw_part = (
        "Content-Type: application/http\n"
        "Content-Transfer-Encoding: binary\n\n"
        "get ChecklistSet?$top=1 HTTP/1.1\n"
        "Accept: application/json\n\n"
    )

    operation = parse_http_part(raw_part)

    assert operation.method == "GET"
    assert operation.path == "ChecklistSet?$top=1"


def test_parse_http_part_accepts_request_line_without_http_version():
    raw_part = (
        "Content-Type: application/http\n"
        "Content-Transfer-Encoding: binary\n\n"
        "GET ChecklistSet?$top=1\n"
        "Accept: application/json\n\n"
    )

    operation = parse_http_part(raw_part)

    assert operation.method == "GET"
    assert operation.path == "ChecklistSet?$top=1"


def test_extract_boundary_handles_quoted_semicolon_value():
    content_type = 'multipart/mixed; boundary="batch_intent_boundary;v=1"; charset=utf-8'

    assert extract_boundary(content_type) == "batch_intent_boundary;v=1"


def test_parse_http_part_accepts_absolute_url_and_bom():
    raw_part = (
        "Content-Type: application/http\n"
        "Content-Transfer-Encoding: binary\n\n"
        "\ufeffGET http://localhost:5000" + SERVICE_ROOT + "/ChecklistSearchSet?$top=1 HTTP/1.1\n"
        "Accept: application/json\n\n"
    )

    operation = parse_http_part(raw_part)

    assert operation.method == "GET"
    assert operation.path == "http://localhost:5000" + SERVICE_ROOT + "/ChecklistSearchSet?$top=1"


def test_parse_batch_request_accepts_http_2_request_line():
    boundary = "batch_intent_boundary"
    body = (
        "--batch_intent_boundary\n"
        "Content-Type: application/http\n"
        "Content-Transfer-Encoding: binary\n\n"
        "GET " + SERVICE_ROOT + "/ChecklistSearchSet?$top=1 HTTP/2\n"
        "Accept: application/json\n\n"
        "--batch_intent_boundary--\n"
    )

    parsed = parse_batch_request(body, boundary)

    assert len(parsed) == 1
    assert parsed[0].method == "GET"
