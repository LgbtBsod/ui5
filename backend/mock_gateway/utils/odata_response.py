from __future__ import annotations

from typing import Any

from fastapi import HTTPException

from .odata import build_odata_error_payload, odata_entity, odata_payload


def odata_collection(results: list[dict[str, Any]], inlinecount: int | None = None) -> dict[str, Any]:
    return odata_payload(results=results, inlinecount=inlinecount)


def odata_error(code: str, message: str, status: int, details: list[dict[str, str]] | None = None) -> None:
    raise HTTPException(
        status_code=status,
        detail=build_odata_error_payload(code=code, message=message, details=details),
    )


__all__ = ["odata_entity", "odata_collection", "odata_error"]
