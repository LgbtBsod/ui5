from fastapi import APIRouter, Response

from services.metadata_cache import get_metadata, metadata_refreshed_at_iso

router = APIRouter(tags=["Metadata"])


@router.get("/$metadata")
def metadata():
    oResponse = Response(content=get_metadata(), media_type="application/xml; charset=utf-8")
    sRefreshedAt = metadata_refreshed_at_iso()
    if sRefreshedAt:
        oResponse.headers["X-Metadata-Refreshed-At"] = sRefreshedAt
    return oResponse
