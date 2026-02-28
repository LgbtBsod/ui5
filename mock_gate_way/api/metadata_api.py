from fastapi import APIRouter, Response

from services.metadata_builder import build_metadata

router = APIRouter(tags=["Metadata"])


@router.get("/$metadata")
def metadata():
    return Response(content=build_metadata(), media_type="application/xml; charset=utf-8")
