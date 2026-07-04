import asyncio
import logging
import os
from contextlib import asynccontextmanager

from fastapi import FastAPI, Request, Exception as FastAPIException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse, Response

from api.analytics_api import router as analytics_router
from api.batch_api import router as batch_router
from api.gateway_canonical_api import router as gateway_canonical_router
from api.capabilities_api import router as capabilities_router
from bootstrap import bootstrap_schema, ensure_schema_compatibility
from background_jobs import lock_cleanup_job, metadata_refresh_job, analytics_refresh_job
from middleware import setup_csrf_middleware, setup_odata_headers_middleware, setup_logging_middleware, setup_error_envelope_middleware, setup_rate_limit_middleware
from config import CORS_ALLOWED_ORIGINS, LOG_REQUEST_BODIES
from services.metadata_cache import refresh_metadata
from utils.odata import SERVICE_ROOT, odata_error_response
from utils.odata_csrf import CsrfStore

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("gateway")


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Application startup/shutdown lifecycle."""
    logger.info("Starting SAP Gateway Simulator...")
    bootstrap_schema()
    refresh_metadata()

    app.state.background_tasks = [
        asyncio.create_task(lock_cleanup_job()),
        asyncio.create_task(metadata_refresh_job()),
        asyncio.create_task(analytics_refresh_job(bootstrap_schema, ensure_schema_compatibility)),
    ]

    yield

    logger.info("Shutting down...")
    for task in app.state.background_tasks:
        task.cancel()
        try:
            await task
        except asyncio.CancelledError:
            pass


app = FastAPI(
    title="SAP Gateway Simulator",
    version="1.0.0",
    lifespan=lifespan,
)

app.state.csrf_store = CsrfStore()

app.add_middleware(
    CORSMiddleware,
    allow_origins=CORS_ALLOWED_ORIGINS or ["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

asyncio.run(setup_csrf_middleware(app, app.state.csrf_store))
setup_rate_limit_middleware(app)
asyncio.run(setup_odata_headers_middleware(app))
asyncio.run(setup_logging_middleware(app, LOG_REQUEST_BODIES))
asyncio.run(setup_error_envelope_middleware(app))

app.include_router(gateway_canonical_router)
app.include_router(analytics_router)
app.include_router(batch_router)
app.include_router(capabilities_router)


@app.get("/")
def health():
    return {"status": "ok", "service": "SAP Gateway Simulator"}


@app.get("/sap/bc/lrep/flex/data/checklist.app.Component")
@app.get("/sap/bc/lrep/flex/data/PRODUCTION_CONTROL_CHECKLIST.Component")
def ui5_flex_stub(appVersion: str | None = None):
    return {"changes": [], "comp": {"name": "PRODUCTION_CONTROL_CHECKLIST.Component", "appVersion": appVersion or "1.0.0"}}


@app.get("/sap/bc/lrep/flex/settings")
@app.get("/sap/bc/lrep/flex/settings/")
def ui5_flex_settings_stub():
    return {
        "isKeyUser": False,
        "isAtoAvailable": False,
        "isProductiveSystem": False,
    }


@app.get("/checklist/app/Component-preload.js")
def component_preload_stub():
    if os.getenv("MOCK_COMPONENT_PRELOAD_STUB", "").strip().lower() not in {"1", "true", "yes", "on"}:
        return Response(status_code=404)
    return Response(content="/* mock preload stub enabled */", media_type="application/javascript")


@app.exception_handler(Exception)
async def odata_exception_handler(request: Request, exc: Exception):
    if request.url.path.startswith(SERVICE_ROOT):
        return odata_error_response(500, "SYSTEM_ERROR", str(exc))
    return JSONResponse(status_code=500, content={"detail": str(exc)})
