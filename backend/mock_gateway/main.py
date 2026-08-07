import asyncio
import logging
import os
from contextlib import asynccontextmanager

from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse, Response

import bootstrap
from api.analytics_api import router as analytics_router
from api.batch_api import router as batch_router
from api.capabilities_api import router as capabilities_router
from api.gateway_attachment_api import router as gateway_attachment_router
from api.gateway_detail_api import router as gateway_detail_router
from api.gateway_draft_api import router as gateway_draft_router
from api.gateway_lock_api import router as gateway_lock_router
from api.gateway_reference_api import router as gateway_reference_router
from api.gateway_root_api import router as gateway_root_router
from api.gateway_save_api import router as gateway_save_router
from api.gateway_user_api import router as gateway_user_router
from background_jobs import analytics_refresh_job, lock_cleanup_job, metadata_refresh_job
from bootstrap import bootstrap_schema
from bootstrap import seed_checklist_roots_if_needed as _seed_checklist_roots_if_needed
from config import CORS_ALLOWED_ORIGINS, LOG_REQUEST_BODIES
from database import SessionLocal, engine
from middleware import (
    setup_csrf_middleware,
    setup_error_envelope_middleware,
    setup_logging_middleware,
    setup_odata_headers_middleware,
    setup_rate_limit_middleware,
)
from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot
from services.metadata_cache import refresh_metadata
from utils.filter_errors import FilterSyntaxError
from utils.odata import SERVICE_ROOT, odata_error_response
from utils.odata_csrf import CsrfStore

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("gateway")


def ensure_schema_compatibility() -> None:
    """Explicitly ensure required tables/settings row exist, using this module's current
    `engine`/`SessionLocal` globals (tests monkeypatch main.engine/main.SessionLocal to
    point at an isolated database). Passes them through as explicit arguments rather than
    assigning to bootstrap.engine/bootstrap.SessionLocal - mutating those module globals
    would leak the test's throwaway engine into every other caller for the rest of the
    process (bootstrap_schema() included), since monkeypatch only reverts what it directly
    patched on `main`, not side effects this function makes on a third module.

    Deliberately bypasses bootstrap.ensure_schema_compatibility()'s AUTO_MUTATE_SCHEMA_ON_STARTUP
    gate: that flag exists to stop *automatic* mutation at real app startup in a hardened
    non-local profile, not to block an explicit administrative call to this function
    (nothing in the real startup path calls this - only bootstrap_schema() does, via
    bootstrap's own gated internal function).
    """
    bootstrap.ensure_required_tables(bind=engine)
    # Bind explicitly to `engine` (this module's current global, possibly monkeypatched)
    # rather than calling the bare sessionmaker, which would otherwise silently target
    # whatever engine SessionLocal was originally constructed with.
    db = SessionLocal(bind=engine)
    try:
        bootstrap.ensure_runtime_settings_row(db)
    finally:
        db.close()


def _ensure_runtime_settings_row(db):
    return bootstrap.ensure_runtime_settings_row(db)


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Application startup/shutdown lifecycle."""
    logger.info("Starting SAP Gateway Simulator...")
    bootstrap_schema()
    refresh_metadata()

    app.state.background_tasks = [
        asyncio.create_task(lock_cleanup_job()),
        asyncio.create_task(metadata_refresh_job()),
        asyncio.create_task(analytics_refresh_job(bootstrap_schema, _ensure_runtime_settings_row)),
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

app.include_router(gateway_reference_router)
app.include_router(gateway_root_router)
app.include_router(gateway_detail_router)
app.include_router(gateway_user_router)
app.include_router(gateway_lock_router)
app.include_router(gateway_save_router)
app.include_router(gateway_draft_router)
app.include_router(gateway_attachment_router)
app.include_router(analytics_router)
app.include_router(batch_router)
app.include_router(capabilities_router)


@app.get("/")
def health():
    return {"status": "ok", "service": "SAP Gateway Simulator"}


@app.get("/sap/bc/lrep/flex/data/checklist.app.Component")
@app.get("/sap/bc/lrep/flex/data/PRODUCTION_CONTROL_CHECKLIST.Component")
def ui5_flex_stub(appVersion: str | None = None):
    return {
        "changes": [],
        "comp": {"name": "PRODUCTION_CONTROL_CHECKLIST.Component", "appVersion": appVersion or "1.0.0"},
    }


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


@app.exception_handler(FilterSyntaxError)
async def odata_filter_syntax_error_handler(request: Request, exc: FilterSyntaxError):
    """A malformed/unsupported $filter must fail closed (400), not fall through to
    the catch-all handler (500) or, worse, silently match the whole table."""
    if request.url.path.startswith(SERVICE_ROOT):
        return odata_error_response(400, "VALIDATION_ERROR", f"Unsupported $filter expression: {exc}")
    return JSONResponse(status_code=400, content={"detail": str(exc)})


@app.exception_handler(Exception)
async def odata_exception_handler(request: Request, exc: Exception):
    logger.exception("Unhandled error on %s %s", request.method, request.url.path)
    if request.url.path.startswith(SERVICE_ROOT):
        return odata_error_response(500, "SYSTEM_ERROR", "An internal server error occurred")
    return JSONResponse(status_code=500, content={"detail": "An internal server error occurred"})
