import asyncio
import logging
import random
import uuid
from datetime import datetime, timedelta, timezone
from contextlib import asynccontextmanager
from pathlib import Path

from fastapi import FastAPI, HTTPException, Request, Response
from fastapi.responses import JSONResponse
from fastapi.middleware.cors import CORSMiddleware
from sqlalchemy import inspect, text

from api.actions_api import router as actions_router
from api.analytics_api import router as analytics_router
from api.batch_api import router as batch_router
from api.capabilities_api import router as capabilities_router
from api.checklist_api import router as checklist_router
from api.dictionary_api import legacy_router as dictionary_legacy_router
from api.dictionary_api import router as dictionary_router
from api.hierarchy_api import router as hierarchy_router
from api.gateway_canonical_api import router as gateway_canonical_router
from api.location_api import router as location_router
from api.lock_api import router as lock_router
from api.lock_entity_api import router as lock_entity_router
from api.lock_history_api import router as lock_history_router
from api.metadata_api import router as metadata_router
from api.odata_compat_api import router as odata_compat_router
from api.person_api import router as person_router
from api.reference_api import router as reference_router
from api.settings_api import router as settings_router
from api.search_api import router as search_router
from config import CORS_ALLOWED_ORIGINS, LOCK_CLEANUP_INTERVAL_SECONDS
from database import Base, SessionLocal, engine
from services.dict_loader import load_dictionary
from services.lock_service import LockService
from models import ChecklistRoot, FrontendRuntimeSettings, DictionaryItem
from utils.odata import SERVICE_ROOT, odata_error_response
from utils.odata_csrf import CsrfStore

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("gateway")




def ensure_schema_compatibility() -> None:
    inspector = inspect(engine)
    if "checklist_root" not in inspector.get_table_names():
        return

    existing = {col["name"] for col in inspector.get_columns("checklist_root")}
    required = [
        "date", "equipment", "lpc_text",
        "observer_fullname", "observer_perner", "observer_position", "observer_orgunit", "observer_integration_name",
        "observed_fullname", "observed_perner", "observed_position", "observed_orgunit", "observed_integration_name",
        "location_key", "location_name", "location_text", "version_number",
    ]

    with engine.begin() as conn:
        for col in required:
            if col in existing:
                continue
            if col == "version_number":
                conn.execute(text("ALTER TABLE checklist_root ADD COLUMN version_number INTEGER DEFAULT 1"))
            else:
                conn.execute(text(f"ALTER TABLE checklist_root ADD COLUMN {col} VARCHAR"))
            logger.info("Added missing checklist_root column: %s", col)


def _seed_checklist_roots_if_needed(db, minimum_rows: int = 100) -> int:
    current_count = db.query(ChecklistRoot).filter((ChecklistRoot.is_deleted.is_(None)) | (ChecklistRoot.is_deleted.is_(False))).count()
    if current_count >= minimum_rows:
        return 0

    lpc_pool = ["LPC-01", "LPC-02", "LPC-03", "LPC-04", "LPC-05"]
    status_pool = ["DRAFT", "SUBMITTED", "DONE", "REJECTED"]
    equipment_pool = ["Pump", "Compressor", "Conveyor", "Boiler", "Generator"]
    observer_pool = [
        "Ivan Ivanov",
        "Anna Petrova",
        "Sergey Smirnov",
        "Olga Sokolova",
        "Petr Kuznetsov",
    ]

    to_create = minimum_rows - current_count
    now = datetime.now(timezone.utc)
    for index in range(to_create):
        sequence = current_count + index + 1
        changed_on = now - timedelta(minutes=index)
        db.add(
            ChecklistRoot(
                id=str(uuid.uuid4()),
                checklist_id=f"CHK-{sequence:05d}",
                lpc=random.choice(lpc_pool),
                status=random.choice(status_pool),
                date=changed_on.date().isoformat(),
                equipment=random.choice(equipment_pool),
                observer_fullname=random.choice(observer_pool),
                changed_on=changed_on,
                changed_by="SYSTEM",
                version_number=1,
                created_on=changed_on,
                created_by="SYSTEM",
                is_deleted=False,
            )
        )

    db.commit()
    logger.info("Seeded checklist roots for search compatibility: added=%s total=%s", to_create, minimum_rows)
    return to_create


def _seed_static_domains(db) -> None:
    from datetime import date
    import uuid

    defaults = {
        "STATUS": [("DRAFT", "Draft"), ("SUBMITTED", "Submitted"), ("DONE", "Done"), ("REJECTED", "Rejected")],
        "PROFESSION": [("OP", "Operator"), ("SUP", "Supervisor")],
        "LPC": [("LPC-01", "LPC 01"), ("LPC-02", "LPC 02")],
        "ATF_CAT": [("GEN", "General"), ("PHOTO", "Photo"), ("DOC", "Document")],
        "TIME_ZONE": [("UTC", "UTC"), ("Europe/Amsterdam", "Europe/Amsterdam")],
    }

    for domain, entries in defaults.items():
        for key, text in entries:
            exists = db.query(DictionaryItem).filter(DictionaryItem.domain == domain, DictionaryItem.key == key).first()
            if exists:
                continue
            db.add(DictionaryItem(
                id=str(uuid.uuid4()),
                domain=domain,
                key=key,
                text=text,
                begda=date(2000, 1, 1),
                endda=None,
            ))
    db.commit()


async def lock_cleanup_job() -> None:
    while True:
        db = SessionLocal()
        try:
            cleaned = LockService.cleanup(db)
            if cleaned:
                logger.info("Cleaned %s expired locks", cleaned)
        finally:
            db.close()
        await asyncio.sleep(LOCK_CLEANUP_INTERVAL_SECONDS)


@asynccontextmanager
async def lifespan(_: FastAPI):
    Base.metadata.create_all(bind=engine)
    ensure_schema_compatibility()

    db = SessionLocal()
    try:
        data_dir = Path(__file__).resolve().parent / "data"
        load_dictionary(db, str(data_dir / "lpc.json"), "LPC")
        load_dictionary(db, str(data_dir / "professions.json"), "PROFESSION")
        _seed_static_domains(db)
        _seed_checklist_roots_if_needed(db, minimum_rows=100)
        oSettings = db.query(FrontendRuntimeSettings).first()
        if not oSettings:
            db.add(FrontendRuntimeSettings(environment="default", autosave_debounce_ms=1200))
            db.commit()
    finally:
        db.close()

    task = asyncio.create_task(lock_cleanup_job())
    yield
    task.cancel()


app = FastAPI(title="SAP Gateway Simulator", version="1.0.0", lifespan=lifespan)



app.state.csrf_store = CsrfStore()


@app.middleware("http")
async def odata_csrf_middleware(request: Request, call_next):
    path = request.url.path
    is_odata = path.startswith(SERVICE_ROOT)
    if not is_odata:
        return await call_next(request)

    session_id = request.cookies.get("SAP_SESSIONID")
    token = request.headers.get("X-CSRF-Token")
    is_fetch = str(token).lower() == "fetch"

    if request.method in {"POST", "PUT", "PATCH", "MERGE", "DELETE"} and not is_fetch:
        if not app.state.csrf_store.validate(session_id, token):
            return odata_error_response(403, "CSRF_TOKEN_MISSING", "CSRF token validation failed")

    response = await call_next(request)

    if is_fetch:
        import uuid

        sid, new_token = app.state.csrf_store.issue(session_id)
        response.headers["X-CSRF-Token"] = new_token
        response.set_cookie("SAP_SESSIONID", sid, httponly=False, samesite="lax")
    return response


@app.middleware("http")
async def request_response_logging(request: Request, call_next):
    logger.info("REQ %s %s query=%s", request.method, request.url.path, request.url.query or "-")
    response = await call_next(request)
    logger.info("RES %s %s status=%s", request.method, request.url.path, response.status_code)
    return response


@app.middleware("http")
async def odata_error_envelope_middleware(request: Request, call_next):
    path = request.url.path
    is_odata = path.startswith(SERVICE_ROOT)
    if not is_odata:
        return await call_next(request)
    try:
        return await call_next(request)
    except HTTPException as exc:
        if isinstance(exc.detail, dict) and exc.detail.get("error"):
            return JSONResponse(status_code=exc.status_code, content=exc.detail)

        code = "SYSTEM_ERROR"
        if exc.status_code == 404:
            code = "NOT_FOUND"
        elif exc.status_code == 400:
            code = "VALIDATION_ERROR"
        elif exc.status_code == 403:
            code = "CSRF_TOKEN_MISSING"
        elif exc.status_code == 409:
            code = "CONFLICT"
        elif exc.status_code == 410:
            code = "LOCK_EXPIRED"
        elif exc.status_code == 412:
            code = "PRECONDITION_FAILED"
        return odata_error_response(exc.status_code, code, str(exc.detail or code))
    except Exception:
        logger.exception("Unhandled OData exception")
        return odata_error_response(500, "SYSTEM_ERROR", "Internal server error")


app.add_middleware(
    CORSMiddleware,
    allow_origins=CORS_ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

app.include_router(lock_router)
app.include_router(lock_entity_router)
app.include_router(lock_history_router)
app.include_router(checklist_router)
app.include_router(search_router)
app.include_router(dictionary_router)
app.include_router(dictionary_legacy_router)
app.include_router(location_router)
app.include_router(person_router)
app.include_router(reference_router)
app.include_router(hierarchy_router)
app.include_router(actions_router)
app.include_router(metadata_router)
# Register canonical Gateway routes before compat shim routes so canonical service-root paths win.
app.include_router(gateway_canonical_router)
app.include_router(odata_compat_router)
app.include_router(analytics_router)
app.include_router(settings_router)
app.include_router(batch_router)
app.include_router(capabilities_router)

# SAP Gateway-style alias paths under service root used by the UI runtime.
app.include_router(lock_router, prefix=SERVICE_ROOT)
app.include_router(lock_entity_router, prefix=SERVICE_ROOT)
app.include_router(lock_history_router, prefix=SERVICE_ROOT)
app.include_router(checklist_router, prefix=SERVICE_ROOT)
app.include_router(search_router, prefix=SERVICE_ROOT)
app.include_router(dictionary_router, prefix=SERVICE_ROOT)
app.include_router(dictionary_legacy_router, prefix=SERVICE_ROOT)
app.include_router(location_router, prefix=SERVICE_ROOT)
app.include_router(person_router, prefix=SERVICE_ROOT)
app.include_router(reference_router, prefix=SERVICE_ROOT)
app.include_router(hierarchy_router, prefix=SERVICE_ROOT)
app.include_router(actions_router, prefix=SERVICE_ROOT)
app.include_router(metadata_router, prefix=SERVICE_ROOT)
app.include_router(analytics_router, prefix=SERVICE_ROOT)
app.include_router(settings_router, prefix=SERVICE_ROOT)
app.include_router(batch_router, prefix=SERVICE_ROOT)
app.include_router(capabilities_router, prefix=SERVICE_ROOT)


@app.get("/")
def health():
    return {"status": "Gateway Simulator Running"}


@app.get("/config/frontend")
def frontend_config(db = SessionLocal()):
    try:
        row = db.query(FrontendRuntimeSettings).order_by(FrontendRuntimeSettings.changed_on.desc()).first()
        if not row:
            row = FrontendRuntimeSettings(environment="default", autosave_debounce_ms=1200)
            db.add(row)
            db.commit()
            db.refresh(row)
        m_timers = {
            "heartbeatMs": int(row.heartbeat_ms or 240000),
            "lockStatusMs": int(row.lock_status_ms or 60000),
            "gcdMs": int(row.gcd_ms or 300000),
            "idleMs": int(row.idle_ms or 600000),
            "autoSaveIntervalMs": int(row.autosave_interval_ms or 60000),
            "autoSaveDebounceMs": int(row.autosave_debounce_ms or 1200),
            "networkGraceMs": int(row.network_grace_ms or 60000),
            "cacheFreshMs": int(row.cache_fresh_ms or 30000),
            "cacheStaleOkMs": int(row.cache_stale_ok_ms or 90000),
            "analyticsRefreshMs": int(row.analytics_refresh_ms or 900000),
            "cacheToleranceMs": 5500,
        }
        return {
            "environment": row.environment,
            "search": {"defaultMaxResults": 100, "growingThreshold": 10},
            "timers": m_timers,
            "variables": dict(m_timers, validationSource="config_frontend"),
        }
    finally:
        db.close()


@app.get(f"{SERVICE_ROOT}/config/frontend")
def frontend_config_odata_alias(db=SessionLocal()):
    return frontend_config(db)


@app.get("/sap/bc/lrep/flex/data/sap_ui5.Component")
def ui5_flex_stub(appVersion: str | None = None):
    return {"changes": [], "comp": {"name": "sap_ui5.Component", "appVersion": appVersion or "1.0.0"}}


@app.get("/sap_ui5/Component-preload.js")
def component_preload_stub():
    return Response(content="/* preload not bundled in mock mode */", media_type="application/javascript")


@app.exception_handler(Exception)
async def odata_exception_handler(request: Request, exc: Exception):
    if request.url.path.startswith(SERVICE_ROOT):
        return odata_error_response(500, "SYSTEM_ERROR", str(exc))
    return JSONResponse(status_code=500, content={"detail": str(exc)})
