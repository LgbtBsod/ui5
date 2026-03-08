import json
import asyncio
import logging
import random
import uuid
from datetime import datetime, timedelta, timezone
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException, Request, Response
from fastapi.responses import JSONResponse
from fastapi.middleware.cors import CORSMiddleware
from sqlalchemy import inspect, text

from api.analytics_api import router as analytics_router
from api.batch_api import router as batch_router
from api.gateway_canonical_api import router as gateway_canonical_router
from config import CORS_ALLOWED_ORIGINS, LOCK_CLEANUP_INTERVAL_SECONDS, METADATA_REFRESH_INTERVAL_SECONDS
from database import Base, SessionLocal, engine
from services.db_seed import seed_reference_data
from services.analytics_service import AnalyticsService
from services.lock_service import LockService
from services.metadata_cache import refresh_metadata
from services.settings_service import DEFAULT_REQUIRED_FIELDS, DEFAULT_UPLOAD_POLICY
from models import ChecklistRoot, FrontendRuntimeSettings
from utils.odata import SERVICE_ROOT, odata_error_response
from utils.odata_batch import extract_boundary, parse_batch_request
from utils.odata_csrf import CsrfStore
from utils.sap_message import build_sap_message

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("gateway")

_LOG_BODY_METHODS = {"POST", "PUT", "PATCH", "MERGE", "DELETE"}
_LOG_BODY_CONTENT_TYPES = (
    "application/json",
    "application/xml",
    "text/plain",
    "multipart/mixed",
)
_LOG_BODY_MAX_CHARS = 4000


def _split_full_name(s_full_name: str) -> tuple[str, str, str]:
    parts = [part for part in str(s_full_name or "").strip().split(" ") if part]
    if not parts:
        return "", "", ""
    if len(parts) == 1:
        return parts[0], "", ""
    if len(parts) == 2:
        return parts[1], parts[0], ""
    return parts[1], parts[0], " ".join(parts[2:])


def _trim_for_log(s_value: str, i_limit: int = _LOG_BODY_MAX_CHARS) -> str:
    text = str(s_value or "")
    if len(text) <= i_limit:
        return text
    return f"{text[:i_limit]}... <trimmed {len(text) - i_limit} chars>"


def _format_batch_operations_for_log(s_body: str, s_content_type: str) -> list[str]:
    try:
        s_boundary = extract_boundary(s_content_type)
        a_operations = parse_batch_request(s_body, s_boundary)
    except Exception as exc:  # noqa: BLE001
        return [f"batch_parse_error={type(exc).__name__}"]

    a_lines: list[str] = []
    i_index = 1
    for o_item in a_operations:
        if isinstance(o_item, list):
            for o_operation in o_item:
                a_lines.append(f"[{i_index}] {o_operation.method} {o_operation.path}")
                i_index += 1
            continue
        a_lines.append(f"[{i_index}] {o_item.method} {o_item.path}")
        i_index += 1
    return a_lines




def _resolve_effective_method(request: Request) -> str:
    s_method = request.method.upper()
    if s_method != "POST":
        return s_method
    s_override = str(
        request.headers.get("X-HTTP-Method")
        or request.headers.get("X-HTTP-Method-Override")
        or ""
    ).strip().upper()
    if s_override in {"PUT", "PATCH", "MERGE", "DELETE"}:
        return s_override
    return s_method

def _build_request_log_lines(request: Request, b_content_forced: bool, s_body_text: str | None, s_effective_method: str) -> list[str]:
    s_content_type = str(request.headers.get("content-type") or "").lower()
    s_method_suffix = "" if s_effective_method == request.method.upper() else f" (effective={s_effective_method})"
    a_lines = [f"REQ {request.method}{s_method_suffix} {request.url.path} query={request.url.query or '-'}"]

    if b_content_forced:
        if "multipart/mixed" in s_content_type:
            a_batch_ops = _format_batch_operations_for_log(s_body_text or "", s_content_type)
            a_lines.append("REQ batch operations:")
            a_lines.extend(f"  - {s_line}" for s_line in a_batch_ops)
        else:
            a_lines.append(f"REQ body={_trim_for_log(s_body_text or '')}")
    return a_lines




def ensure_schema_compatibility() -> None:
    inspector = inspect(engine)
    if "checklist_root" in inspector.get_table_names():
        existing = {col["name"] for col in inspector.get_columns("checklist_root")}
        required = [
            "integration_flag", "date", "time_check", "time_zone", "equipment", "bukrs", "lpc_text",
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
                elif col == "integration_flag":
                    conn.execute(text("ALTER TABLE checklist_root ADD COLUMN integration_flag BOOLEAN DEFAULT 0"))
                else:
                    conn.execute(text(f"ALTER TABLE checklist_root ADD COLUMN {col} VARCHAR"))
                logger.info("Added missing checklist_root column: %s", col)

    if "frontend_runtime_settings" in inspector.get_table_names():
        settings_existing = {col["name"] for col in inspector.get_columns("frontend_runtime_settings")}
        settings_required = {
            "required_fields_json": "TEXT",
            "upload_policy_json": "TEXT",
        }
        with engine.begin() as conn:
            for col, sql_type in settings_required.items():
                if col in settings_existing:
                    continue
                conn.execute(text(f"ALTER TABLE frontend_runtime_settings ADD COLUMN {col} {sql_type}"))
                logger.info("Added missing frontend_runtime_settings column: %s", col)

    checklist_child_columns = {
        "checklist_check": {"comment": "TEXT"},
        "checklist_barrier": {"comment": "TEXT"},
    }
    with engine.begin() as conn:
        for table_name, required_columns in checklist_child_columns.items():
            if table_name not in inspector.get_table_names():
                continue
            existing = {col["name"] for col in inspector.get_columns(table_name)}
            for col, sql_type in required_columns.items():
                if col in existing:
                    continue
                conn.execute(text(f"ALTER TABLE {table_name} ADD COLUMN {col} {sql_type}"))
                logger.info("Added missing %s column: %s", table_name, col)

    if "analytics_snapshot" in inspector.get_table_names():
        analytics_existing = {col["name"] for col in inspector.get_columns("analytics_snapshot")}
        analytics_required = {
            "selected_year": "INTEGER DEFAULT 0",
            "source_key": "VARCHAR DEFAULT 'ALL'",
            "available_years_json": "TEXT DEFAULT '[]'",
            "closed_count": "INTEGER DEFAULT 0",
            "registered_count": "INTEGER DEFAULT 0",
            "failed_checklist_count": "INTEGER DEFAULT 0",
            "failed_barrier_checklist_count": "INTEGER DEFAULT 0",
            "avg_checks_rate": "REAL DEFAULT 0",
            "avg_barriers_rate": "REAL DEFAULT 0",
            "healthy_count": "INTEGER DEFAULT 0",
            "source": "VARCHAR DEFAULT 'backend'",
            "refreshed_at": "DATETIME",
        }
        with engine.begin() as conn:
            for col, sql_type in analytics_required.items():
                if col in analytics_existing:
                    continue
                conn.execute(text(f"ALTER TABLE analytics_snapshot ADD COLUMN {col} {sql_type}"))
                logger.info("Added missing analytics_snapshot column: %s", col)

    if "analytics_breakdown" in inspector.get_table_names():
        breakdown_existing = {col["name"] for col in inspector.get_columns("analytics_breakdown")}
        breakdown_required = {
            "selected_year": "INTEGER DEFAULT 0",
            "source_key": "VARCHAR DEFAULT 'ALL'",
        }
        with engine.begin() as conn:
            for col, sql_type in breakdown_required.items():
                if col in breakdown_existing:
                    continue
                conn.execute(text(f"ALTER TABLE analytics_breakdown ADD COLUMN {col} {sql_type}"))
                logger.info("Added missing analytics_breakdown column: %s", col)


def _seed_checklist_roots_if_needed(db, minimum_rows: int = 100) -> int:
    current_count = db.query(ChecklistRoot).filter((ChecklistRoot.is_deleted.is_(None)) | (ChecklistRoot.is_deleted.is_(False))).count()
    if current_count >= minimum_rows:
        return 0

    lpc_pool = ["LPC-01", "LPC-02", "LPC-03", "LPC-04", "LPC-05"]
    status_pool = ["DRAFT", "REGISTERED", "CLOSED"]
    equipment_pool = ["Pump", "Compressor", "Conveyor", "Boiler", "Generator"]
    bukrs_pool = ["1000", "2000", "3000"]
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
                integration_flag=(sequence % 4 == 0),
                date=changed_on.date().isoformat(),
                time_check=changed_on.strftime("%H:%M"),
                time_zone="Europe/Saratov",
                equipment=random.choice(equipment_pool),
                bukrs=random.choice(bukrs_pool),
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


def _ensure_integration_samples(db, minimum_ratio: float = 0.18) -> int:
    roots = db.query(ChecklistRoot).filter((ChecklistRoot.is_deleted.is_(None)) | (ChecklistRoot.is_deleted.is_(False))).order_by(ChecklistRoot.created_on.asc()).all()
    total = len(roots)
    if total == 0:
        return 0
    current_integration = [root for root in roots if bool(root.integration_flag)]
    target_count = max(1, int(total * float(minimum_ratio or 0)))
    if len(current_integration) >= target_count:
        return 0

    updated = 0
    for root in roots:
        if bool(root.integration_flag):
            continue
        root.integration_flag = True
        root.bukrs = ""
        root.location_key = ""
        root.location_name = ""
        root.location_text = ""
        root.observer_fullname = ""
        root.observer_perner = ""
        root.observer_position = ""
        root.observer_orgunit = ""
        updated += 1
        if len(current_integration) + updated >= target_count:
            break

    if updated:
        db.commit()
        logger.info("Backfilled integration sample checklist roots: updated=%s target=%s total=%s", updated, target_count, total)
    return updated


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


async def metadata_refresh_job() -> None:
    while True:
        try:
            refresh_metadata()
            logger.info("Service metadata cache refreshed")
        except Exception:
            logger.exception("Failed to refresh service metadata cache")
        await asyncio.sleep(METADATA_REFRESH_INTERVAL_SECONDS)


async def analytics_refresh_job() -> None:
    while True:
        db = SessionLocal()
        try:
            settings_row = db.query(FrontendRuntimeSettings).first()
            interval_ms = int(getattr(settings_row, "analytics_refresh_ms", 300000) or 300000)
            if AnalyticsService.should_refresh(db, max(5, int(interval_ms / 1000))):
                AnalyticsService.refresh_cache(db, trigger="scheduler")
            sleep_seconds = 5 if AnalyticsService._dirty else max(5, min(60, int(interval_ms / 1000)))
        except Exception:
            logger.exception("Failed to refresh analytics cache")
            sleep_seconds = 10
        finally:
            db.close()
        await asyncio.sleep(sleep_seconds)


@asynccontextmanager
async def lifespan(_: FastAPI):
    Base.metadata.create_all(bind=engine)
    ensure_schema_compatibility()

    db = SessionLocal()
    try:
        seed_reference_data(db)
        _seed_checklist_roots_if_needed(db, minimum_rows=100)
        _ensure_integration_samples(db)
        oSettings = db.query(FrontendRuntimeSettings).first()
        if not oSettings:
            db.add(FrontendRuntimeSettings(
                environment="default",
                autosave_debounce_ms=1200,
                required_fields_json=json.dumps(DEFAULT_REQUIRED_FIELDS),
                upload_policy_json=json.dumps(DEFAULT_UPLOAD_POLICY)
            ))
            db.commit()
        else:
            bChanged = False
            if not str(getattr(oSettings, "required_fields_json", "") or "").strip():
                oSettings.required_fields_json = json.dumps(DEFAULT_REQUIRED_FIELDS)
                bChanged = True
            if not str(getattr(oSettings, "upload_policy_json", "") or "").strip():
                oSettings.upload_policy_json = json.dumps(DEFAULT_UPLOAD_POLICY)
                bChanged = True
            if bChanged:
                db.commit()
        AnalyticsService.refresh_cache(db, trigger="startup")
    finally:
        db.close()

    refresh_metadata()

    cleanup_task = asyncio.create_task(lock_cleanup_job())
    metadata_task = asyncio.create_task(metadata_refresh_job())
    analytics_task = asyncio.create_task(analytics_refresh_job())
    yield
    cleanup_task.cancel()
    metadata_task.cancel()
    analytics_task.cancel()


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

    b_is_batch = path.endswith("/$batch")
    if request.method in {"POST", "PUT", "PATCH", "MERGE", "DELETE"} and not is_fetch and not b_is_batch:
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
async def odata_response_headers_middleware(request: Request, call_next):
    response = await call_next(request)
    if request.url.path.startswith(SERVICE_ROOT):
        # Canonical SAP Gateway OData v2 headers.
        if "DataServiceVersion" not in response.headers:
            response.headers["DataServiceVersion"] = "2.0"
        if "MaxDataServiceVersion" not in response.headers:
            response.headers["MaxDataServiceVersion"] = "2.0"
        ct = response.headers.get("content-type", "")
        if ct and "application/json" in ct and "charset" not in ct:
            response.headers["content-type"] = ct.split(";")[0] + "; charset=utf-8"
        if "sap-server" not in response.headers:
            response.headers["sap-server"] = "mock/1.0"
        if "sap-message" not in response.headers and hasattr(response, "body") and response.status_code < 400:
            try:
                payload = json.loads((response.body or b"{}").decode("utf-8"))
            except Exception:
                payload = {}
            data = payload.get("d") if isinstance(payload, dict) else None
            if isinstance(data, dict) and data.get("Message"):
                response.headers["sap-message"] = build_sap_message(data.get("Message"), "success", code=str(data.get("ReasonCode") or ""))
    return response

@app.middleware("http")
async def request_response_logging(request: Request, call_next):
    s_effective_method = _resolve_effective_method(request)
    if s_effective_method != request.method.upper():
        request.scope["method"] = s_effective_method

    s_content_type = str(request.headers.get("content-type") or "").lower()
    b_method_with_body = s_effective_method in _LOG_BODY_METHODS
    b_supported_content_type = any(s_item in s_content_type for s_item in _LOG_BODY_CONTENT_TYPES)
    b_read_body = b_method_with_body and b_supported_content_type

    s_body_text: str | None = None
    if b_read_body:
        body_bytes = await request.body()
        s_body_text = body_bytes.decode("utf-8", errors="replace")

    for s_line in _build_request_log_lines(request, b_read_body, s_body_text, s_effective_method):
        logger.info(s_line)

    response = await call_next(request)
    logger.info("RES %s %s status=%s", request.method, request.url.path, response.status_code)
    return response


@app.middleware("http")
async def odata_error_envelope_middleware(request: Request, call_next):
    path = request.url.path
    is_odata = path.startswith(SERVICE_ROOT)
    if not is_odata:
        return await call_next(request)
    def _map_status_to_code(i_status: int) -> str:
        if i_status == 404:
            return "NOT_FOUND"
        if i_status == 400:
            return "VALIDATION_ERROR"
        if i_status == 403:
            return "CSRF_TOKEN_MISSING"
        if i_status == 409:
            return "CONFLICT"
        if i_status == 410:
            return "LOCK_EXPIRED"
        if i_status == 412:
            return "PRECONDITION_FAILED"
        if i_status == 422:
            return "VALIDATION_ERROR"
        if i_status >= 500:
            return "SYSTEM_ERROR"
        return "SYSTEM_ERROR"

    try:
        response = await call_next(request)
        if response.status_code >= 400:
            payload = None
            if hasattr(response, "body") and response.body:
                try:
                    payload = json.loads(response.body.decode("utf-8"))
                except Exception:
                    payload = None
            if not (isinstance(payload, dict) and isinstance(payload.get("error"), dict)):
                s_message = "Request failed"
                if isinstance(payload, dict) and payload.get("detail"):
                    s_message = str(payload.get("detail"))
                return odata_error_response(response.status_code, _map_status_to_code(response.status_code), s_message)
        return response
    except HTTPException as exc:
        if isinstance(exc.detail, dict) and exc.detail.get("error"):
            return JSONResponse(status_code=exc.status_code, content=exc.detail)

        code = _map_status_to_code(exc.status_code)
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

app.include_router(gateway_canonical_router)
app.include_router(analytics_router)
app.include_router(batch_router)
app.include_router(analytics_router, prefix=SERVICE_ROOT)
app.include_router(batch_router, prefix=SERVICE_ROOT)


@app.get("/")
def health():
    return {"status": "Gateway Simulator Running"}

@app.get("/sap/bc/lrep/flex/data/checklist.app.Component")
def ui5_flex_stub(appVersion: str | None = None):
    return {"changes": [], "comp": {"name": "checklist.app.Component", "appVersion": appVersion or "1.0.0"}}


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
    return Response(content="/* preload not bundled in mock mode */", media_type="application/javascript")


@app.exception_handler(Exception)
async def odata_exception_handler(request: Request, exc: Exception):
    if request.url.path.startswith(SERVICE_ROOT):
        return odata_error_response(500, "SYSTEM_ERROR", str(exc))
    return JSONResponse(status_code=500, content={"detail": str(exc)})
