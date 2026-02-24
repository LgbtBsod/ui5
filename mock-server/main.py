from __future__ import annotations

from fastapi import FastAPI, HTTPException, Query
from pydantic import BaseModel, Field

from delta_engine import apply_delta
from key_mapping_engine import apply_mapping, build_key_mapping
from lock_engine import acquire_lock, heartbeat, release, validate_write_lock
from search_engine import query_checklist_flat
from state import db, iso
from version_engine import bump_version, ensure_root, validate_client_version


app = FastAPI(title="Mock SAP Gateway Simulator", version="1.0.0")


class AcquireLockRequest(BaseModel):
    root_id: str
    owner: str
    allow_steal: bool = False


class HeartbeatRequest(BaseModel):
    root_id: str
    lock_id: str
    owner: str


class ReleaseRequest(BaseModel):
    root_id: str
    lock_id: str
    owner: str


class SaveChangesRequest(BaseModel):
    root_id: str
    lock_id: str
    owner: str
    client_version: int = Field(ge=1)
    root_patch: dict = Field(default_factory=dict)


class AutoSaveRequest(BaseModel):
    root_id: str
    lock_id: str
    owner: str
    root_delta: dict = Field(default_factory=dict)
    checks_delta: list[dict] = Field(default_factory=list)


class SaveDraftRequest(BaseModel):
    root_id: str
    lock_id: str
    owner: str
    checks: list[dict] = Field(default_factory=list)
    root_payload: dict = Field(default_factory=dict)


class ErrorFlagsRequest(BaseModel):
    force_500: bool = False


def maybe_force_500() -> None:
    if db["error_flags"].get("force_500"):
        raise HTTPException(status_code=500, detail={"code": "INTERNAL_ERROR", "message": "Forced mock 500"})


@app.get("/health")
def health() -> dict:
    return {"status": "ok", "locks": len(db["locks"]), "roots": len(db["roots"])}


@app.post("/admin/error-flags")
def set_error_flags(payload: ErrorFlagsRequest) -> dict:
    db["error_flags"]["force_500"] = payload.force_500
    return {"error_flags": db["error_flags"]}


@app.post("/lock/acquire")
def lock_acquire(payload: AcquireLockRequest) -> dict:
    maybe_force_500()
    lock = acquire_lock(payload.root_id, payload.owner, allow_steal=payload.allow_steal)
    return {"d": lock.__dict__}


@app.post("/lock/heartbeat")
def lock_heartbeat(payload: HeartbeatRequest) -> dict:
    maybe_force_500()
    lock = heartbeat(payload.root_id, payload.lock_id, payload.owner)
    return {"d": lock.__dict__}


@app.post("/lock/release")
def lock_release(payload: ReleaseRequest) -> dict:
    maybe_force_500()
    release(payload.root_id, payload.lock_id, payload.owner)
    return {"d": {"released": True}}


@app.post("/SaveChanges")
def save_changes(payload: SaveChangesRequest) -> dict:
    maybe_force_500()
    validate_write_lock(payload.root_id, payload.lock_id, payload.owner)
    root = ensure_root(payload.root_id)
    validate_client_version(root, payload.client_version)
    root["data"].update(payload.root_patch)
    version_data = bump_version(root)
    return {"d": {"result": "saved", **version_data}}


@app.post("/AutoSave")
def auto_save(payload: AutoSaveRequest) -> dict:
    maybe_force_500()
    validate_write_lock(payload.root_id, payload.lock_id, payload.owner)
    root = apply_delta(payload.root_id, payload.root_delta, payload.checks_delta)
    return {
        "d": {
            "result": "autosaved",
            "root_id": root["id"],
            "changed_on": iso(root["changed_on"]),
            "version_number": root["version_number"],
        }
    }


@app.post("/SaveDraft")
def save_draft(payload: SaveDraftRequest) -> dict:
    maybe_force_500()
    validate_write_lock(payload.root_id, payload.lock_id, payload.owner)
    key_mapping = build_key_mapping(payload.root_id, payload.checks)
    resolved_root_id, resolved_checks = apply_mapping(payload.root_id, payload.checks, key_mapping)
    root = apply_delta(resolved_root_id, payload.root_payload, resolved_checks)

    return {
        "d": {
            "result": "draft_saved",
            "root_id": resolved_root_id,
            "version_number": root["version_number"],
            "key_mapping": key_mapping,
        }
    }


@app.get("/ChecklistFlatSet")
def checklist_flat_set(
    top: int = Query(20, alias="$top", ge=1, le=500),
    skip: int = Query(0, alias="$skip", ge=0),
    status: str | None = Query(None, alias="status"),
    sort_by: str = Query("changed_on", alias="sort_by"),
    order: str = Query("desc", alias="order"),
) -> dict:
    maybe_force_500()
    return query_checklist_flat(top=top, skip=skip, status=status, sort_by=sort_by, order=order)
