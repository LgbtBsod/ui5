from __future__ import annotations

from dataclasses import dataclass
from datetime import timedelta

from fastapi import HTTPException

from state import LOCK_TTL_SECONDS, db, iso, now


@dataclass
class LockInfo:
    root_id: str
    lock_id: str
    owner: str
    acquired_on: str
    expires_on: str


def _is_expired(lock: dict) -> bool:
    return lock["expires_at"] <= now()


def _active_lock(root_id: str) -> dict | None:
    lock = db["locks"].get(root_id)
    if not lock:
        return None
    if _is_expired(lock):
        db["locks"].pop(root_id, None)
        return None
    return lock


def acquire_lock(root_id: str, owner: str, allow_steal: bool = False) -> LockInfo:
    lock = _active_lock(root_id)
    if lock:
        if allow_steal:
            db["locks"].pop(root_id, None)
        else:
            raise HTTPException(
                status_code=409,
                detail={
                    "code": "LOCKED",
                    "message": "Root is locked by another session",
                    "lock": {
                        "root_id": root_id,
                        "owner": lock["owner"],
                        "lock_id": lock["lock_id"],
                        "expires_on": iso(lock["expires_at"]),
                    },
                },
            )

    locked_at = now()
    new_lock = {
        "root_id": root_id,
        "lock_id": f"L-{root_id}-{owner}",
        "owner": owner,
        "acquired_at": locked_at,
        "expires_at": locked_at + timedelta(seconds=LOCK_TTL_SECONDS),
    }
    db["locks"][root_id] = new_lock
    return LockInfo(
        root_id=root_id,
        lock_id=new_lock["lock_id"],
        owner=owner,
        acquired_on=iso(new_lock["acquired_at"]),
        expires_on=iso(new_lock["expires_at"]),
    )


def heartbeat(root_id: str, lock_id: str, owner: str) -> LockInfo:
    lock = db["locks"].get(root_id)
    if not lock:
        raise HTTPException(status_code=410, detail={"code": "LOCK_EXPIRED", "message": "Lock already expired or missing"})

    if _is_expired(lock):
        db["locks"].pop(root_id, None)
        raise HTTPException(status_code=410, detail={"code": "LOCK_EXPIRED", "message": "Lock expired"})

    if lock["lock_id"] != lock_id or lock["owner"] != owner:
        raise HTTPException(status_code=409, detail={"code": "LOCKED", "message": "Lock owner mismatch"})

    lock["expires_at"] = now() + timedelta(seconds=LOCK_TTL_SECONDS)
    return LockInfo(
        root_id=root_id,
        lock_id=lock["lock_id"],
        owner=lock["owner"],
        acquired_on=iso(lock["acquired_at"]),
        expires_on=iso(lock["expires_at"]),
    )


def release(root_id: str, lock_id: str, owner: str) -> None:
    lock = db["locks"].get(root_id)
    if not lock:
        return
    if lock["lock_id"] != lock_id or lock["owner"] != owner:
        raise HTTPException(status_code=409, detail={"code": "LOCKED", "message": "Only lock owner can release"})
    db["locks"].pop(root_id, None)


def validate_write_lock(root_id: str, lock_id: str, owner: str) -> dict:
    lock = db["locks"].get(root_id)
    if not lock or _is_expired(lock):
        db["locks"].pop(root_id, None)
        raise HTTPException(status_code=410, detail={"code": "LOCK_EXPIRED", "message": "Write lock expired"})

    if lock["lock_id"] != lock_id or lock["owner"] != owner:
        raise HTTPException(status_code=409, detail={"code": "LOCKED", "message": "Root locked by another user"})

    return lock
