"""SaveService - бизнес-логика операций сохранения Checklist.

Этот модуль инкапсулирует всю логику сохранения, обновления и мутаций
ChecklistRoot, Check и Barrier entities, обеспечивая:
- Валидацию payload
- Применение изменений к root entity
- Применение изменений к detail rows (checks/barriers)
- Обработку attachments
- Проверку блокировок (locks)
- Обновление метаданных (changed_by, changed_on, version_number)

Принципы:
- Single Responsibility: только бизнес-логика сохранения
- Dependency Injection: все зависимости передаются явно
- Type Safety: полные type hints для всех функций
"""

import uuid
from typing import Any

from fastapi import Response
from sqlalchemy.orm import Session

from api.gateway_mutations import (
    _apply_save_detail_rows,
    _apply_save_root,
    _save_request_root,
)
from api.gateway_operations import _hex as _hex_id
from config import LOCK_TTL
from models import ChecklistRoot
from services.analytics_service import AnalyticsService
from services.current_user_service import CurrentUserService
from services.lock_service import LockService
from utils.odata import format_datetime
from utils.sap_message import build_sap_message
from utils.time import now_utc


class SaveResult:
    """Результат операции сохранения.

    Attributes:
        success: Флаг успешного выполнения
        db_key: HEX идентификатор сохранённого root
        changed_on: Timestamp последнего изменения
        version_number: Новая версия записи
        lock_refreshed: Флаг обновления блокировки
        lock_expires_at: Время истечения блокировки
        request_id: Уникальный ID запроса
        error_code: Код ошибки (если есть)
        error_message: Сообщение об ошибке (если есть)
    """

    def __init__(
        self,
        success: bool = True,
        db_key: str = "",
        changed_on: str = "",
        version_number: int = 0,
        lock_refreshed: bool = False,
        lock_expires_at: str = "",
        request_id: str = "",
        server_now: str = "",
        error_code: str | None = None,
        error_message: str | None = None,
    ):
        self.success = success
        self.db_key = db_key
        self.changed_on = changed_on
        self.version_number = version_number
        self.lock_refreshed = lock_refreshed
        self.lock_expires_at = lock_expires_at
        self.request_id = request_id
        self.server_now = server_now
        self.error_code = error_code
        self.error_message = error_message

    def to_dict(self) -> dict[str, Any]:
        """Конвертирует результат в словарь для OData response."""
        return {
            "db_key": self.db_key,
            "changed_on": self.changed_on,
            "version_number": self.version_number,
            "code": "LOCK_OK" if self.success else self.error_code,
            "reason_code": "SAVED" if self.success else self.error_code,
            "lock_refreshed": self.lock_refreshed,
            "lock_expires_at": self.lock_expires_at,
            "server_now": self.server_now,
            "request_id": self.request_id,
        }

    def to_error_dict(self) -> dict[str, Any]:
        """Конвертирует результат в словарь для error response."""
        return {
            "error": {
                "code": self.error_code or "UNKNOWN_ERROR",
                "message": self.error_message or "An unknown error occurred",
            }
        }


class SaveService:
    """Сервис для операций сохранения Checklist.

    Инкапсулирует бизнес-логику AutoSave, SaveChanges и других мутаций.
    Все методы статические для упрощения dependency injection.

    Example:
        result = SaveService.auto_save(
            db=db,
            payload=payload,
            request=request,
            session_guid=session_guid,
        )
        if result.success:
            return odata_entity(result.to_dict())
        else:
            return JSONResponse(result.to_error_dict(), status_code=409)
    """

    @staticmethod
    def validate_lock_or_error(
        db: Session,
        root_id: int,
        session_guid: str,
    ) -> SaveResult | None:
        """Валидирует наличие активной блокировки для сессии.

        Args:
            db: Database session
            root_id: ID checklist root
            session_guid: GUID сессии пользователя

        Returns:
            SaveResult с ошибкой если валидация не прошла, None если OK

        Raises:
            ValueError: Если lock validation failed
        """
        try:
            LockService.validate_session_lock(db, root_id, session_guid)
            return None
        except ValueError as ex:
            error_code = str(ex)
            if error_code in {"LOCK_MISSING", "LOCK_NOT_OWNED_BY_SESSION"}:
                return SaveResult(
                    success=False,
                    error_code="LOCK_CONFLICT",
                    error_message="Active lock for session is required",
                )
            return SaveResult(
                success=False,
                error_code="LOCK_EXPIRED",
                error_message="Lock expired",
            )

    @staticmethod
    def apply_save_and_commit(
        db: Session,
        root: ChecklistRoot,
        payload_body: dict[str, Any],
        check_kind: Any,
        barrier_kind: Any,
        session_guid: str,
    ) -> SaveResult:
        """Применяет изменения к root и detail rows, commit'ит транзакцию.

        Args:
            db: Database session
            root: ChecklistRoot entity
            payload_body: Тело payload с checks и barriers
            check_kind: Kind constant для checks
            barrier_kind: Kind constant для barriers
            session_guid: GUID сессии для обновления lock

        Returns:
            SaveResult с данными о сохранённой записи
        """
        # Применяем изменения к root
        SaveService._apply_root_changes(db, root, payload_body)

        # Применяем изменения к detail rows
        _apply_save_detail_rows(db, root, payload_body.get("checks"), check_kind)
        _apply_save_detail_rows(db, root, payload_body.get("barriers"), barrier_kind)

        # Обновляем lock
        lock_refreshed, lock_expires_at = SaveService._refresh_lock(db, root.id, session_guid)

        # Обновляем метаданные
        root.changed_by = CurrentUserService.resolve_uname(db=db) or root.changed_by or "ANON"
        root.changed_on = now_utc()
        root.version_number = int(root.version_number or 0) + 1

        # Commit
        db.commit()
        AnalyticsService.mark_dirty()

        # Reload для получения актуальных данных
        db.refresh(root)

        return SaveResult(
            success=True,
            db_key=_hex_id(root.id),
            changed_on=format_datetime(root.changed_on),
            version_number=int(root.version_number or 0),
            lock_refreshed=lock_refreshed,
            lock_expires_at=lock_expires_at,
            server_now=format_datetime(now_utc()),
        )

    @staticmethod
    def _apply_root_changes(
        db: Session,
        root: ChecklistRoot,
        payload_body: dict[str, Any],
    ) -> None:
        """Применяет изменения к root entity из payload.

        Args:
            db: Database session
            root: ChecklistRoot entity
            payload_body: Тело payload
        """
        _apply_save_root(root, _save_request_root({"Payload": payload_body}), db)

    @staticmethod
    def _refresh_lock(
        db: Session,
        root_id: int,
        session_guid: str,
    ) -> tuple[bool, str]:
        """Обновляет timestamp и expiry активной блокировки.

        Args:
            db: Database session
            root_id: ID checklist root
            session_guid: GUID сессии

        Returns:
            Tuple[lock_refreshed, lock_expires_at]
        """
        active_lock = LockService.active_lock(db, root_id)
        lock_refreshed_at = now_utc()

        if active_lock and str(active_lock.session_guid or "").strip() == str(session_guid or "").strip():
            active_lock.last_refresh_at = lock_refreshed_at
            active_lock.expires_at = lock_refreshed_at + LOCK_TTL
            return True, format_datetime(LockService.lock_expires_at(active_lock))

        return False, ""

    @staticmethod
    def build_success_response(
        result: SaveResult,
        response: Response,
        message: str = "Operation completed successfully",
        sap_code: str = "SUCCESS",
    ) -> None:
        """Добавляет SAP message headers к response.

        Args:
            result: SaveResult операции
            response: FastAPI Response object
            message: Сообщение для пользователя
            sap_code: Код сообщения SAP
        """
        result.request_id = str(uuid.uuid4())
        response.headers["sap-message"] = build_sap_message(message, "success", code=sap_code)
