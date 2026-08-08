"""[SEED] Начальные тестовые данные для демо — создаются при старте сервера.
Позволяют сразу проверить Object Page с заполненными базовыми полями
(персона, дата, локация, etc.) без необходимости создавать запись через UI.

Refactored with type hints (PEP 484) and logging instead of print statements.
"""
from __future__ import annotations

import logging

import serve_config

from . import config
from . import state
from . import odata_format

logger = logging.getLogger(__name__)


def seed_test_data(port: int | None = None) -> None:
    """Создаёт тестовую запись CheckRoot со всеми базовыми полями.
    
    Args:
        port: Server port for display in log message
    """
    if state.store["CheckRoots"]:  # Уже есть данные — не пересоздаём
        return

    now = odata_format.odata_date()
    # [Фаза 5] RootId дублируется в ActiveUUID (Edm.Guid) — должен быть
    # каноническим GUID-форматом, не произвольной строкой; DocId ("TEST-001")
    # остаётся отдельным бизнес-дружественным полем.
    root_id = "550e8400-e29b-41d4-a716-446655440999"

    # CheckRoot with basic-proxy fields
    root = {
        "RootId": root_id,
        "DocId": "TEST-001",
        "Date": "/Date(1704067200000)/",  # 2024-01-01 00:00:00 UTC
        "Time": "PT09H30M00S",  # 09:30:00
        "Timezone": "Europe/Moscow",
        "LocationKey": "550e8400-e29b-41d4-a716-446655440111",  # Северный вход
        "Equipment": "Станок №5",
        "ObserverPerner": "00000001",  # Иван Петров (Инженер по ОТ)
        "ObservedPerner": "00000002",  # Мария Сидорова (Оператор)
        "LpcKey": "2",
        "ProfKey": "ELECTRICIAN",
        "Status": "OK",
        "ThisIsIntegrationData": False,
        "CreatedBy": "SEED_USER",
        "CreatedAt": now,
        "LastChangedAt": now,
    }
    state.store["CheckRoots"][root_id] = root

    # CheckBasic (SSOT для basic-proxy полей)
    basic = {
        "RootId": root_id,
        "Date": root["Date"],
        "Time": root["Time"],
        "Timezone": root["Timezone"],
        "Equipment": root["Equipment"],
        "LocationKey": root["LocationKey"],
        "ObserverPerner": root["ObserverPerner"],
        "ObservedPerner": root["ObservedPerner"],
        "LpcKey": root["LpcKey"],
        "ProfKey": root["ProfKey"],
        "LastChangedAt": now,
    }
    state.store["CheckBasics"][root_id] = basic

    # Test CheckItems
    state.store["CheckItems"][(root_id, "seed-item-001")] = {
        "RootId": root_id,
        "ItemId": "seed-item-001",
        "Code": "VISUAL_INSPECTION",
        "RawText": "",
        "Comment": "Визуальный осмотр пройден успешно",
        "Result": serve_config.RESULT_CODE_SATISFACTORY,
        "LastChangedAt": now,
    }
    state.store["CheckItems"][(root_id, "seed-item-002")] = {
        "RootId": root_id,
        "ItemId": "seed-item-002",
        "Code": "DOCUMENT_REVIEW",
        "RawText": "",
        "Comment": "Документы в порядке",
        "Result": serve_config.RESULT_CODE_SATISFACTORY,
        "LastChangedAt": now,
    }

    # Test Barriers
    state.store["Barriers"][(root_id, "seed-barrier-001")] = {
        "RootId": root_id,
        "BarrierId": "seed-barrier-001",
        "Code": "SAFETY_FENCE",
        "Comment": "Ограждение установлено корректно",
        "Result": serve_config.RESULT_CODE_SATISFACTORY,
        "LastChangedAt": now,
    }

    display_port = port if port is not None else config.DEFAULT_PORT
    logger.info(
        "[SEED] Created test record: CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')",
        root_id, config.ZERO_GUID
    )
    logger.info(
        "[SEED] URL: http://localhost:%s/index.html#/CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')",
        display_port, root_id, config.ZERO_GUID
    )
