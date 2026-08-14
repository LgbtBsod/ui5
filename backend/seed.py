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
    """Создаёт тестовые записи CheckRoot: обычную (TEST-001) и интеграционную
    (INT-001, ThisIsIntegrationData=True) — вторая нужна, чтобы можно было
    сразу проверить в UI поведение read-only-для-интеграции (Updatable,
    IntegrationEditGuard.js, PernrRule.js/CheckCodeCoverage.js) без ручного
    временного переключения флага в этом файле.

    Args:
        port: Server port for display in log message
    """
    if state.store["CheckRoots"]:  # Уже есть данные — не пересоздаём
        return

    _seed_manual_record()
    _seed_integration_record()

    display_port = port if port is not None else config.DEFAULT_PORT
    for root_id, doc_id in (
        ("550e8400-e29b-41d4-a716-446655440999", "TEST-001"),
        ("884e1129-8862-5b31-a71d-0c706049ecd2", "INT-001"),
    ):
        logger.info(
            "[SEED] Created %s: CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')",
            doc_id, root_id, config.ZERO_GUID
        )
        logger.info(
            "[SEED] URL: http://localhost:%s/index.html#/CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')",
            display_port, root_id, config.ZERO_GUID
        )


def _seed_manual_record() -> None:
    """Обычная (не интеграционная) тестовая запись — как и раньше."""
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
        "ObserverPernr": "00000001",  # Иван Петров (Инженер по ОТ)
        "ObservedPernr": "00000002",  # Мария Сидорова (Оператор)
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
        "ObserverPernr": root["ObserverPernr"],
        "ObservedPernr": root["ObservedPernr"],
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


def _seed_integration_record() -> None:
    """[Fix, use-case pass #4] Реальный XML-пример входящего сообщения от
    интеграции (n0:CheckListRequest, urn:nornik.ru:HCM:SCM-CS, предоставлен
    пользователем) заведён как постоянная demo-запись - чтобы read-only-
    поведение для интеграционных данных (Updatable, IntegrationEditGuard.js,
    PernrRule.js/CheckCodeCoverage.js skip, бейдж "Создано интеграцией")
    можно было увидеть в UI сразу, без временного переключения флага в этом
    файле для каждой живой проверки.

    Маппинг полей из XML - какие поля пришли "как есть" (raw/unmatched -
    то, ради чего ObserverIntegrationName/ObservedIntegrationName/RawText
    вообще существуют), а какие интерпретированы как уже нормализованные
    реальным ABAP-обработчиком до попадания в наш стор (см. abap/README.md -
    у полей без fallback-механизма, вроде LpcKey/ProfKey/Timezone, в системе
    просто нет места для "сырого" значения, значит по контракту они должны
    приходить уже сопоставленными):
      - LPC "ПК-2" -> LpcKey "2" (тот же уровень, что и "КПР-2" в справочнике
        PkLevels - иная аббревиатура, не другой код).
      - profession "машинист ПДМ" -> ProfKey "OPERATOR" (ближайшее по смыслу
        значение в справочнике Professions - точного совпадения нет).
      - TMZN_CHECK "RUS03" -> Timezone "Europe/Moscow" (нет точного
        совпадения с TimeZoneCode - интерпретация).
    А что оставлено НЕсопоставленным намеренно (это и есть предмет теста):
      - OBSERVER_NAME/OBSERVED_NAME -> ObserverIntegrationName/
        ObservedIntegrationName, Pernr пустой - ФИО без табельного номера
        (см. XML: только свободный текст, нет ссылки на Person).
      - CHECKS.OBS_TEXT -> CheckItem.RawText, Code = сентинел "000.000" - ни
        один из двух текстов ("Проверка СИЗ"/"Проверка наряда-допуска") не
        совпадает ни с одним CheckTypeCode в справочнике.
      - BARRIERS.BARRIERS_NUMBER ("B-001"/"B-002") -> Barrier.Code как есть,
        БЕЗ фолбэка (см. resolve_barrier - у Barrier, в отличие от CheckItem,
        нет RawText-фолбэка, задокументировано тестом
        test_unknown_code_gives_empty_text_without_raw_text_fallback) - оба
        кода не совпадают ни с одним BarrierTypeCode, так что колонка "Тип
        барьера" у обеих строк будет пустой. Это не баг заведения demo-
        данных, а честное отражение реального пробела: если интеграция
        действительно присылает такие коды, для барьеров, вероятно, нужен
        свой RawText-фолбэк по аналогии с CheckItem - стоит обсудить отдельно.
    """
    now = odata_format.odata_date()
    root_id = "884e1129-8862-5b31-a71d-0c706049ecd2"  # = REQUEST_ID из XML

    root = {
        "RootId": root_id,
        "DocId": "INT-001",
        "Date": "/Date(1784419200000)/",  # 2026-07-19 00:00:00 UTC (DATE_CHECK)
        "Time": "PT10H15M30S",  # 10:15:30 (TIME_CHECK)
        "Timezone": "Europe/Moscow",  # TMZN_CHECK "RUS03" - интерпретация, см. docstring
        "LocationKey": "",  # нет соответствия в LocationHierarchy - см. LocationName ниже
        "LocationName": "Участок №4, горизонт -320м",  # LOCATION как есть
        "LocationText": "Участок №4, горизонт -320м",  # то же самое в staging-поле интеграции
        "Equipment": "ПДМ Caterpillar R1700",  # EQUIPMENT
        "ObserverPernr": "",
        "ObserverIntegrationName": "Иванов Петр Алексеевич",  # OBSERVER_NAME, без Pernr
        "ObservedPernr": "",
        "ObservedIntegrationName": "Сидоров Алексей",  # OBSERVED_NAME, без Pernr
        "LpcKey": "2",  # LPC "ПК-2" - интерпретация, см. docstring
        "ProfKey": "OPERATOR",  # profession "машинист ПДМ" - интерпретация, см. docstring
        "Status": "OK",
        "ThisIsIntegrationData": True,
        "CreatedBy": "INTEGRATION",
        "CreatedAt": now,
        "LastChangedAt": now,
    }
    state.store["CheckRoots"][root_id] = root

    basic = {
        "RootId": root_id,
        "Date": root["Date"],
        "Time": root["Time"],
        "Timezone": root["Timezone"],
        "Equipment": root["Equipment"],
        "LocationKey": root["LocationKey"],
        "LocationName": root["LocationName"],
        "LocationText": root["LocationText"],
        "ObserverPernr": root["ObserverPernr"],
        "ObserverIntegrationName": root["ObserverIntegrationName"],
        "ObservedPernr": root["ObservedPernr"],
        "ObservedIntegrationName": root["ObservedIntegrationName"],
        "LpcKey": root["LpcKey"],
        "ProfKey": root["ProfKey"],
        "LastChangedAt": now,
    }
    state.store["CheckBasics"][root_id] = basic

    # CHECKS - оба OBS_TEXT не совпадают ни с одним CheckTypeCode, поэтому
    # оба попадают в RawText-фолбэк (см. resolve_check_item).
    state.store["CheckItems"][(root_id, "int-item-001")] = {
        "RootId": root_id,
        "ItemId": "int-item-001",
        "Code": "000.000",
        "RawText": "Проверка СИЗ",
        "Comment": "Отсутствуют защитные очки",
        "Result": serve_config.RESULT_CODE_UNSATISFACTORY,
        "LastChangedAt": now,
    }
    state.store["CheckItems"][(root_id, "int-item-002")] = {
        "RootId": root_id,
        "ItemId": "int-item-002",
        "Code": "000.000",
        "RawText": "Проверка наряда-допуска",
        "Comment": "Наряд-допуск оформлен корректно",
        "Result": serve_config.RESULT_CODE_SATISFACTORY,
        "LastChangedAt": now,
    }

    # BARRIERS - "B-001"/"B-002" не совпадают ни с одним BarrierTypeCode;
    # Barrier не имеет RawText-фолбэка (осознанное отличие от CheckItem,
    # см. docstring) - колонка "Тип барьера" будет пустой для обеих строк.
    state.store["Barriers"][(root_id, "int-barrier-001")] = {
        "RootId": root_id,
        "BarrierId": "int-barrier-001",
        "Code": "B-001",
        "Comment": "Отсутствует заземляющий кабель",
        "Result": serve_config.RESULT_CODE_UNSATISFACTORY,
        "LastChangedAt": now,
    }
    state.store["Barriers"][(root_id, "int-barrier-002")] = {
        "RootId": root_id,
        "BarrierId": "int-barrier-002",
        "Code": "B-002",
        "Comment": "Повреждение ограждения рабочей зоны",
        "Result": serve_config.RESULT_CODE_UNSATISFACTORY,
        "LastChangedAt": now,
    }
