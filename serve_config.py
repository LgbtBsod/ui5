#!/usr/bin/env python3
# -*- coding: utf-8 -*-

DEFAULT_PORT = 8000

ODATA_PREFIX = "/sap/opu/odata/sap/ZCHECK_SRV"

CSRF_TOKEN = "mock-csrf-token"

# Единственный источник истины для кода "успешно/удовлетворительно" —
# раньше был захардкожен как голый литерал "X" в REFERENCE_DATA ниже и
# независимо в serve.py (seed-данные + compute_check_root_view), без общей
# Python-константы. Клиентский аналог — Constants.RESULT_CODES.SATISFACTORY
# (ext/util/Constants.js) — синхронизация между JS и Python по-прежнему
# ручная (SSOT только внутри каждого языка по отдельности).
RESULT_CODE_SATISFACTORY = "X"

# [Fix, use-case pass #3] Было "" (пустая строка). Живой баг: framework'овская
# ВСТРОЕННАЯ (не наша) проверка обязательных полей перед Save трактует
# значение SmartField/ComboBox, равное "", как "поле не заполнено" —
# независимо от того, что "" был вполне легитимным выбранным кодом
# ("Неудовлетворительно"), а не признаком "пользователь ничего не выбрал".
# Подтверждено живьём: выбор "Неудовлетворительно" в новом ComboBox-дропдауне
# (см. annotation/check-item.xml, annotation/barrier.xml) стабильно давал
# "Результат: обязательное поле", хотя значение было явно проставлено. Это
# внутренняя логика sap.ui.generic.template/SmartField, annotation-конфигом
# не переопределяется. Заменено на " " (один пробел) — ABAP-конвенция
# abap_false (в отличие от abap_true = "X") для однобайтовых флаг-полей
# (см. abap/README.md, ABAP-поля типа CHAR1 инициализируются пробелом, не
# пустой строкой) — тот же самый признак "не пройдено", но НЕ falsy/пустая
# строка ни в Python (bool(" ") is True), ни в JS ("" !== " ", !" " ===
# false), так что framework-проверка больше не путает его с "не заполнено".
# Единственная реальная константа для этого кода теперь здесь — раньше "" был
# рассеян как голый литерал по resolvers.py (has_error_checks/
# has_error_barriers/CommentFieldControl) и по REFERENCE_DATA ниже, без
# общей точки истины (то же самое SSOT-рассуждение, что и у SATISFACTORY
# выше).
RESULT_CODE_UNSATISFACTORY = " "


# [W1] 4 top-level транзакционные сущности (Root/Basic/CheckItem/Barrier),
# см. abap/README.md — независимые ETag на Root/Basic, композитный ключ
# RootId+ItemId / RootId+BarrierId на дочерних. Заменяет старую 3-сущностную
# модель CheckHeaders/CheckRows/Barriers.
# [Фаза 5, NW 7.50/pre-S4 1610] CheckRoots — составной ActiveUUID/DraftUUID
# ключ (см. serve.py CHECKROOT_KEY_RE/wrap_entity) — не используется напрямую
# для поиска в store (тот держит CheckRoots keyed by RootId), только для
# документации ожидаемого ключа и generic list-response key_parts building.
TRANSACTIONAL_ENTITIES = {
    "CheckRoots": ("ActiveUUID", "DraftUUID"),
    "CheckBasics": ("RootId",),
    "CheckItems": ("RootId", "ItemId"),
    "Barriers": ("RootId", "BarrierId"),
    "DraftAdministrativeData": ("DraftUUID",),
}

ENTITY_TYPES = {
    "CheckRoots": "ZCheckService.CheckRoot",
    "CheckBasics": "ZCheckService.CheckBasic",
    "CheckItems": "ZCheckService.CheckItem",
    "Barriers": "ZCheckService.Barrier",
    "DraftAdministrativeData": "ZCheckService.DraftAdministrativeData",
    "Persons": "ZCheckService.Person",
    "LocationHierarchy": "ZCheckService.LocationHierarchy",
    "Locations": "ZCheckService.Location",
    "BarrierTypes": "ZCheckService.BarrierType",
    "CheckTypes": "ZCheckService.CheckType",
    "CheckResults": "ZCheckService.CheckResult",
    "PkLevels": "ZCheckService.PkLevel",
    "TimeZones": "ZCheckService.TimeZone",
    "Professions": "ZCheckService.Profession",
}

REFERENCE_KEY_PROPS = {
    "Persons": "Pernr",
    "LocationHierarchy": "LocationUuid",
    "Locations": "LocationUuid",
    "BarrierTypes": "BarrierTypeCode",
    "CheckTypes": "CheckTypeCode",
    "CheckResults": "ResultCode",
    "PkLevels": "PkLevel",
    "TimeZones": "TimeZoneCode",
    "Professions": "ProfessionCode",
}


def _build_persons():
    raw = [
        {"pernr": "00000001", "position": "Инженер по ОТ", "full_name": "Иван Петров", "active_from": "/Date(1577836800000)/", "active_to": None},
        {"pernr": "00000002", "position": "Оператор", "full_name": "Мария Сидорова", "active_from": "/Date(1577836800000)/", "active_to": None},
        {"pernr": "00000003", "position": "Слесарь", "full_name": "Сергей Иванов", "active_from": "/Date(1640995200000)/", "active_to": None},
        {"pernr": "00000004", "position": "Специалист по ОТ", "full_name": "Анна Смирнова", "active_from": "/Date(1640995200000)/", "active_to": None},
        {"pernr": "00000005", "position": "Электромонтёр", "full_name": "Дмитрий Кузнецов", "active_from": "/Date(1672531200000)/", "active_to": None},
        {"pernr": "00000006", "position": "Оператор", "full_name": "Ольга Новикова", "active_from": "/Date(1672531200000)/", "active_to": "/Date(1719792000000)/"},
    ]
    return [
        {
            "Pernr": person["pernr"],
            "Fio": "%s %s" % (person["position"], person["full_name"]),
            "ActiveFrom": person["active_from"],
            "ActiveTo": person["active_to"],
        }
        for person in raw
    ]


REFERENCE_DATA = {
    "Persons": _build_persons(),
    "LocationHierarchy": [
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440101", "LocationCode": "SITE-1", "LocationName": "Площадка №1", "ParentLocationUuid": "", "HasChildren": True, "Level": 0},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440111", "LocationCode": "LOC-001", "LocationName": "Северный вход", "ParentLocationUuid": "550e8400-e29b-41d4-a716-446655440101", "HasChildren": False, "Level": 1},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440112", "LocationCode": "LOC-002", "LocationName": "Южный вход", "ParentLocationUuid": "550e8400-e29b-41d4-a716-446655440101", "HasChildren": False, "Level": 1},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440102", "LocationCode": "SITE-2", "LocationName": "Площадка №2", "ParentLocationUuid": "", "HasChildren": True, "Level": 0},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440121", "LocationCode": "LOC-003", "LocationName": "Склад ГСМ", "ParentLocationUuid": "550e8400-e29b-41d4-a716-446655440102", "HasChildren": False, "Level": 1},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440104", "LocationCode": "LOC-004", "LocationName": "Административный корпус", "ParentLocationUuid": "", "HasChildren": False, "Level": 0},
    ],
    "Locations": [
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440101", "LocationCode": "SITE-1", "LocationName": "Площадка №1"},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440111", "LocationCode": "LOC-001", "LocationName": "Северный вход"},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440112", "LocationCode": "LOC-002", "LocationName": "Южный вход"},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440102", "LocationCode": "SITE-2", "LocationName": "Площадка №2"},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440121", "LocationCode": "LOC-003", "LocationName": "Склад ГСМ"},
        {"LocationUuid": "550e8400-e29b-41d4-a716-446655440104", "LocationCode": "LOC-004", "LocationName": "Административный корпус"},
    ],
    # Category — [W1] новое поле, добавлено в ZI_CheckType/ZI_BarrierType для
    # группировки в TypePicker.js (в исходной 4-сущностной CDS-модели его не
    # было — см. abap/cds/ZI_CheckType.ddls.asddls).
    "BarrierTypes": [
        {"BarrierTypeCode": "SAFETY_FENCE", "BarrierTypeText": "Защитное ограждение", "Category": "Физические", "PkLevels": "0,1,2,3,4"},
        {"BarrierTypeCode": "WARNING_SIGN", "BarrierTypeText": "Предупреждающий знак", "Category": "Физические", "PkLevels": "0,1,2,3,4"},
        {"BarrierTypeCode": "LOCKOUT_TAGOUT", "BarrierTypeText": "Блокировка/бирка (LOTO)", "Category": "Процедурные", "PkLevels": "2,3,4"},
        {"BarrierTypeCode": "GUARD_RAIL", "BarrierTypeText": "Перильное ограждение", "Category": "Физические", "PkLevels": "3,4"},
        {"BarrierTypeCode": "EMERGENCY_STOP", "BarrierTypeText": "Аварийный стоп", "Category": "Физические", "PkLevels": "2,3,4"},
        {"BarrierTypeCode": "ACCESS_CONTROL", "BarrierTypeText": "Контроль доступа", "Category": "Процедурные", "PkLevels": "3,4"},
        {"BarrierTypeCode": "FIRE_SUPPRESSION", "BarrierTypeText": "Система пожаротушения", "Category": "Физические", "PkLevels": "4"},
        {"BarrierTypeCode": "GAS_DETECT", "BarrierTypeText": "Детектор газа", "Category": "Физические", "PkLevels": "4"},
    ],
    "CheckTypes": [
        {"CheckTypeCode": "VISUAL_INSPECTION", "CheckTypeText": "Визуальный осмотр", "Category": "Общие", "PkLevels": "0,1,2,3,4"},
        {"CheckTypeCode": "DOCUMENT_REVIEW", "CheckTypeText": "Проверка документов", "Category": "Общие", "PkLevels": "0,1,2,3,4"},
        {"CheckTypeCode": "INTERVIEW", "CheckTypeText": "Опрос персонала", "Category": "Персонал", "PkLevels": "2,3,4"},
        {"CheckTypeCode": "EQUIPMENT_CHECK", "CheckTypeText": "Проверка оборудования", "Category": "Оборудование", "PkLevels": "3,4"},
        {"CheckTypeCode": "AUDIT", "CheckTypeText": "Аудит процессов", "Category": "Процессы", "PkLevels": "4"},
        {"CheckTypeCode": "RISK_ASSESSMENT", "CheckTypeText": "Оценка рисков", "Category": "Процессы", "PkLevels": "3,4"},
        {"CheckTypeCode": "TRAINING_VERIFY", "CheckTypeText": "Проверка обучения", "Category": "Персонал", "PkLevels": "1,2,3,4"},
        {"CheckTypeCode": "EMERGENCY_DRILL", "CheckTypeText": "Тренировка по авариям", "Category": "Процессы", "PkLevels": "4"},
    ],
    "CheckResults": [
        {"ResultCode": RESULT_CODE_SATISFACTORY, "ResultText": "Удовлетворительно"},
        {"ResultCode": RESULT_CODE_UNSATISFACTORY, "ResultText": "Неудовлетворительно"},
    ],
    "PkLevels": [
        {"PkLevel": "0", "PkLevelText": "КПР-0"},
        {"PkLevel": "1", "PkLevelText": "КПР-1"},
        {"PkLevel": "2", "PkLevelText": "КПР-2"},
        {"PkLevel": "3", "PkLevelText": "КПР-3"},
        {"PkLevel": "4", "PkLevelText": "КПР-4"},
    ],
    "TimeZones": [
        {"TimeZoneCode": "Europe/Moscow", "TimeZoneText": "Москва (UTC+3)"},
        {"TimeZoneCode": "Asia/Yekaterinburg", "TimeZoneText": "Екатеринбург (UTC+5)"},
        {"TimeZoneCode": "Asia/Novosibirsk", "TimeZoneText": "Новосибирск (UTC+7)"},
        {"TimeZoneCode": "Asia/Vladivostok", "TimeZoneText": "Владивосток (UTC+10)"},
        {"TimeZoneCode": "Asia/Yerevan", "TimeZoneText": "Ереван (UTC+4)"},
        {"TimeZoneCode": "Europe/Minsk", "TimeZoneText": "Минск (UTC+3)"},
        {"TimeZoneCode": "Asia/Almaty", "TimeZoneText": "Алма-Ата (UTC+6)"},
        {"TimeZoneCode": "Asia/Tashkent", "TimeZoneText": "Ташкент (UTC+5)"},
        {"TimeZoneCode": "UTC", "TimeZoneText": "UTC (UTC+0)"},
    ],
    "Professions": [
        {"ProfessionCode": "ELECTRICIAN", "ProfessionText": "Электромонтёр"},
        {"ProfessionCode": "FITTER", "ProfessionText": "Слесарь"},
        {"ProfessionCode": "OPERATOR", "ProfessionText": "Оператор"},
        {"ProfessionCode": "ENGINEER", "ProfessionText": "Инженер"},
        {"ProfessionCode": "SAFETY_OFFICER", "ProfessionText": "Специалист по ОТ"},
    ],
}


# [Аудит: OWASP API3:2023 Broken Object Property Level Authorization]
# PUT/PATCH/MERGE раньше копировали присланное тело клиента поверх хранимой
# записи без фильтрации — позволяя переписать read-only/вычисляемые поля
# (Status, ThisIsIntegrationData, ChecksAmount, CreatedBy, ...), хотя они уже
# помечены sap:creatable="false"/sap:updatable="false" в localService/metadata.xml.
# Списки ниже — та же информация, продублированная для рантайм-проверки на
# Python-стороне (metadata.xml не парсится сервером в рантайме, см. serve.py
# read_static_file) — при добавлении нового read-only поля в metadata.xml
# нужно синхронно дополнить и здесь.
READONLY_FIELDS = {
    "CheckRoots": {
        "RootId", "DocId", "Status", "StatusCriticality", "ThisIsIntegrationData",
        "ChecksAmount", "ChecksSuccess", "BarriersAmount", "BarriersSuccess",
        "SuccessRateChecks", "SuccessRateBarriers", "ChecksCriticality", "BarriersCriticality",
        "HasErrorChecks", "HasErrorBarriers", "HeaderKpiTitle", "HeaderKpiSubtitle",
        "ChecksHidden", "BarriersHidden", "IntegrationBadgeHidden",
        "ChecksErrorBadgeHidden", "BarriersErrorBadgeHidden", "Updatable",
        "LastChangedAt", "CreatedBy", "CreatedAt",
    },
    "CheckBasics": {
        "RootId", "TimezoneText", "LocationText", "ObserverPosition", "ObserverOrgunit",
        "ObserverIntegrationName", "ObservedPosition", "ObservedOrgunit",
        "ObservedIntegrationName", "LpcText", "ProfText", "LastChangedAt",
    },
    "CheckItems": {"RootId", "ItemId", "LastChangedAt", "CommentFieldControl", "Deletable"},
    "Barriers": {"RootId", "BarrierId", "LastChangedAt", "CommentFieldControl", "Deletable"},
}


def strip_readonly_fields(set_name, body):
    """Отбрасывает из тела запроса свойства, помеченные read-only в
    metadata.xml для set_name — вызывается на КАЖДЫЙ PUT/PATCH/MERGE до
    применения тела к хранимой записи."""
    readonly = READONLY_FIELDS.get(set_name)
    if not readonly:
        return dict(body or {})
    return {k: v for k, v in (body or {}).items() if k not in readonly}


class BUSINESS_RULES:
    CRITICALITY_NEGATIVE_MAX = 50
    CRITICALITY_CRITICAL_MAX = 80

    # [W1/аудит W4] BARRIERS_HIDDEN_PK_LEVELS/CHECKS_HIDDEN_PK_LEVEL — то же
    # правило, что раньше жило ТОЛЬКО в ext/util/Constants.js на клиенте
    # (см. аудит, Weak Spot W4). Теперь единственный источник истины здесь
    # (push-down в ответе сервера, ChecksHidden/BarriersHidden на CheckRoots),
    # клиент их больше не пересчитывает для сохранённых записей.
    BARRIERS_HIDDEN_PK_LEVELS = ("", "0", "1")

    CHECKS_HIDDEN_PK_LEVEL = ""

    CHECK_TYPE_RAW_TEXT_CODE = "000.000"

    # [Fix] Chart visualization thresholds for UI.Chart Bullet annotations.
    # SAPUI5 1.71 VizFrame uses these to render color zones in bullet charts.
    # SSOT: defined here in Python backend, pushed down via compute_check_root_view,
    # NOT duplicated in JS Constants.js or CDS views.
    CHART_TARGET_ZONE_MIN = 80  # Green zone >= 80%
    CHART_WARNING_ZONE_MIN = 50  # Yellow zone 50-79%, Red < 50%
