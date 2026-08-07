"""Static configuration: constants, entity-type/key metadata, and the
switchable-mock-user registry. No mutable state lives here (see state.py) -
everything in this module is safe to import from anywhere else in the
package without risking an import cycle.

Clean Code improvements:
- Centralized magic numbers and constants
- Type hints for better IDE support and static analysis
- Clear separation of concerns
- Self-documenting variable names
"""
import os
from datetime import datetime, timezone
from typing import Dict, Tuple, Any

import serve_config

# =============================================================================
# Server Configuration
# =============================================================================
DEFAULT_PORT = serve_config.DEFAULT_PORT
ODATA_PREFIX = serve_config.ODATA_PREFIX
CSRF_TOKEN = serve_config.CSRF_TOKEN

# Directory resolution: backend/config.py -> backend/ -> redux/ (two levels up)
# This must land on the redux/ project root (where index.html, annotation/,
# localService/ live) exactly like the original serve.py's WEBAPP_DIR did.
WEBAPP_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# =============================================================================
# Data Configuration
# =============================================================================
REFERENCE_DATA = serve_config.REFERENCE_DATA

DATASET_INDEX: Dict[str, Tuple[Any, str]] = {
    set_name: (serve_config.ENTITY_TYPES[set_name], key_prop)
    for set_name, key_prop in serve_config.REFERENCE_KEY_PROPS.items()
}

KEY_PROP_TUPLES: Dict[str, Tuple[str, ...]] = serve_config.TRANSACTIONAL_ENTITIES
ENTITY_TYPES: Dict[str, str] = serve_config.ENTITY_TYPES

# =============================================================================
# Draft Protocol Constants
# =============================================================================
# [Фаза 5, NW 7.50/pre-S4 1610] "Заглушка" вместо NULL для стороны пары
# ActiveUUID/DraftUUID, которая не применима к текущей строке (классический
# manual draft-паттерн на NW 7.50/pre-S4 1610 — пример в системе:
# SEPMRA_PD_D). У активной строки DraftUUID всегда таким, у create-драфта
# (без активного двойника) — ActiveUUID.
ZERO_GUID: str = "00000000-0000-0000-0000-000000000000"

# ETag sentinel value for entities without proper ETags
ETAG_SENTINEL_MS: int = int(datetime(1999, 1, 1, tzinfo=timezone.utc).timestamp() * 1000)

# =============================================================================
# Mock User Registry
# =============================================================================
# [Fix] Default identity + a small switchable-user registry. Originally a
# single fixed identity (no multi-user concept at all), which meant the whole
# "someone else has this draft locked" branch of the real Fiori/BOPF draft
# protocol was permanently dormant/untestable.
MOCK_USER: str = "MOCK_USER"
MOCK_USER_DISPLAY_NAME: str = "Mock User"

MOCK_USER_REGISTRY: Dict[str, str] = {
    # uname -> display name, for the header-selectable identities used to
    # simulate a second/third user editing the same object.
    "MOCK_USER": "Mock User",
    "PETROV": "Инженер по ОТ Иван Петров",
    "SIDOROVA": "Оператор Мария Сидорова",
}

# =============================================================================
# Business Rules & Validation
# =============================================================================
# [Basic-proxy / EMPIRICAL FIX] Fiori Elements Object Page (SAPUI5 1.71,
# OData v2, composition 0..1) не резолвит BindingContext для facet, чей
# UI.FieldGroup стоит на entity-навигации (to_Basic) — проверено эмпирически
# на реальном UI. Единственный рабочий вариант: эти поля редактируются
# ПРЯМО на CheckRoots, а DPC_EXT-прокси незаметно маршрутизирует их запись
# в CheckBasics — SSOT по значению остаётся там.
BASIC_PROXY_FIELDS: frozenset[str] = frozenset({
    "Date", "Time", "Timezone", "LocationKey", "LocationName", "Equipment",
    "ObserverFullname", "ObserverPerner", "ObservedFullname", "ObservedPerner",
    "LpcKey", "ProfKey"
})

# [BUG #5 FIX] Список обязательных полей соответствует Nullable="false" в
# metadata.xml + Common.Required в annotations.xml. Single source of truth
# shared by _draft_activate's required-field gate (draft_service.py).
REQUIRED_ROOT_FIELDS: Dict[str, str] = {
    "ObserverFullname": "ФИО инспектора",
    "ObservedFullname": "ФИО проверяемого",
    "Date": "Дата проверки",
    "Time": "Время проверки",
    "Timezone": "Часовой пояс",
    "LpcKey": "Уровень КПР",
    "ProfKey": "Профессия",
}

# [Fix] ObserverFullname/ObservedFullname get filled two different ways:
# F4 value-help writes the picked person's name directly, OR a record can be
# identified purely by Perner reference with display name resolved at read time.
REQUIRED_FIELD_ALTERNATES: Dict[str, str] = {
    "ObserverFullname": "ObserverPerner",
    "ObservedFullname": "ObservedPerner",
}

# [Fix, exhaustive-sweep pass] Every tagged CheckItem/Barrier row must have a
# recorded Result before its parent draft can Activate.
REQUIRED_CHILD_FIELD: str = "Result"
REQUIRED_CHILD_FIELD_LABEL: str = "Результат"
