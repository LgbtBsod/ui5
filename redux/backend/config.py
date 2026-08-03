"""Static configuration: constants, entity-type/key metadata, and the
switchable-mock-user registry. No mutable state lives here (see state.py) -
everything in this module is safe to import from anywhere else in the
package without risking an import cycle.
"""
import os
from datetime import datetime, timezone

import serve_config

DEFAULT_PORT = serve_config.DEFAULT_PORT
ODATA_PREFIX = serve_config.ODATA_PREFIX
# backend/config.py -> backend/ -> redux/ (two levels up) - this must land on
# the redux/ project root (where index.html, annotation/, localService/ live)
# exactly like the original serve.py's WEBAPP_DIR did when the file lived
# directly in redux/, one level shallower.
WEBAPP_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CSRF_TOKEN = serve_config.CSRF_TOKEN

REFERENCE_DATA = serve_config.REFERENCE_DATA

DATASET_INDEX = {
    set_name: (serve_config.ENTITY_TYPES[set_name], key_prop)
    for set_name, key_prop in serve_config.REFERENCE_KEY_PROPS.items()
}

# [Фаза 5, NW 7.50/pre-S4 1610] "Заглушка" вместо NULL для стороны пары
# ActiveUUID/DraftUUID, которая не применима к текущей строке (классический
# ручной draft-паттерн на NW 7.50/pre-S4 1610 — пример в системе:
# SEPMRA_PD_D). У активной строки DraftUUID всегда таким, у create-драфта
# (без активного двойника) — ActiveUUID.
ZERO_GUID = "00000000-0000-0000-0000-000000000000"

# [Fix] Default identity + a small switchable-user registry. Originally a
# single fixed identity (no multi-user concept at all), which meant the whole
# "someone else has this draft locked" branch of the real Fiori/BOPF draft
# protocol (sap.suite.ui.generic.template's checkForForeignUserLock,
# DraftAdministrativeData.InProcessByUser, the "ST_GENERIC_DRAFT_LOCKED_BY_
# USER" message, the EditState "Locked" quick filter) was permanently
# dormant/untestable - there was never anyone else to be locked by.
# _resolve_mock_user (state.py) reads an `X-Mock-User` request header (same
# convention as ../backend/mock_gateway's CurrentUserService.resolve_uname),
# defaulting to MOCK_USER so ordinary UI5 traffic (which never sends this
# header) is unaffected - this is purely a manual-testing knob, gated on
# nothing else since this whole server is already a trust-everything local
# dev mock.
MOCK_USER = "MOCK_USER"
MOCK_USER_REGISTRY = {
    # uname -> display name, for the header-selectable identities used to
    # simulate a second/third user editing the same object. Any uname not
    # listed here still works via the header - it just displays as itself
    # (see state._mock_user_display_name) - this registry only exists to
    # give the two or three "usual suspects" a friendly Cyrillic display name
    # matching the rest of this mock's seed data conventions.
    "MOCK_USER": "Mock User",
    "PETROV": "Инженер по ОТ Иван Петров",
    "SIDOROVA": "Оператор Мария Сидорова",
}
MOCK_USER_DISPLAY_NAME = "Mock User"

KEY_PROP_TUPLES = serve_config.TRANSACTIONAL_ENTITIES  # entity_set -> (prop, ...) composite key
ENTITY_TYPES = serve_config.ENTITY_TYPES

# [Basic-proxy / EMPIRICAL FIX] Fiori Elements Object Page (SAPUI5 1.71,
# OData v2, composition 0..1) не резолвит BindingContext для facet, чей
# UI.FieldGroup стоит на entity-навигации (to_Basic) — проверено эмпирически
# на реальном UI. Единственный рабочий вариант: эти поля редактируются
# ПРЯМО на CheckRoots (см. ZC_CheckRoot.ddls.asddls), а DPC_EXT-прокси
# (здесь) незаметно маршрутизирует их запись в CheckBasics — SSOT по
# значению остаётся там, собственный ETag CheckBasics не затрагивается
# записью через Root. Не путать с DocId/Status/ThisIsIntegrationData —
# те принадлежат Root напрямую.
BASIC_PROXY_FIELDS = {
    "Date", "Time", "Timezone", "LocationKey", "LocationName", "Equipment",
    "ObserverFullname", "ObserverPerner", "ObservedFullname", "ObservedPerner",
    "LpcKey", "ProfKey"
}

ETAG_SENTINEL_MS = int(datetime(1999, 1, 1, tzinfo=timezone.utc).timestamp() * 1000)

# [BUG #5 FIX] Список обязательных полей соответствует Nullable="false" в
# metadata.xml + Common.Required в annotations.xml. Single source of truth
# shared by _draft_activate's required-field gate (draft_service.py).
REQUIRED_ROOT_FIELDS = {
    "ObserverFullname": "ФИО инспектора",
    "ObservedFullname": "ФИО проверяемого",
    "Date": "Дата проверки",
    "Time": "Время проверки",
    "Timezone": "Часовой пояс",
    "LpcKey": "Уровень КПР",
    "ProfKey": "Профессия",
}

# [Fix] ObserverFullname/ObservedFullname get filled two different ways: the
# real UI's F4 value-help writes the picked person's name into this field
# directly (Common.ValueList InOut, see annotations.xml), OR a record can be
# identified purely by an ObserverPerner/ObservedPerner reference and have
# its display name resolved live at read time (resolve_check_basic's Persons
# lookup - this is how seed_test_data's records work). Both are legitimate
# "this field is filled" signals; checking only the literal free-text key
# would reject Save/Activate on a perfectly valid Perner-identified record
# (one that displays a real name on screen via the lookup) just because
# nothing ever happened to ALSO copy that name into the free-text field.
REQUIRED_FIELD_ALTERNATES = {
    "ObserverFullname": "ObserverPerner",
    "ObservedFullname": "ObservedPerner",
}

# [Fix, exhaustive-sweep pass] Every tagged CheckItem/Barrier row must have a
# recorded Result (not None - a blank/unanswered check is meaningless data)
# before its parent draft can Activate. Deliberately NOT a metadata.xml
# Nullable="false" - a freshly-added row legitimately has no Result yet
# while the checklist is still being filled in, same rationale as
# REQUIRED_ROOT_FIELDS staying Nullable="true" and gating at Activation only.
REQUIRED_CHILD_FIELD = "Result"
REQUIRED_CHILD_FIELD_LABEL = "Результат"
