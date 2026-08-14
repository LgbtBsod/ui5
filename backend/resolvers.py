"""Read-only resolution/formatting layer: turns raw store rows into their
OData wire shape. Everything here is a pure function of (config, state) plus
its own arguments - no mutation of store/draft_store happens in this module.
Depends on config.py, state.py, odata_format.py only (draft_service.py
depends on this module, not the other way around, to avoid an import cycle
between "read a draft's admin data" and "mutate a draft").

Refactored with type hints (PEP 484) and reduced cyclomatic complexity by
extracting helper functions for filter evaluation and expand operations.

[Fix, best-practices pass] The docstring used to also claim a "Strategy
Pattern for filter evaluation (filter_strategies.py)" - that module existed
as a full parallel implementation (FilterStrategyFactory + one class per
operator) but was never actually wired up: eval_one_filter/apply_filter
below have always been, and still are, a plain regex-match chain. The
import and the never-read `_filter_factory` global were dead scaffolding
left over from an incomplete refactor - removed along with
filter_strategies.py itself (zero other consumers anywhere in the repo).
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional, Tuple, Callable
# [Fix, use-case pass] Was `from functools import lru_cache` - lru_cache was
# never actually used anywhere in this file (dead import), while
# apply_orderby() below calls `functools.cmp_to_key` - the MODULE, never
# imported under that name - a real, live NameError on every $orderby query.
# It went undetected because no client this session ever sent one until the
# new standard sap.ui.comp.valuehelpdialog.ValueHelpDialog (replacing the
# old custom TypePicker, which sorted client-side and never sent $orderby)
# started sorting its own results by default - confirmed via the live
# server log ("NameError: name 'functools' is not defined") the moment that
# dialog's default column sort fired a real $orderby request.
from functools import cmp_to_key

import serve_config

from . import config
from . import state
from . import odata_format


# Type aliases
EntityRow = Dict[str, Any]
FilterPredicate = Callable[[EntityRow], bool]
KeyParts = Dict[str, str]
QueryParams = Dict[str, Any]


def _ref_lookup(
    set_name: str,
    code_field: str,
    text_field: str,
    code_value: Any
) -> Optional[Any]:
    """Look up reference text by code from reference data.
    
    Args:
        set_name: Reference entity set name (e.g., "Persons", "PkLevels")
        code_field: Field name containing the code
        text_field: Field name containing the display text
        code_value: Code value to look up
        
    Returns:
        Display text if found, None otherwise
    """
    for row in config.REFERENCE_DATA.get(set_name, []):
        if str(row.get(code_field)) == str(code_value):
            return row.get(text_field)
    return None


def resolve_check_basic(row: EntityRow) -> EntityRow:
    """Resolve reference texts for CheckBasics entity.
    
    Push-down resolver for CheckBasics - analogous to ZI_CheckBasic CDS view
    (coalesce Person.Fio / *_IntegrationName, LpcText/ProfText/TimezoneText/LocationName).
    
    Args:
        row: Raw CheckBasics entity row
        
    Returns:
        Resolved entity row with display texts
    """
    out = dict(row)

    observer_fio = _ref_lookup("Persons", "Pernr", "Fio", out.get("ObserverPernr")) if out.get("ObserverPernr") else None
    out["ObserverFullname"] = observer_fio or out.get("ObserverIntegrationName") or ""

    observed_fio = _ref_lookup("Persons", "Pernr", "Fio", out.get("ObservedPernr")) if out.get("ObservedPernr") else None
    out["ObservedFullname"] = observed_fio or out.get("ObservedIntegrationName") or ""

    out["LpcText"] = _ref_lookup("PkLevels", "PkLevel", "PkLevelText", out.get("LpcKey")) or "" if out.get("LpcKey") else ""
    out["ProfText"] = _ref_lookup("Professions", "ProfessionCode", "ProfessionText", out.get("ProfKey")) or "" if out.get("ProfKey") else ""
    out["TimezoneText"] = _ref_lookup("TimeZones", "TimeZoneCode", "TimeZoneText", out.get("Timezone")) or "" if out.get("Timezone") else ""

    if out.get("LocationKey"):
        resolved_name = _ref_lookup("Locations", "LocationUuid", "LocationName", out["LocationKey"])
        if resolved_name:
            out["LocationName"] = resolved_name

    return out


def _resolve_type_and_result(
    row: EntityRow,
    type_set: str,
    type_code_field: str,
    type_text_field: str,
    raw_text_fallback: bool = False
) -> EntityRow:
    """Resolve type code to text and result code to text for check items/barriers.
    
    Common logic for resolve_check_item/resolve_barrier: Code->Text lookup from
    type reference data + Result->ResultText lookup from CheckResults.
    
    Args:
        row: Raw entity row
        type_set: Reference entity set name (e.g., "CheckTypes", "BarrierTypes")
        type_code_field: Field name containing the type code
        type_text_field: Field name containing the type display text
        raw_text_fallback: If True, use RawText as fallback when type not found
                          (used for resolve_check_item, not for resolve_barrier)
        
    Returns:
        Entity row with resolved Text and ResultText fields
    """
    out = dict(row)
    type_text = _ref_lookup(type_set, type_code_field, type_text_field, out.get("Code")) if out.get("Code") else None
    if raw_text_fallback:
        out["Text"] = type_text or out.get("RawText") or ""
    else:
        out["Text"] = type_text or ""
    out["ResultText"] = _ref_lookup("CheckResults", "ResultCode", "ResultText", out.get("Result")) or "" if out.get("Result") is not None else ""
    # [Fix] CheckItem/Barrier are real draft nodes now (Common.DraftNode) -
    # IsActiveEntity mirrors CheckRoot's own draft signature so the standard
    # template/SmartTable can tell an active-instance row from a
    # draft-instance one, same convention as the root.
    out["IsActiveEntity"] = out.get("DraftUUID", config.ZERO_GUID) == config.ZERO_GUID
    # [Fix, exhaustive-sweep pass] Common.FieldControl (annotations.xml) live-
    # binds Comment's mandatory state to this Edm.Byte value (7=mandatory,
    # 3=optional) via sap.ui.comp.smartfield.FieldControl - recomputed
    # automatically whenever Result changes, no reload/JS needed. "Failing"
    # uses the exact same check as compute_check_root_view's
    # has_error_checks/has_error_barriers (CheckResults' only two reference
    # codes are RESULT_CODE_SATISFACTORY="X" and RESULT_CODE_UNSATISFACTORY=
    # " " - see serve_config.py for why it's a space, not "". An unanswered
    # Result is None, not the unsatisfactory code, so this never misfires on
    # a brand-new not-yet-assessed row).
    out["CommentFieldControl"] = 7 if out.get("Result") == serve_config.RESULT_CODE_UNSATISFACTORY else 3
    # [Fix, use-case pass] sap:deletable-path (metadata.xml) reads this
    # Boolean to gate the ResponsiveTable's per-row Delete button. This used
    # to be `out.get("Result") is None` - "only deletable before the
    # inspector has picked anything" - which worked only because the OLD
    # add-row dialog (TypePicker/LocalItemSuggest, removed this session)
    # created the row EMPTY and left Result unset until the user confirmed a
    # choice at the very end, so there was a real window where Result was
    # None. The NEW add-row dialog (SubTableCrud.js) creates the row via
    # createEntry() with Result defaulted to "X" (Satisfactory) immediately,
    # as a convenience default the SmartField then displays pre-filled - so
    # by the time ANY row (new or pre-existing) ever reaches this resolver,
    # Result is never None. Confirmed live: with the old check still in
    # place, the Delete button stayed permanently disabled for every row in
    # both tables, old and new alike - not a narrower "can't delete
    # classified rows" restriction, a total dead button. Fiori draft UX
    # already gives the right protection for free: this entity is a
    # Common.DraftNode, so Delete only ever acts on the current unsaved
    # draft (the standard native button's own confirmation dialog is the
    # remaining safeguard) - once Saved, the row becomes part of the active
    # version like everything else on the page, same as any other field
    # edit. No extra Result-based gate is needed on top of that.
    out["Deletable"] = True
    return out


def resolve_check_item(row: EntityRow) -> EntityRow:
    """Resolve CheckItem entity with type and result texts.
    
    Args:
        row: Raw CheckItem entity row
        
    Returns:
        Resolved entity row with Text, ResultText, and computed fields
    """
    return _resolve_type_and_result(row, "CheckTypes", "CheckTypeCode", "CheckTypeText", raw_text_fallback=True)


def resolve_barrier(row: EntityRow) -> EntityRow:
    """Resolve Barrier entity with type and result texts.
    
    Args:
        row: Raw Barrier entity row
        
    Returns:
        Resolved entity row with Text, ResultText, and computed fields
    """
    return _resolve_type_and_result(row, "BarrierTypes", "BarrierTypeCode", "BarrierTypeText")


def _rate_to_criticality(rate: float, total: int) -> int:
    """Convert success rate percentage to criticality level (1=Critical, 2=Warning, 3=Good).
    
    Args:
        rate: Success rate percentage (0-100)
        total: Total number of items
        
    Returns:
        Criticality level: 1 (Critical), 2 (Warning), or 3 (Good/Satisfactory)
    """
    if not total:
        return 3
    if rate >= 90:
        return 3
    if rate >= 70:
        return 2
    return 1


def compute_check_root_view(
    root_id: str,
    root_override: Optional[EntityRow] = None
) -> Optional[EntityRow]:
    """Compute CheckRoot view with KPIs, criticality, and computed fields.
    
    Push-down KPI/criticality/hidden/HeaderKpi* calculations - analogous to 
    ZI_CheckRoot CDS view (push-down on HANA in real CDS). Computed on each 
    read from store, not stored denormalized.
    
    Args:
        root_id: Root ID to compute view for
        root_override: Optional override row (for draft rows from draft_store 
                      instead of active store). CheckBasics still resolves from 
                      active root_id (root-scoped proxy-fields, not draft-node).
                      CheckItems/Barriers are now real draft-nodes (Common.DraftNode) 
                      - resolve from instance_tag (draft_row['DraftUUID'] when 
                      viewing draft, otherwise ZERO_GUID/active).
        
    Returns:
        Computed entity row with all KPI and computed fields, or None if not found
    """
    root = root_override if root_override is not None else state.store["CheckRoots"].get(root_id)
    if root is None:
        return None
    instance_tag = root.get("DraftUUID", config.ZERO_GUID) if root_override is not None else config.ZERO_GUID
    basic = state.store["CheckBasics"].get(root_id)
    checks = state._checks_of(root_id, instance_tag)
    barriers = state._barriers_of(root_id, instance_tag)

    checks_amount = len(checks)
    checks_success = sum(1 for c in checks if c.get("Result") == serve_config.RESULT_CODE_SATISFACTORY)
    has_error_checks = any(c.get("Result") == serve_config.RESULT_CODE_UNSATISFACTORY for c in checks)

    barriers_amount = len(barriers)
    barriers_success = sum(1 for b in barriers if b.get("Result") == serve_config.RESULT_CODE_SATISFACTORY)
    has_error_barriers = any(b.get("Result") == serve_config.RESULT_CODE_UNSATISFACTORY for b in barriers)

    success_rate_checks = round(checks_success / checks_amount * 100, 2) if checks_amount else None
    success_rate_barriers = round(barriers_success / barriers_amount * 100, 2) if barriers_amount else None

    lpc_key = str((basic or {}).get("LpcKey") or "")

    out = dict(root)
    # Internal bookkeeping (see draft_service._snapshot_children/
    # _draft_admin_data_row) - never part of the OData wire shape.
    out.pop("_child_snapshot", None)
    out.pop("_draft_created_at", None)
    out["ChecksAmount"] = checks_amount
    out["ChecksSuccess"] = checks_success
    out["BarriersAmount"] = barriers_amount
    out["BarriersSuccess"] = barriers_success
    out["SuccessRateChecks"] = success_rate_checks
    out["SuccessRateBarriers"] = success_rate_barriers
    out["ChecksCriticality"] = _rate_to_criticality(round(checks_success / checks_amount * 100) if checks_amount else 0, checks_amount)
    out["BarriersCriticality"] = _rate_to_criticality(round(barriers_success / barriers_amount * 100) if barriers_amount else 0, barriers_amount)
    out["HasErrorChecks"] = has_error_checks
    out["HasErrorBarriers"] = has_error_barriers
    out["StatusCriticality"] = {"CRITICAL": 1, "WARNING": 2}.get(root.get("Status"), 3)

    # [Basic-proxy] Полный набор полей-прокси Basic на Root (см.
    # config.BASIC_PROXY_FIELDS) — обязателен здесь целиком, иначе после
    # save+refetch поля Object Page (Time/ObserverFullname/...) откатятся
    # к undefined, хотя в CheckBasics данные сохранены корректно.
    basic_resolved = resolve_check_basic(basic) if basic else {}
    for field in config.BASIC_PROXY_FIELDS:
        out[field] = basic_resolved.get(field)
    out["LpcText"] = basic_resolved.get("LpcText") or ""
    out["ProfText"] = basic_resolved.get("ProfText") or ""
    out["TimezoneText"] = basic_resolved.get("TimezoneText") or ""
    out["LocationName"] = basic_resolved.get("LocationName") or ""

    # SSOT-предупреждение: этот формат независимо задублирован для
    # transient-объекта (до первого save) в ext/util/KpiSync.js через
    # i18n-ключи kpiTitleFormat/kpiSubtitleFormat (i18n/i18n.properties).
    # Единый источник истины здесь (сохранённая запись) — при изменении
    # текста/порядка полей править синхронно оба места.
    out["HeaderKpiTitle"] = "КПР: %s · Профессия: %s" % (out["LpcText"] or "—", out["ProfText"] or "—")
    out["HeaderKpiSubtitle"] = "Успешных проверок: %d · Барьеров: %d" % (checks_success, barriers_amount)

    # [Fix] Chart target/warning values for UI.Chart Bullet visualization.
    # Push-down from serve_config.BUSINESS_RULES CHART_* constants (SSOT).
    # SAPUI5 1.71 VizFrame reads these to render color zones in bullet charts.
    out["ChecksChartTargetValue"] = round(checks_amount * serve_config.BUSINESS_RULES.CHART_TARGET_ZONE_MIN / 100) if checks_amount else 0
    out["BarriersChartTargetValue"] = round(barriers_amount * serve_config.BUSINESS_RULES.CHART_TARGET_ZONE_MIN / 100) if barriers_amount else 0
    out["ChecksChartWarningValue"] = round(checks_amount * serve_config.BUSINESS_RULES.CHART_WARNING_ZONE_MIN / 100) if checks_amount else 0
    out["BarriersChartWarningValue"] = round(barriers_amount * serve_config.BUSINESS_RULES.CHART_WARNING_ZONE_MIN / 100) if barriers_amount else 0

    out["ChecksHidden"] = (lpc_key == serve_config.BUSINESS_RULES.CHECKS_HIDDEN_PK_LEVEL)
    out["BarriersHidden"] = lpc_key in serve_config.BUSINESS_RULES.BARRIERS_HIDDEN_PK_LEVELS

    out["ThisIsIntegrationData"] = bool(out.get("ThisIsIntegrationData", False))

    # [Fix, use-case pass #4] Integration-sourced records are view-only end to
    # end - editing them was previously allowed after a warn-and-confirm
    # dialog (IntegrationEditGuard.js), but per the real business rule this
    # data has no owner on our side to correct: re-syncing from the
    # integration feed wouldn't pick up local edits anyway (see
    # integrationEditWarningMsg), so letting anyone edit it was always a false
    # affordance. sap:updatable-path="Updatable" + sap:deletable-path=
    # "Updatable" (metadata.xml) and UI.Updatable/UI.Deletable Path=
    # "Updatable" (check-root.xml) drive the client's own "Редактировать"/
    # "Удалить" buttons off this same field - single source of truth, no
    # duplicated client-side logic. No server-side PATCH/DELETE rejection
    # backstop here (deliberately kept out of scope, see the user's own
    # "не паримся" call) - the annotation-driven button hide is the sole
    # enforcement layer; a client bypassing it entirely (direct OData call,
    # stale cached UI) is not guarded against server-side.
    out["Updatable"] = not out["ThisIsIntegrationData"]

    out["IntegrationBadgeHidden"] = not out["ThisIsIntegrationData"]
    out["ChecksErrorBadgeHidden"] = not has_error_checks
    out["BarriersErrorBadgeHidden"] = not has_error_barriers

    # [Fix, minimal-extension-set pass] Criticality for the 3 header badges
    # (annotations.xml check-root.xml's FieldGroup#General). A constant/
    # EnumMember Criticality on a boolean UI.DataField was confirmed live to
    # NOT reach sap.ui.comp.smartfield.ODataControlFactory's ObjectStatus
    # state/icon in this exact SAPUI5 1.71.84 build (renders
    # sap-icon://status-inactive / ValueState.None regardless of the
    # EnumMember given) - only a Path-bound Criticality does, the same
    # already-proven pattern this file uses for ChecksCriticality/
    # BarriersCriticality on the UI.DataPoint progress bars. Same numeric
    # convention as StatusCriticality above (0=None/1=Error/2=Warning/
    # 3=Success, per sap.ui.core.ValueState via ODataControlFactory's
    # mStatesInt) - these badges only ever need None/Error/Neutral-as-None,
    # so a plain 0/1 constant per flag is enough, no percentage math needed.
    out["ChecksErrorCriticality"] = 1 if has_error_checks else 0
    out["BarriersErrorCriticality"] = 1 if has_error_barriers else 0
    out["IntegrationCriticality"] = 0
    return out


def wrap_entity(
    entity_set: str,
    entity_type: str,
    row: EntityRow,
    key_parts: Optional[KeyParts]
) -> EntityRow:
    """Wrap raw entity row with OData metadata and computed fields.
    
    Args:
        entity_set: Entity set name (e.g., "CheckRoots", "CheckBasics")
        entity_type: Entity type name from metadata
        row: Raw entity row from store
        key_parts: Key parts for URL generation
        
    Returns:
        Wrapped entity row with __metadata and computed fields
    """
    if entity_set == "CheckRoots":
        # [Фаза 5] is_active определяется ПРОИСХОЖДЕНИЕМ строки (откуда её
        # достал вызывающий код — store vs draft_store), а не разбором ключа
        # из URL: единственный надёжный источник истины, раз в ключе больше
        # нет явного IsActiveEntity-флага. row["IsActiveEntity"] выставляется
        # там же, где строка извлекается (query._find_by_key/_resolve_source/
        # draft_service функции) — см. _augment_checkroot.
        root_id = row["RootId"]
        is_active = bool(row.get("IsActiveEntity", True))
        if is_active:
            with_text = compute_check_root_view(root_id)
            active_uuid, draft_uuid = root_id, config.ZERO_GUID
            has_draft_entity = state._find_draft_by_active_uuid(root_id)[0] is not None
            has_active_entity = True
        else:
            with_text = compute_check_root_view(root_id, root_override=row)
            active_uuid = row.get("ActiveUUID") or config.ZERO_GUID
            draft_uuid = row["DraftUUID"]
            has_draft_entity = False
            has_active_entity = active_uuid != config.ZERO_GUID
        with_text["RootId"] = root_id
        with_text["ActiveUUID"] = active_uuid
        with_text["DraftUUID"] = draft_uuid
        with_text["IsActiveEntity"] = is_active
        with_text["HasActiveEntity"] = has_active_entity
        with_text["HasDraftEntity"] = has_draft_entity
        # [Fix] Both branches now go through the same aggregating
        # odata_format._root_etag_string (root + CheckBasics + the RIGHT
        # instance's tagged CheckItems/Barriers - see compute_etag_timestamp_ms)
        # instead of the draft branch reading only the draft row's own
        # LastChangedAt, which never moved when only a child row was edited/
        # added/deleted during the draft session.
        etag = odata_format._root_etag_string(root_id) if is_active else odata_format._root_etag_string(root_id, root_override=row)
        with_text["__metadata"] = odata_format._checkroot_meta(active_uuid, draft_uuid, etag)
        return with_text
    elif entity_set == "CheckBasics":
        with_text = resolve_check_basic(row)
        etag = with_text.get("LastChangedAt")
    elif entity_set == "CheckItems":
        with_text = resolve_check_item(row)
        etag = with_text.get("LastChangedAt")
    elif entity_set == "Barriers":
        with_text = resolve_barrier(row)
        etag = with_text.get("LastChangedAt")
    else:
        with_text = dict(row)
        etag = None
    with_text["__metadata"] = odata_format.meta_for(entity_set, entity_type, key_parts, etag)
    return with_text


def _resolve_sublocation_names(root_name):
    locations = config.REFERENCE_DATA.get("Locations", [])
    root = next((l for l in locations if l.get("LocationName") == root_name), None)
    if not root:
        return {root_name}
    names = {root_name}
    frontier = [root["LocationUuid"]]
    while frontier:
        next_frontier = []
        for parent_uuid in frontier:
            for loc in locations:
                if loc.get("ParentLocationUuid") == parent_uuid:
                    names.add(loc["LocationName"])
                    next_frontier.append(loc["LocationUuid"])
        frontier = next_frontier
    return names


def _rewrite_with_sublocation_tokens(tokens):
    if not any(re.match(r"with_sub\s+eq\s+'?true'?", t, re.I) for t in tokens):
        return tokens
    out = []
    for t in tokens:
        if re.match(r"with_sub\s+eq\s+'?true'?", t, re.I):
            continue
        m = re.match(r"substringof\('([^']*)'\s*,\s*LocationName\)", t, re.I) \
            or re.match(r"LocationName\s+eq\s+'([^']*)'", t, re.I)
        out.append("location_name_insub('%s')" % m.group(1) if m else t)
    return out


def compare_dates(row_val, filter_val, op):
    if row_val is None or row_val == "":
        return op == "ge"
    row_time = odata_format.parse_odata_date(row_val)
    filter_time = odata_format.parse_odata_date(filter_val)
    if row_time is None or filter_time is None:
        return True
    if op == "le":
        return row_time <= filter_time
    if op == "ge":
        return row_time >= filter_time
    return True


def eval_one_filter(row, tok):
    m = re.match(r"location_name_insub\('([^']*)'\)", tok)
    if m:
        return str(row.get("LocationName") or "") in _resolve_sublocation_names(m.group(1))

    m = re.match(r"substringof\('([^']*)'\s*,\s*(\w+)\)", tok, re.I)
    if m:
        return m[1].lower() in str(row.get(m[2]) or "").lower()
    m = re.match(r"tolower\((\w+)\)\s+eq\s+'([^']*)'", tok, re.I)
    if m:
        return str(row.get(m[1]) or "").lower() == m[2].lower()
    m = re.match(r"(\w+)\s+le\s+(datetime'[^']+'|/Date\(\d+\)/|'[^']*')", tok, re.I)
    if m:
        return compare_dates(row.get(m[1]), m[2], "le")
    m = re.match(r"(\w+)\s+ge\s+(datetime'[^']+'|/Date\(\d+\)/|'[^']*')", tok, re.I)
    if m:
        return compare_dates(row.get(m[1]), m[2], "ge")
    m = re.match(r"(PkLevels)\s+eq\s+'([^']*)'", tok, re.I)
    if m:
        csv = [s.strip() for s in str(row.get("PkLevels") or "").split(",")]
        return m[2] in csv
    # [Fix] Field names widened from \w+ to [\w/]+ below (and two new
    # patterns added) so nav-property-shaped filter fields work - e.g. the
    # standard EditState quick filter (see annotations.xml,
    # query._augment_edit_state_fields) sends "SiblingEntity/IsActiveEntity eq
    # null" and "DraftAdministrativeData/InProcessByUser ne ''". Those aren't
    # real navigations here - _augment_edit_state_fields precomputes them as
    # plain (if slash-containing) dict keys on each row, so a normal row.get()
    # already resolves them correctly.
    m = re.match(r"([\w/]+)\s+eq\s+(true|false)", tok, re.I)
    if m:
        return bool(row.get(m[1])) == (m[2].lower() == "true")
    m = re.match(r"([\w/]+)\s+ne\s+'([^']*)'", tok, re.I)
    if m:
        return str(row.get(m[1]) or "") != m[2]
    m = re.match(r"([\w/]+)\s+eq\s+'([^']*)'", tok)
    if m:
        return str(row.get(m[1]) or "") == m[2]
    m = re.match(r"([\w/]+)\s+eq\s+null", tok, re.I)
    if m:
        return row.get(m[1]) is None
    return False


def apply_filter(data, s_filter, search):
    result = data
    if s_filter:
        # [Fix] Top-level "or" support - the standard EditState quick filter
        # (see annotations.xml Common.DraftRoot/SemanticKey,
        # query._augment_edit_state_fields) sends compound expressions like
        # "IsActiveEntity eq false or SiblingEntity/IsActiveEntity eq null".
        # OData "and" binds tighter than "or" and none of our filters nest
        # parens, so splitting on "or" first, then "and" within each branch,
        # is a correct (if partial) precedence match for every filter this
        # mock actually needs to understand.
        or_branches = re.split(r'\s+or\s+', s_filter, flags=re.I)

        def matches(row):
            for branch in or_branches:
                tokens = re.split(r'\s+and\s+', branch, flags=re.I)
                tokens = _rewrite_with_sublocation_tokens(tokens)
                if all(eval_one_filter(row, tok) for tok in tokens):
                    return True
            return False

        result = [row for row in result if matches(row)]
    if search:
        needle = search.lower()
        result = [
            row for row in result
            if any(needle in str(v if v is not None else "").lower() for v in row.values())
        ]
    return result


def apply_select(data, s_select):
    if not s_select:
        return data
    fields = [f.strip() for f in s_select.split(",")]
    return [{f: row.get(f) for f in fields} for row in data]


def apply_orderby(data, s_orderby):
    if not s_orderby:
        return data
    specs = []
    for part in s_orderby.split(","):
        pieces = part.strip().split()
        field = pieces[0]
        direction = (pieces[1] if len(pieces) > 1 else "asc").lower()
        specs.append((field, direction == "desc"))

    def compare_rows(a, b):
        for field, desc in specs:
            va = str(a.get(field) or "")
            vb = str(b.get(field) or "")
            if va < vb:
                return 1 if desc else -1
            if va > vb:
                return -1 if desc else 1
        return 0

    return sorted(data, key=cmp_to_key(compare_rows))


def _draft_admin_data_row(draft_uuid, requesting_user=config.MOCK_USER):
    """[Fix] DraftAdministrativeData was declared in metadata.xml (EntityType,
    EntitySet, Association, and the NavigationProperty on CheckRoot) but had
    zero backend implementation - any live fetch (direct nav GET or
    $expand=DraftAdministrativeData) 404'd or silently omitted the property,
    so the standard Fiori Elements "being edited by X" object-page banner
    could never work. Only meaningful for an actual draft (an active row's
    own DraftUUID is always ZERO_GUID, so its nav naturally resolves to
    nothing for the DRAFT'S OWN key - see apply_expand's active-row fallback,
    which routes through state._find_draft_by_active_uuid instead, mirroring
    how _sibling_row already handles the same asymmetry).

    [Fix] InProcessByUser/InProcessByUserDescription now come from the DRAFT
    ROW's own InProcessByUser (stamped by draft_service._draft_prepare at
    creation time), not a hardcoded constant - and DraftIsCreatedByMe/
    DraftIsLastChangedByMe/DraftIsProcessedByMe are computed by comparing
    against requesting_user instead of being permanently True. This is what
    makes the standard "locked by <someone else>" banner/message able to say
    something true now that requesting_user can actually differ per request
    (X-Mock-User)."""
    draft_row = state.draft_store["CheckRoots"].get(draft_uuid)
    if draft_row is None:
        return None
    created_at = draft_row.get("_draft_created_at") or draft_row.get("CreatedAt")
    changed_at = draft_row.get("LastChangedAt")
    created_by = draft_row.get("CreatedByUser", config.MOCK_USER)
    in_process_by = draft_row.get("InProcessByUser", config.MOCK_USER)
    return {
        "DraftUUID": draft_uuid,
        "CreationDateTime": created_at,
        "CreatedByUser": created_by,
        "CreatedByUserDescription": state._mock_user_display_name(created_by),
        "LastChangeDateTime": changed_at,
        "LastChangedByUser": in_process_by,
        "LastChangedByUserDescription": state._mock_user_display_name(in_process_by),
        "InProcessByUser": in_process_by,
        "InProcessByUserDescription": state._mock_user_display_name(in_process_by),
        "DraftIsCreatedByMe": created_by == requesting_user,
        "DraftIsLastChangedByMe": in_process_by == requesting_user,
        "DraftIsProcessedByMe": in_process_by == requesting_user,
    }


def _sibling_row(active_uuid, draft_uuid):
    """[Fix] SiblingEntity was declared in metadata.xml (self-referencing
    Association + NavigationProperty on CheckRoot) but had zero backend
    implementation. Returns the wrap_entity-ready raw row for "the other
    half of the active/draft pair", or None if there is none:
      - addressed via the ACTIVE side (draft_uuid == ZERO_GUID): the sibling
        is this active row's own in-progress edit-draft, if any.
      - addressed via a DRAFT (draft_uuid != ZERO_GUID): the sibling is the
        active row this draft belongs to, if it has one yet (a create-draft
        has no active twin)."""
    if draft_uuid == config.ZERO_GUID:
        _sibling_draft_uuid, sibling_draft_row = state._find_draft_by_active_uuid(active_uuid)
        return sibling_draft_row
    draft_row = state.draft_store["CheckRoots"].get(draft_uuid)
    if draft_row is None:
        return None
    sibling_active_uuid = draft_row.get("ActiveUUID", config.ZERO_GUID)
    if sibling_active_uuid == config.ZERO_GUID:
        return None
    return state.store["CheckRoots"].get(sibling_active_uuid)


def apply_expand(row, entity_set, s_expand, requesting_user=config.MOCK_USER):
    if not s_expand:
        return row
    out = dict(row)
    root_id = row.get("RootId")
    for nav in [e.strip() for e in s_expand.split(",")]:
        if entity_set != "CheckRoots":
            continue
        if nav == "to_Basic":
            basic = state.store["CheckBasics"].get(root_id)
            out["to_Basic"] = wrap_entity("CheckBasics", config.ENTITY_TYPES["CheckBasics"], basic, {"RootId": root_id}) if basic else None
        elif nav == "to_Checks":
            # [Fix] CheckItems are real draft nodes now - only expand THIS
            # row's own instance (row["DraftUUID"] - ZERO_GUID for active,
            # the specific draft's uuid otherwise), see state._checks_of.
            instance_tag = row.get("DraftUUID", config.ZERO_GUID)
            rows = [
                wrap_entity("CheckItems", config.ENTITY_TYPES["CheckItems"], r, {"RootId": rid, "ItemId": iid})
                for (rid, iid), r in state.store["CheckItems"].items()
                if rid == root_id and r.get("DraftUUID", config.ZERO_GUID) == instance_tag
            ]
            out["to_Checks"] = {"results": rows, "__count": str(len(rows))}
        elif nav == "to_Barriers":
            instance_tag = row.get("DraftUUID", config.ZERO_GUID)
            rows = [
                wrap_entity("Barriers", config.ENTITY_TYPES["Barriers"], r, {"RootId": rid, "BarrierId": bid})
                for (rid, bid), r in state.store["Barriers"].items()
                if rid == root_id and r.get("DraftUUID", config.ZERO_GUID) == instance_tag
            ]
            out["to_Barriers"] = {"results": rows, "__count": str(len(rows))}
        elif nav == "DraftAdministrativeData":
            # [Fix] see _draft_admin_data_row. A draft-addressed row expands
            # its OWN DraftUUID directly. An ACTIVE row's own DraftUUID is
            # always the ZERO_GUID stub (no draft_uuid column of its own -
            # see metadata.xml CheckRoot comment), so naively reading
            # row["DraftUUID"] here would ALWAYS resolve to nothing, even
            # when this active row genuinely HasDraftEntity - this is exactly
            # the gap that made CRUDManager.checkForForeignUserLock's
            # $expand=DraftAdministrativeData read (issued against the ACTIVE
            # row's own binding context, per the real editEntity flow) always
            # come back empty, so the friendly "locked by X" message could
            # never render. Mirrors _sibling_row's identical special-case for
            # SiblingEntity: if this row is active, look up its current
            # edit-draft (if any) by RootId instead of by its own key field.
            draft_uuid = row.get("DraftUUID")
            if (not draft_uuid or draft_uuid == config.ZERO_GUID) and row.get("IsActiveEntity", True):
                draft_uuid, _draft_row = state._find_draft_by_active_uuid(row.get("ActiveUUID", root_id))
            admin_row = _draft_admin_data_row(draft_uuid, requesting_user) if draft_uuid and draft_uuid != config.ZERO_GUID else None
            out["DraftAdministrativeData"] = (
                wrap_entity("DraftAdministrativeData", config.ENTITY_TYPES["DraftAdministrativeData"], admin_row, {"DraftUUID": draft_uuid})
                if admin_row else None
            )
        elif nav == "SiblingEntity":
            # [Fix] see _sibling_row.
            sibling_row = _sibling_row(row.get("ActiveUUID", config.ZERO_GUID), row.get("DraftUUID", config.ZERO_GUID))
            out["SiblingEntity"] = wrap_entity("CheckRoots", config.ENTITY_TYPES["CheckRoots"], sibling_row, None) if sibling_row else None
    return out
