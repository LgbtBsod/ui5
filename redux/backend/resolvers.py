"""Read-only resolution/formatting layer: turns raw store rows into their
OData wire shape. Everything here is a pure function of (config, state) plus
its own arguments - no mutation of store/draft_store happens in this module.
Depends on config.py, state.py, odata_format.py only (draft_service.py
depends on this module, not the other way around, to avoid an import cycle
between "read a draft's admin data" and "mutate a draft").
"""
import re
import functools

import serve_config

from . import config
from . import state
from . import odata_format


def _ref_lookup(set_name, code_field, text_field, code_value):
    for row in config.REFERENCE_DATA.get(set_name, []):
        if str(row.get(code_field)) == str(code_value):
            return row.get(text_field)
    return None


def resolve_check_basic(row):
    """Пуш-даун резолва текстов на CheckBasics — аналог ZI_CheckBasic (coalesce
    Person.Fio / *_IntegrationName, LpcText/ProfText/TimezoneText/LocationName)."""
    out = dict(row)

    observer_fio = _ref_lookup("Persons", "Pernr", "Fio", out.get("ObserverPerner")) if out.get("ObserverPerner") else None
    out["ObserverFullname"] = observer_fio or out.get("ObserverIntegrationName") or ""

    observed_fio = _ref_lookup("Persons", "Pernr", "Fio", out.get("ObservedPerner")) if out.get("ObservedPerner") else None
    out["ObservedFullname"] = observed_fio or out.get("ObservedIntegrationName") or ""

    out["LpcText"] = _ref_lookup("PkLevels", "PkLevel", "PkLevelText", out.get("LpcKey")) or "" if out.get("LpcKey") else ""
    out["ProfText"] = _ref_lookup("Professions", "ProfessionCode", "ProfessionText", out.get("ProfKey")) or "" if out.get("ProfKey") else ""
    out["TimezoneText"] = _ref_lookup("TimeZones", "TimeZoneCode", "TimeZoneText", out.get("Timezone")) or "" if out.get("Timezone") else ""

    if out.get("LocationKey"):
        resolved_name = _ref_lookup("Locations", "LocationUuid", "LocationName", out["LocationKey"])
        if resolved_name:
            out["LocationName"] = resolved_name

    return out


def _resolve_type_and_result(row, type_set, type_code_field, type_text_field, raw_text_fallback=False):
    """Общая часть resolve_check_item/resolve_barrier: Code->Text по
    справочнику типа + Result->ResultText по CheckResults. raw_text_fallback
    сохраняет поведение resolve_check_item (RawText для строк из интеграции
    без сопоставления в CheckTypes) — resolve_barrier такого fallback не имеет."""
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
    return out


def resolve_check_item(row):
    return _resolve_type_and_result(row, "CheckTypes", "CheckTypeCode", "CheckTypeText", raw_text_fallback=True)


def resolve_barrier(row):
    return _resolve_type_and_result(row, "BarrierTypes", "BarrierTypeCode", "BarrierTypeText")


def _rate_to_criticality(rate, total):
    if not total:
        return 3
    if rate >= 90:
        return 3
    if rate >= 70:
        return 2
    return 1


def compute_check_root_view(root_id, root_override=None):
    """[W1] Пуш-даун KPI/criticality/hidden/HeaderKpi* — аналог ZI_CheckRoot
    (push-down на HANA в реальной CDS), здесь пересчитывается на каждое
    чтение из store, а не хранится денормализованно.

    root_override — [Фаза 3] позволяет вызвать эту же вычислительную функцию
    для черновика (draft_store["CheckRoots"][root_id]) вместо активной
    записи; CheckBasics по-прежнему резолвится от активного root_id (root-
    scoped proxy-поля, не draft-узел). [Fix] CheckItems/Barriers ТЕПЕРЬ
    настоящие draft-узлы (Common.DraftNode) - резолвятся от instance_tag
    (draft_row['DraftUUID'] при просмотре черновика, иначе ZERO_GUID/active)
    вместо всегда-активных таблиц, см. state._checks_of/_barriers_of."""
    root = root_override if root_override is not None else state.store["CheckRoots"].get(root_id)
    if root is None:
        return None
    instance_tag = root.get("DraftUUID", config.ZERO_GUID) if root_override is not None else config.ZERO_GUID
    basic = state.store["CheckBasics"].get(root_id)
    checks = state._checks_of(root_id, instance_tag)
    barriers = state._barriers_of(root_id, instance_tag)

    checks_amount = len(checks)
    checks_success = sum(1 for c in checks if c.get("Result") == serve_config.RESULT_CODE_SATISFACTORY)
    has_error_checks = any(c.get("Result") == "" for c in checks)

    barriers_amount = len(barriers)
    barriers_success = sum(1 for b in barriers if b.get("Result") == serve_config.RESULT_CODE_SATISFACTORY)
    has_error_barriers = any(b.get("Result") == "" for b in barriers)

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

    out["ChecksHidden"] = (lpc_key == serve_config.BUSINESS_RULES.CHECKS_HIDDEN_PK_LEVEL)
    out["BarriersHidden"] = lpc_key in serve_config.BUSINESS_RULES.BARRIERS_HIDDEN_PK_LEVELS

    out["ThisIsIntegrationData"] = bool(out.get("ThisIsIntegrationData", False))

    out["IntegrationBadgeHidden"] = not out["ThisIsIntegrationData"]
    out["ChecksErrorBadgeHidden"] = not has_error_checks
    out["BarriersErrorBadgeHidden"] = not has_error_barriers
    return out


def wrap_entity(entity_set, entity_type, row, key_parts):
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

    return sorted(data, key=functools.cmp_to_key(compare_rows))


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
