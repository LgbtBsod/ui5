"""OData wire-format helpers: date/etag formatting, key-segment/`__metadata`
building, and the CheckRoots ETag aggregation. Depends on config.py and
state.py (for store reads used by the etag aggregation) only.
"""
import re
from datetime import datetime, timezone
from urllib.parse import quote

import serve_config

from . import config
from . import state


def odata_date(dt=None):
    if dt is None:
        dt = datetime.now(timezone.utc)
    return "/Date(%d)/" % int(dt.timestamp() * 1000)


def parse_odata_date(s):
    if s is None or s == "":
        return None
    s = str(s).strip()
    m = re.match(r'^/Date\((\d+)\)/$', s)
    if m:
        return int(m.group(1))
    m = re.match(r"^datetime'([^']+)'$", s, re.I)
    if m:
        return int(datetime.fromisoformat(m.group(1).replace("Z", "+00:00")).timestamp() * 1000)
    m = re.match(r"^'([^']+)'$", s)
    if m:
        try:
            return int(datetime.fromisoformat(m.group(1).replace("Z", "+00:00")).timestamp() * 1000)
        except:
            pass
    try:
        ts = datetime.fromisoformat(s.replace("Z", "+00:00")).timestamp()
        return int(ts * 1000)
    except:
        pass
    return None


def generate_uuid():
    import uuid
    return str(uuid.uuid4())


def _build_key_segment(entity_set, key_parts):
    """key_parts: dict {PropName: value}. Single-key -> 'val'; composite -> Prop='val',Prop2='val2'."""
    props = config.KEY_PROP_TUPLES.get(entity_set) or (serve_config.REFERENCE_KEY_PROPS.get(entity_set),)
    if len(props) == 1:
        return "'%s'" % quote(str(key_parts[props[0]]), safe="")
    return ",".join("%s='%s'" % (p, quote(str(key_parts[p]), safe="")) for p in props)


def meta_for(entity_set, entity_type, key_parts, etag=None):
    out = {
        "type": entity_type,
        "uri": "%s/%s(%s)" % (config.ODATA_PREFIX, entity_set, _build_key_segment(entity_set, key_parts))
    }
    if etag:
        out["etag"] = 'W/"%s"' % etag
    return out


def _checkroot_meta(active_uuid, draft_uuid, etag):
    """[Фаза 5, NW 7.50/pre-S4 1610] __metadata URI по ActiveUUID/DraftUUID —
    целевая система не поддерживает IsActiveEntity как часть ключа "из
    коробки" (см. localService/metadata.xml, комментарий у EntityType
    CheckRoot). Оба GUID-а всегда присутствуют в URL, один из них — ZERO_GUID."""
    out = {
        "type": "ZCheckService.CheckRoot",
        "uri": "%s/CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')" % (config.ODATA_PREFIX, active_uuid, draft_uuid)
    }
    if etag:
        out["etag"] = 'W/"%s"' % etag
    return out


def compute_etag_timestamp_ms(root_id, root_override=None):
    """[W1] Root.LastChangedAt один не годится в ETag — представление
    CheckRoots(...) включает ВЫЧИСЛЯЕМЫЕ поля из CheckItems/Barriers
    (ChecksAmount/HeaderKpiTitle/...), которые меняются при add/delete
    дочерней строки, даже когда сам Root не PATCH-ится. ETag обязан
    отражать представление целиком — берём max(Root, все дети), как и
    старая модель (compute_etag_timestamp_ms). Basic/CheckItem/Barrier как
    отдельные сущности сохраняют СОБСТВЕННЫЙ независимый ETag (см.
    abap/README.md) — это правило только для составного представления
    самого CheckRoots."""
    root = root_override or state.store["CheckRoots"].get(root_id)
    if not root:
        return config.ETAG_SENTINEL_MS
    # [Fix] CheckItems/Barriers are real draft nodes now - when computing the
    # etag for a DRAFT (root_override given), aggregate over THAT draft's own
    # tagged children, not the active instance's. Without this, editing only
    # a child row while a draft is open (e.g. via inline SmartTable edit)
    # never moved the draft's own etag, so a subsequent legitimate
    # If-Match-guarded PATCH on the root could spuriously succeed/fail
    # against a stale value. CheckBasics stays root-scoped either way.
    instance_tag = root.get("DraftUUID", config.ZERO_GUID) if root_override is not None else config.ZERO_GUID
    candidates = [parse_odata_date(root.get("LastChangedAt") or root.get("CreatedAt"))]
    basic = state.store["CheckBasics"].get(root_id)
    if basic:
        candidates.append(parse_odata_date(basic.get("LastChangedAt")))
    for c in state._checks_of(root_id, instance_tag):
        candidates.append(parse_odata_date(c.get("LastChangedAt")))
    for b in state._barriers_of(root_id, instance_tag):
        candidates.append(parse_odata_date(b.get("LastChangedAt")))
    candidates = [c for c in candidates if c is not None]
    return max(candidates) if candidates else config.ETAG_SENTINEL_MS


def _root_etag_string(root_id, root_override=None):
    """Форматированный ETag CheckRoots(...) как OData date-строка — общее
    выражение, ранее задублированное в wrap_entity и _precondition_failed."""
    return odata_date(datetime.fromtimestamp(compute_etag_timestamp_ms(root_id, root_override) / 1000, tz=timezone.utc))
