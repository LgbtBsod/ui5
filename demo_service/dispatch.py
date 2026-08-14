"""dispatch_request for ZDEMO_SRV - the one mandatory member of the
gateway's plugin contract (see gateway/registry.py's docstring).

[Fix, toolkit pass] Now driven entirely by toolkit.crud - proves
toolkit/crud.py's generic engine genuinely works for a service that isn't
backend/, and (unlike before this pass) no longer imports anything from
backend/ at all: SINGLE_KEY_RE/ENTITY_SET_RE come from toolkit.patterns
directly, not backend.patterns. Still deliberately tiny: one entity set
(Widgets), no drafts, no batch, no CSRF - just enough to prove a second,
independently-implemented service can register and serve real requests
without touching backend/'s own state, patterns, or handlers.
"""
from __future__ import annotations

from typing import Any, Optional, Tuple

from toolkit.crud import EntityConfig, dispatch_request as _crud_dispatch

from . import state

_WIDGETS = EntityConfig(
    entity_set="Widgets",
    entity_type="ZDemoService.Widget",
    key_props=("Id",),
    url_prefix="/sap/opu/odata/sap/ZDEMO_SRV",
    id_factory=state.draw_id,
)
_CONFIGS = {"Widgets": _WIDGETS}


def dispatch_request(
    method: str,
    rel_url: str,
    body: Optional[dict] = None,
    headers: Optional[dict] = None
) -> Tuple[int, Any, str]:
    return _crud_dispatch(_CONFIGS, state.store, method, rel_url, body=body, headers=headers)
