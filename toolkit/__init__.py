"""Reusable, service-agnostic toolkit for building OData v2 mock services on
top of the multi-service gateway (see gateway/registry.py). Two independent
pieces:

    toolkit.patterns  - the two genuinely entity-model-agnostic URL regexes
                         (single-key entity, bare entity-set list).
    toolkit.crud       - generic non-draft list/get/create/update/delete,
                         config-driven (EntityConfig), no draft awareness.
    toolkit.draft       - generic BOPF/Fiori-Elements draft-protocol state
                         machine (Prepare/Activate/Discard + composition-
                         child clone/re-tag/purge), config-driven
                         (DraftRootConfig/ChildEntityConfig).

Nothing in this package imports from backend/ or any concrete service -
that dependency direction only ever runs the other way (backend/,
demo_service/, and any future service import FROM toolkit). See
C:\\Users\\lgbtb\\.claude\\plans\\rosy-swinging-nest.md for the design
rationale and the adversarial review that shaped this boundary.
"""
from .patterns import SINGLE_KEY_RE, ENTITY_SET_RE
from .crud import EntityConfig, dispatch_request as crud_dispatch_request
from .draft import ChildEntityConfig, DraftRootConfig, DraftEngine

__all__ = [
    "SINGLE_KEY_RE",
    "ENTITY_SET_RE",
    "EntityConfig",
    "crud_dispatch_request",
    "ChildEntityConfig",
    "DraftRootConfig",
    "DraftEngine",
]
