"""Source of truth for the two URL regexes that are genuinely generic across
ANY OData v2 entity model - not tied to CheckRoots/CheckItems/Barriers or any
other specific entity set naming.

Moved here (out of backend/patterns.py) so that a service wanting only
generic CRUD (toolkit.crud) never has to import backend/ - and therefore
never transitively drags in backend/config.py -> serve_config.py ->
backend/state.py - just to parse a URL. backend/patterns.py's own
`Patterns` class re-exports SINGLE_KEY_RE/ENTITY_SET_RE from here (not the
other way around) so every existing backend/ caller is unaffected.

The rest of backend/patterns.py's regexes (COMPOSITE_KEY_RE,
CHECKROOT_KEY_RE, NAV_*_RE) stay in backend/ - they're hardcoded to
CheckRoots' own nav-property names and composite-key shape, not reusable.
"""
from __future__ import annotations

import re
from typing import Pattern

#: Single key entity: EntityName('key_value')
SINGLE_KEY_RE: Pattern[str] = re.compile(r"^(\w+)\('([^']*)'\)$")

#: Simple entity set list: EntitySet
ENTITY_SET_RE: Pattern[str] = re.compile(r"^(\w+)$")
