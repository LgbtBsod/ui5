"""Independent, process-lifetime in-memory store for ZDEMO_SRV - a SEPARATE
Python module-level dict from backend.state.store, by construction (two
different modules naturally have two different global namespaces). This is
the whole isolation mechanism: no explicit namespacing/swapping needed, no
shared mutable state to accidentally collide with backend/'s own store.
"""
from typing import Any, Dict

# store["Widgets"][Id] -> dict
store: Dict[str, Dict[str, dict]] = {"Widgets": {}}

_next_id = [1]


def draw_id() -> str:
    widget_id = "W%03d" % _next_id[0]
    _next_id[0] += 1
    return widget_id
