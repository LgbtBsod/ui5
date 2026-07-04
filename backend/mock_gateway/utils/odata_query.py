from __future__ import annotations

from config import MAX_PAGE_SIZE


def parse_paging(top: int | None, skip: int | None, default_top: int = 50) -> tuple[int, int]:
    """Single source of truth for $top/$skip normalization. Previously this cap was only
    ever applied ad hoc at one call site (dictionary_item_set); every other list endpoint
    that went through this function (ChecklistSearchSet, PersonVHSet, etc.) accepted an
    unbounded $top straight from the client - a full-table-scan / memory-exhaustion vector
    on exactly the endpoints most likely to be hit with a large dataset.
    """
    t = int(top if top is not None else default_top)
    s = int(skip if skip is not None else 0)
    return min(max(0, t), MAX_PAGE_SIZE), max(0, s)


def with_inlinecount(inlinecount: str | None, total: int) -> int | None:
    return total if str(inlinecount or "").lower() == "allpages" else None
