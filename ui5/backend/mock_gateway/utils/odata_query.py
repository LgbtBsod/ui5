from __future__ import annotations


def parse_paging(top: int | None, skip: int | None, default_top: int = 50) -> tuple[int, int]:
    t = int(top if top is not None else default_top)
    s = int(skip if skip is not None else 0)
    return max(0, t), max(0, s)


def with_inlinecount(inlinecount: str | None, total: int) -> int | None:
    return total if str(inlinecount or "").lower() == "allpages" else None
