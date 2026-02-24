def apply_paging(items, top=None, skip=None):
    if skip:
        items = items[skip:]
    if top:
        items = items[:top]
    return items