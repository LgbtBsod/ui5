import re
from fastapi import Request
from data.repository import get_entity_set, CHECKS, BARRIERS
from core.query_options import apply_paging
from core.expand import apply_expand
from core.response import wrap


async def handle_request(request: Request):

    path = request.url.path.strip("/")
    query = request.query_params

    # EntitySet('key')
    match = re.match(r"(\w+)\('(.+)'\)", path)

    if match:
        entity_set = match.group(1)
        key = match.group(2)
        data_store = get_entity_set(entity_set)

        item = data_store.get(key)
        expand = query.get("$expand")

        if item:
            result = apply_expand(entity_set, item, expand)
            return wrap(result)

    # Navigation
    nav_match = re.match(r"(\w+)\('(.+)'\)/(\w+)", path)
    if nav_match:
        entity_set = nav_match.group(1)
        key = nav_match.group(2)
        nav = nav_match.group(3)

        if nav == "to_checks":
            items = CHECKS[key]
        elif nav == "to_barriers":
            items = BARRIERS[key]
        else:
            return wrap({})

        top = int(query.get("$top", 50))
        skip = int(query.get("$skip", 0))

        items = apply_paging(items, top, skip)
        return wrap({"results": [i.dict() for i in items]})

    # EntitySet
    data_store = get_entity_set(path)
    if data_store:
        items = list(data_store.values())

        top = int(query.get("$top", 20))
        skip = int(query.get("$skip", 0))
        expand = query.get("$expand")

        items = apply_paging(items, top, skip)

        results = [
            apply_expand(path, i, expand)
            for i in items
        ]

        return wrap({"results": results})

    return wrap({})