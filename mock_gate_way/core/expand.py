from data.repository import BASICS, CHECKS, BARRIERS


def apply_expand(entity_name, item, expand):
    data = item.dict()

    if not expand:
        return data

    expands = [e.strip() for e in expand.split(",")]

    if entity_name == "ChecklistSet":
        if "to_basic" in expands:
            data["to_basic"] = BASICS[item.id].dict()

        if "to_checks" in expands:
            data["to_checks"] = {
                "results": [c.dict() for c in CHECKS[item.id]]
            }

        if "to_barriers" in expands:
            data["to_barriers"] = {
                "results": [b.dict() for b in BARRIERS[item.id]]
            }

    return data