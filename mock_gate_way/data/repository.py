from .generator import generate_checklist

CHECKLISTS = {}
BASICS = {}
CHECKS = {}
BARRIERS = {}

for i in range(1, 201):
    c, b, ch, br = generate_checklist(i)
    CHECKLISTS[c.id] = c
    BASICS[c.id] = b
    CHECKS[c.id] = ch
    BARRIERS[c.id] = br


def get_entity_set(name):
    return {
        "ChecklistSet": CHECKLISTS,
        "BasicInfoSet": BASICS,
        "CheckItemSet": CHECKS,
        "BarrierItemSet": BARRIERS,
    }.get(name)