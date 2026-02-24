RELATIONS = [
    {
        "principal": "Checklist",
        "dependent": "BasicInfo",
        "principal_key": "id",
        "dependent_key": "checklist_id",
        "nav_principal": "to_basic",
        "multiplicity_principal": "1",
        "multiplicity_dependent": "1"
    },
    {
        "principal": "Checklist",
        "dependent": "CheckItem",
        "principal_key": "id",
        "dependent_key": "checklist_id",
        "nav_principal": "to_checks",
        "multiplicity_principal": "1",
        "multiplicity_dependent": "*"
    },
    {
        "principal": "Checklist",
        "dependent": "BarrierItem",
        "principal_key": "id",
        "dependent_key": "checklist_id",
        "nav_principal": "to_barriers",
        "multiplicity_principal": "1",
        "multiplicity_dependent": "*"
    }
]