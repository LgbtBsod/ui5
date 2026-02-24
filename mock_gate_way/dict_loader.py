import json
from datetime import date

from sqlalchemy.orm import Session

from models import Person


def load_persons(db: Session, path: str) -> int:
    with open(path, "r", encoding="utf-8") as file:
        data = json.load(file)

    created = 0
    for person in data.get("persons", []):
        if db.query(Person).filter(Person.perner == person["perner"]).first():
            continue

        db.add(
            Person(
                perner=person["perner"],
                first_name=person.get("first_name") or person.get("firstName", ""),
                last_name=person.get("last_name") or person.get("lastName", ""),
                middle_name=person.get("middle_name") or person.get("middleName", ""),
                position=person.get("position", ""),
                org_unit=person.get("orgUnit", ""),
                integration_name=person.get("integrationName", ""),
                begda=date.fromisoformat(person.get("begda", "2000-01-01")),
                endda=date.fromisoformat(person["endda"]) if person.get("endda") else None,
            )
        )
        created += 1

    db.commit()
    return created
