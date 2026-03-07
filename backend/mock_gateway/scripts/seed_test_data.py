"""Manual test data seeder for local frontend integration.

Usage:
    python mock_gate_way/scripts/seed_test_data.py
"""

from __future__ import annotations

import random
import string
import sys
from datetime import date
from pathlib import Path

from sqlalchemy import text

PROJECT_DIR = Path(__file__).resolve().parents[1]
if str(PROJECT_DIR) not in sys.path:
    sys.path.insert(0, str(PROJECT_DIR))

from database import Base, SessionLocal, engine
from models import Person

FIRST_NAMES = [
    "Ivan", "Petr", "Anna", "Elena", "Olga", "Nikolay", "Sergey", "Maria", "Daria", "Alexey",
]
LAST_NAMES = [
    "Ivanov", "Petrov", "Smirnov", "Kuznetsov", "Popov", "Vasiliev", "Sokolov", "Mikhailov", "Fedorov", "Morozov",
]
MIDDLE_NAMES = [
    "Ivanovich", "Petrovich", "Sergeevich", "Alexandrovich", "Nikolaevich", "Andreevna", "Petrovna", "Ivanovna",
]
POSITIONS = [
    "Operator", "Technician", "Foreman", "Engineer", "Supervisor", "Shift Lead",
]
ORG_UNITS = [
    "MPL-01", "MPL-02", "MPL-03", "MPL-04", "MPL-05",
]


def _random_perner(existing: set[str]) -> str:
    while True:
        value = "".join(random.choices(string.digits, k=8))
        if value not in existing:
            return value


def seed_persons(count: int = 30) -> int:
    db = SessionLocal()
    try:
        existing_perners = {p[0] for p in db.query(Person.perner).all()}
        created = 0

        for _ in range(count):
            perner = _random_perner(existing_perners)
            existing_perners.add(perner)

            db.add(
                Person(
                    perner=perner,
                    first_name=random.choice(FIRST_NAMES),
                    last_name=random.choice(LAST_NAMES),
                    middle_name=random.choice(MIDDLE_NAMES),
                    position=random.choice(POSITIONS),
                    org_unit=random.choice(ORG_UNITS),
                    integration_name=f"INT_{perner}",
                    begda=date(2020, 1, 1),
                    endda=None,
                )
            )
            created += 1

        db.commit()
        return created
    finally:
        db.close()


def seed_locations() -> int:
    """Create minimal MPL hierarchy if table is empty.

    Table is intentionally managed with raw SQL because current location service
    reads directly from `locations` and no ORM model exists yet.
    """
    db = SessionLocal()
    try:
        db.execute(
            text(
                """
                CREATE TABLE IF NOT EXISTS locations (
                    node_id TEXT PRIMARY KEY,
                    parent_id TEXT NULL,
                    location_name TEXT NOT NULL,
                    begda DATE NOT NULL,
                    endda DATE NULL
                )
                """
            )
        )

        count = db.execute(text("SELECT COUNT(*) FROM locations")).scalar() or 0
        if count > 0:
            db.commit()
            return 0

        rows = [
            ("MPL-ROOT", None, "MPL Root", "2020-01-01", None),
            ("MPL-PLANT-1", "MPL-ROOT", "Plant 1", "2020-01-01", None),
            ("MPL-PLANT-2", "MPL-ROOT", "Plant 2", "2020-01-01", None),
            ("MPL-ZONE-1A", "MPL-PLANT-1", "Zone 1A", "2020-01-01", None),
            ("MPL-ZONE-1B", "MPL-PLANT-1", "Zone 1B", "2020-01-01", None),
            ("MPL-ZONE-2A", "MPL-PLANT-2", "Zone 2A", "2020-01-01", None),
        ]

        for node_id, parent_id, location_name, begda, endda in rows:
            db.execute(
                text(
                    """
                    INSERT INTO locations (node_id, parent_id, location_name, begda, endda)
                    VALUES (:node_id, :parent_id, :location_name, :begda, :endda)
                    """
                ),
                {
                    "node_id": node_id,
                    "parent_id": parent_id,
                    "location_name": location_name,
                    "begda": begda,
                    "endda": endda,
                },
            )

        db.commit()
        return len(rows)
    finally:
        db.close()


def main() -> None:
    Base.metadata.create_all(bind=engine)

    created_people = seed_persons(30)
    created_locations = seed_locations()

    print(f"People added: {created_people}")
    print(f"Locations added: {created_locations}")


if __name__ == "__main__":
    main()
