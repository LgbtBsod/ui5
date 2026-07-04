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
from services.db_seed import seed_persons, seed_locations, reset_db

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


def main() -> None:
    Base.metadata.create_all(bind=engine)

    created_people = seed_persons(30)
    created_locations = seed_locations()

    print(f"People added: {created_people}")
    print(f"Locations added: {created_locations}")


if __name__ == "__main__":
    main()
