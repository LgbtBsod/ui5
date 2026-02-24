import random
import uuid
from datetime import datetime
from .models import Checklist, BasicInfo, CheckItem, BarrierItem


def generate_checklist(index):
    checklist_id = str(index)

    checklist = Checklist(
        id=checklist_id,
        status=random.choice(["OPEN", "CLOSED"]),
        successRateChecks=random.randint(50, 100),
        successRateBarriers=random.randint(50, 100),
        last_modified=datetime.utcnow()
    )

    basic = BasicInfo(
        id=str(uuid.uuid4()),
        checklist_id=checklist_id,
        date="2026-02-01",
        equipment="Pump A",
        LPC_TEXT=random.choice(["L1", "L2", "L3"]),
        OBSERVER_FULLNAME="John Doe",
        OBSERVED_FULLNAME="Jane Smith",
        LOCATION_NAME="Plant 1",
        LOCATION_TEXT="Zone A"
    )

    checks = [
        CheckItem(
            id=str(uuid.uuid4()),
            checklist_id=checklist_id,
            selected=False,
            text=f"Check {i}",
            result=random.choice([True, False])
        )
        for i in range(120)
    ]

    barriers = [
        BarrierItem(
            id=str(uuid.uuid4()),
            checklist_id=checklist_id,
            selected=False,
            text=f"Barrier {i}",
            result=random.choice([True, False])
        )
        for i in range(80)
    ]

    return checklist, basic, checks, barriers