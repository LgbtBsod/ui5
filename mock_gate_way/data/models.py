from pydantic import BaseModel
from datetime import datetime


class Checklist(BaseModel):
    id: str
    status: str
    successRateChecks: int
    successRateBarriers: int
    last_modified: datetime


class BasicInfo(BaseModel):
    id: str
    checklist_id: str
    date: str
    equipment: str
    LPC_TEXT: str
    OBSERVER_FULLNAME: str
    OBSERVED_FULLNAME: str
    LOCATION_NAME: str
    LOCATION_TEXT: str


class CheckItem(BaseModel):
    id: str
    checklist_id: str
    selected: bool
    text: str
    result: bool


class BarrierItem(BaseModel):
    id: str
    checklist_id: str
    selected: bool
    text: str
    result: bool