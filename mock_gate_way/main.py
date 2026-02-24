import asyncio
import logging
from contextlib import asynccontextmanager
from pathlib import Path

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from api.actions_api import router as actions_router
from api.checklist_api import router as checklist_router
from api.dictionary_api import legacy_router as dictionary_legacy_router
from api.dictionary_api import router as dictionary_router
from api.hierarchy_api import router as hierarchy_router
from api.location_api import router as location_router
from api.lock_api import router as lock_router
from api.metadata_api import router as metadata_router
from api.person_api import router as person_router
from config import LOCK_CLEANUP_INTERVAL_SECONDS
from database import Base, SessionLocal, engine
from services.dict_loader import load_dictionary
from services.lock_service import LockService

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("gateway")


async def lock_cleanup_job() -> None:
    while True:
        db = SessionLocal()
        try:
            cleaned = LockService.cleanup(db)
            if cleaned:
                logger.info("Cleaned %s expired locks", cleaned)
        finally:
            db.close()
        await asyncio.sleep(LOCK_CLEANUP_INTERVAL_SECONDS)


@asynccontextmanager
async def lifespan(_: FastAPI):
    Base.metadata.create_all(bind=engine)

    db = SessionLocal()
    try:
        data_dir = Path(__file__).resolve().parent / "data"
        load_dictionary(db, str(data_dir / "lpc.json"), "LPC")
        load_dictionary(db, str(data_dir / "professions.json"), "PROFESSION")
    finally:
        db.close()

    task = asyncio.create_task(lock_cleanup_job())
    yield
    task.cancel()


app = FastAPI(title="SAP Gateway Simulator", version="1.0.0", lifespan=lifespan)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

app.include_router(lock_router)
app.include_router(checklist_router)
app.include_router(dictionary_router)
app.include_router(dictionary_legacy_router)
app.include_router(location_router)
app.include_router(person_router)
app.include_router(hierarchy_router)
app.include_router(actions_router)
app.include_router(metadata_router)


@app.get("/")
def health():
    return {"status": "Gateway Simulator Running"}
