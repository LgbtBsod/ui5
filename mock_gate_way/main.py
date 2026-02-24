# main.py
from api.checklist_api import router as checklist_router
import asyncio
from database import SessionLocal
from services.lock_service import LockService
from fastapi import FastAPI
from database import engine, Base
from api.lock_api import router as lock_router

app = FastAPI()

Base.metadata.create_all(bind=engine)

app.include_router(lock_router)



app.include_router(checklist_router)

@app.get("/")
def health():
    return {"status": "Gateway Simulator Running"}

    async def lock_cleanup_job():
    while True:
        db = SessionLocal()
        try:
            LockService.cleanup(db)
        finally:
            db.close()
        await asyncio.sleep(30)  # каждые 30 секунд


@app.on_event("startup")
async def startup_event():
    asyncio.create_task(lock_cleanup_job())

@app.on_event("startup")
def startup():

    db = SessionLocal()

    load_dictionary(db, "data/lpc.json", "LPC")
    load_dictionary(db, "data/professions.json", "PROFESSION")

    db.close()