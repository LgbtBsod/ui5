import logging

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from services.analytics_service import AnalyticsService

router = APIRouter(tags=["Analytics"])
logger = logging.getLogger("gateway.analytics.api")


@router.get("/WorkflowAnalytics")
def workflow_analytics(db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db)
    logger.info("WorkflowAnalytics payload=%s", payload)
    return payload


@router.get("/analytics/process")
def process_analytics(db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db)
    logger.info("analytics/process payload=%s", payload)
    return payload


@router.get("/SimpleAnalytical")
def simple_analytical(db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db)
    logger.info("SimpleAnalytical payload=%s", payload)
    return payload
