from config import ALLOW_MOCK_USER_HEADER
from fastapi import APIRouter

router = APIRouter(tags=["Capabilities"])


@router.get("/capabilities")
def capabilities():
    return {
        "contractVersion": "1.2.0",
        "backendMode": "real",
        "features": {
            "lockStatus": True,
            "lockHeartbeat": True,
            "autoSave": True,
            "processAnalytics": True,
            "dictionaryLookup": True,
            "personSuggestion": True,
            "locationsHierarchy": True,
            "exportReport": True,
            "batch": True,
            "saveViaFunctionImport": True,
            "autosaveViaFunctionImport": True,
            "lockHistoryEntity": True,
            "requestIdempotency": True,
            "referenceBundle": True,
            "featureFlagsEndpoint": True,
            "optimizedSearchEntity": True,
        },
        "compatibility": {
            "minUiContractVersion": "1.0.0",
            "maxUiContractVersion": "1.x"
        },
        "paths": {
            "serviceRoot": "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV",
            "batch": "/$batch",
            "saveFunctionImport": "/SaveChanges",
            "autosaveFunctionImport": "/AutoSave",
            "lockStatusEntity": "/LockStatusSet",
            "lockHistoryEntity": "/LockLogs",
            "referenceBundle": "/reference/bundle",
            "searchRowsEntity": "/SearchRows",
        },
        "identityMode": "mock_header" if ALLOW_MOCK_USER_HEADER else "runtime_user_context",
        "source": "gateway_capabilities"
    }
