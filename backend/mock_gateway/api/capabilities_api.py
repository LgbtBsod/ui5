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
            "batch": "/$batch",
            "saveFunctionImport": "/actions/SaveChecklist",
            "autosaveFunctionImport": "/actions/AutoSaveChecklist",
            "lockStatusEntity": "/LockStatusSet",
            "lockHistoryEntity": "/LockLogs",
            "referenceBundle": "/reference/bundle",
            "searchRowsEntity": "/SearchRows",
        },
        "source": "gateway_capabilities"
    }
