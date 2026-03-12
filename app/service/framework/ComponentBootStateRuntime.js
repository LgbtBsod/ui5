sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/util/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/util/runtime/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentBootContracts"
], function (ModelStateRuntime, CloneUtil, WorkflowTelemetry, FrontendConfigConstants, ComponentBootContracts) {
    "use strict";

    var PATHS = ComponentBootContracts.PATHS;
    var READINESS_STATUS = ComponentBootContracts.READINESS_STATUS;
    var CONFIG_SOURCE = ComponentBootContracts.FRONTEND_CONFIG_SOURCE;

    function initializeBootState(oStateModel) {
        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/isLoading": true,
            "/masterDataLoading": false,
            "/locationsLoading": false,
            "/readiness/app": {
                status: READINESS_STATUS.LOADING,
                ready: false,
                readyAt: "",
                error: ""
            }
        });
    }

    function seedFrontendState(oStateModel, oEnvModel) {
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.CURRENT_USER, {
            fullName: "",
            permissions: [],
            permissionRules: [],
            canView: false,
            canEdit: false,
            canDelete: false,
            summaryText: "",
            fetchedAt: ""
        });
        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/requiredFields": [],
            "/frontendVariables": Object.assign({}, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES),
            "/frontendConfigSource": CONFIG_SOURCE.GATEWAY
        });
        ModelStateRuntime.writeOnModel(oEnvModel, "/variables", Object.assign({}, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES));
    }

    function finalizeBootSuccess(mOptions) {
        var oStateModel = mOptions.stateModel;
        var oCacheModel = mOptions.cacheModel;
        var sCacheAt = mOptions.cacheAt;
        var sReadyAt = mOptions.readyAt;
        var sTabSessionId = mOptions.tabSessionId;
        var oServerState = mOptions.serverState;
        var aCheckLists = mOptions.checkLists || [];

        ModelStateRuntime.writeOnModel(oCacheModel, "/pristineSnapshot", CloneUtil.clone(aCheckLists, []));
        ModelStateRuntime.setManyOnModel(oCacheModel, {
            "/lastServerState": oServerState || {
                fetchedAt: sCacheAt,
                count: aCheckLists.length
            },
            "/keyMapping": {}
        });
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.CACHE_VALIDATION_AT, sCacheAt);
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.READINESS_APP, {
            status: READINESS_STATUS.READY,
            ready: true,
            readyAt: sReadyAt,
            error: ""
        });
        WorkflowTelemetry.emit("boot.readiness.ready", {
            stateModel: oStateModel,
            payload: {
                readyAt: sReadyAt,
                activeTabSessionId: sTabSessionId
            }
        });
    }

    function finalizeBootError(oStateModel, sErrorMessage, fnBundleText, sTabSessionId) {
        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/loadError": true,
            "/loadErrorMessage": fnBundleText("loadErrorMessage") + ": " + sErrorMessage
        });
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.READINESS_APP, {
            status: READINESS_STATUS.ERROR,
            ready: false,
            readyAt: "",
            error: sErrorMessage
        });
        WorkflowTelemetry.emit("boot.readiness.error", {
            stateModel: oStateModel,
            payload: {
                error: sErrorMessage,
                activeTabSessionId: sTabSessionId
            }
        });
    }

    return {
        finalizeBootError: finalizeBootError,
        finalizeBootSuccess: finalizeBootSuccess,
        initializeBootState: initializeBootState,
        seedFrontendState: seedFrontendState
    };
});
