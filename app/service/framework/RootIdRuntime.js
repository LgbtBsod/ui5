sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (ModelStateRuntime, ControllerModelRuntime, CreateSentinel, ModelPathContracts) {
    "use strict";

    function sanitizeId(vId) {
        return String(vId || "").trim();
    }

    function isRealId(vId) {
        var sId = sanitizeId(vId);
        return !!sId && !CreateSentinel.isCreateId(sId);
    }

    function resolveCanonicalId(aCandidates) {
        var i;
        var sCandidate;
        for (i = 0; i < aCandidates.length; i += 1) {
            sCandidate = sanitizeId(aCandidates[i]);
            if (isRealId(sCandidate)) {
                return sCandidate;
            }
        }
        for (i = 0; i < aCandidates.length; i += 1) {
            sCandidate = sanitizeId(aCandidates[i]);
            if (sCandidate) {
                return sCandidate;
            }
        }
        return "";
    }

    function resolveFromStateModel(oStateModel) {
        return resolveCanonicalId([
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, ""),
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, ""),
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, "")
        ]);
    }

    function resolveActiveFromStateModel(oStateModel) {
        return resolveCanonicalId([
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, ""),
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "")
        ]);
    }

    function resolveFromController(oController) {
        var oStateModel = ControllerModelRuntime.state(oController);
        return resolveFromStateModel(oStateModel);
    }

    function resolveCurrentRootId(oController) {
        return sanitizeId((oController && oController._currentRootId && oController._currentRootId()) || "")
            || resolveFromController(oController);
    }

    function withCurrentRootId(oController, mInput) {
        var oInput = Object.assign({}, mInput || {});
        oInput.rootId = resolveCurrentRootId(oController);
        return oInput;
    }

    return {
        resolveCanonicalId: resolveCanonicalId,
        resolveFromStateModel: resolveFromStateModel,
        resolveActiveFromStateModel: resolveActiveFromStateModel,
        resolveFromController: resolveFromController,
        resolveCurrentRootId: resolveCurrentRootId,
        withCurrentRootId: withCurrentRootId
    };
});
