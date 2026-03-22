sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPersonInputRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailValidationHelperRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailValidationStateRuntime"
], function (DetailPersonInputRuntime, StatePaths, ControllerViewStateRuntime, LayoutStateRuntime, ModelStateRuntime, ModelContracts, DetailValidationHelperRuntime, DetailValidationStateRuntime) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_MODEL = MODELS.DETAIL;

    function onDetailModelChanged(oController, oEvent, mStatePaths) {
        var oDetailModel = ModelStateRuntime.model(oController, DETAIL_MODEL);
        var sPath = oEvent && oEvent.getParameter && oEvent.getParameter("path");
        var aRequired = ModelStateRuntime.read(oController, STATE_MODEL, "/requiredFields", []) || [];
        var sValidationKey;
        var sRequiredPath;
        var sModelPath;
        var sMode;
        var vCurrent;

        if (!ModelStateRuntime.model(oController, VIEW_MODEL) || !oDetailModel || !sPath) {
            return;
        }

        sModelPath = "/" + String(sPath || "").replace(/^\//, "");
        if (sPath === "/") {
            ControllerViewStateRuntime.set(oController, "/deleteChecklistConfirmArmed", false);
        }
        DetailPersonInputRuntime.syncDrafts(oController, oDetailModel, sModelPath);

        sMode = LayoutStateRuntime.normalizeMode(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, ""), "");
        sRequiredPath = DetailValidationHelperRuntime.normalizeRequiredPath(sModelPath);
        if (DetailValidationHelperRuntime.shouldTrackSelectedDirtyPath(sModelPath) && (sMode === "EDIT" || sMode === "CREATE") && aRequired.indexOf(sRequiredPath) < 0) {
            DetailValidationStateRuntime.recompute(oController, "selectedSync", false, mStatePaths);
            return;
        }

        if (aRequired.indexOf(sRequiredPath) < 0) {
            return;
        }

        sValidationKey = DetailValidationHelperRuntime.toValidationKey(sRequiredPath);
        vCurrent = ModelStateRuntime.read(oController, DETAIL_MODEL, DetailValidationHelperRuntime.toDetailModelPath(sRequiredPath), undefined);
        DetailValidationHelperRuntime.setValidationMissingKey(oController, sValidationKey, !DetailValidationHelperRuntime.isFilledValidationValue(vCurrent));
        DetailValidationStateRuntime.recompute(oController, "fieldChange", false, mStatePaths);
    }

    return {
        onDetailModelChanged: onDetailModelChanged
    };
});
