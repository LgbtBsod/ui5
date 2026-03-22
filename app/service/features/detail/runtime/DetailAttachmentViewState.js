sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (ModelContracts, StatePaths, ControllerModelRuntime, ControllerViewStateRuntime, ModelStateRuntime, RootIdRuntime, WorkflowContracts, DetailUseCaseConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function hasActiveRoot(oController) {
        var oDetailModel = ControllerModelRuntime.detail(oController);
        var sDetailRootId = String(ModelStateRuntime.readOnModel(oDetailModel, DETAIL_MODEL_PATHS.ROOT_ID, "") || "").trim();
        var sCanonicalRootId = RootIdRuntime.resolveFromController(oController);
        var aSessionAttachments = ControllerViewStateRuntime.get(oController, "/sessionAttachments", []);

        return !!(sDetailRootId || sCanonicalRootId || (Array.isArray(aSessionAttachments) && aSessionAttachments.length));
    }

    function sync(oController) {
        var sEditMode = WorkflowContracts.normalizeEditMode(
            ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "")
        );
        var bEditable = sEditMode !== WorkflowContracts.EDIT_MODES.READ;
        var bHasRoot = hasActiveRoot(oController);
        var bExpanded = !!ControllerViewStateRuntime.get(oController, "/attachmentsExpanded", false);
        var bNarrow = !!ControllerViewStateRuntime.get(oController, "/narrowDetailViewport", false);

        ControllerViewStateRuntime.setMany(oController, {
            "/attachmentActionsEnabled": bEditable && bHasRoot,
            "/attachmentMetaEditable": bEditable && bHasRoot,
            "/showSessionAttachments": !bExpanded,
            "/attachmentDesktopColumnsVisible": !bNarrow,
            "/attachmentActionsColumnWidth": bNarrow ? "9rem" : "14rem"
        });
    }

    return {
        sync: sync
    };
});
