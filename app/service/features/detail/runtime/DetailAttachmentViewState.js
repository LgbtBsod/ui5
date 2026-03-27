sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (ModelContracts, StatePaths, ControllerViewStateRuntime, ModelStateRuntime, ModelPathContracts, WorkflowContracts, DetailUseCaseConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function getModel(oController, sName) {
        return oController && oController.getModel ? oController.getModel(sName) : null;
    }

    function resolveActiveDbKey(oController) {
        var oStateModel = getModel(oController, MODELS.STATE);
        var oDetailModel = getModel(oController, MODELS.DETAIL);
        return String(
            ModelStateRuntime.readOnModel(oDetailModel, DETAIL_MODEL_PATHS.ROOT_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "")
            || ""
        ).trim();
    }

    function hasActiveRoot(oController) {
        var oDetailModel = getModel(oController, MODELS.DETAIL);
        var sDetailRootId = String(ModelStateRuntime.readOnModel(oDetailModel, DETAIL_MODEL_PATHS.ROOT_ID, "") || "").trim();
        var sCanonicalRootId = resolveActiveDbKey(oController);
        var aSessionAttachments = ControllerViewStateRuntime.get(oController, "/sessionAttachments", []);
        var aPersistedAttachments = ModelStateRuntime.readOnModel(oDetailModel, DETAIL_MODEL_PATHS.ATTACHMENTS, []);

        return !!(
            sDetailRootId
            || sCanonicalRootId
            || (Array.isArray(aSessionAttachments) && aSessionAttachments.length)
            || (Array.isArray(aPersistedAttachments) && aPersistedAttachments.length)
        );
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
            "/attachmentTableClass": !bExpanded
                ? "flatEditorTable detailAttachmentTable detailAttachmentTableSession"
                : "flatEditorTable detailAttachmentTable",
            "/attachmentDesktopColumnsVisible": !bNarrow,
            "/attachmentActionsColumnWidth": bNarrow ? "9rem" : "14rem"
        });
    }

    return {
        sync: sync
    };
});
