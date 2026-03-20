sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (ControllerModelRuntime, ControllerViewStateRuntime, ModelStateRuntime, WorkflowContracts) {
    "use strict";

    function hasActiveRoot(oController) {
        var oSelected = ControllerModelRuntime.selected(oController);
        var oViewState = ControllerModelRuntime.viewState(oController);
        var sSelectedRootId = String(ModelStateRuntime.readOnModel(oSelected, "/root/id", "") || "").trim();
        var sActiveObjectId = String(ModelStateRuntime.read(oController, "state", "/activeObjectId", "") || "").trim();
        var sSelectedId = String(ModelStateRuntime.read(oController, "state", "/selectedId", "") || "").trim();
        var aSessionAttachments = ModelStateRuntime.readOnModel(oViewState, "/sessionAttachments", []);

        return !!(sSelectedRootId || sActiveObjectId || sSelectedId || (Array.isArray(aSessionAttachments) && aSessionAttachments.length));
    }

    function sync(oController) {
        var sEditMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.read(oController, "state", "/workflow/detail/editMode", ""));
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
