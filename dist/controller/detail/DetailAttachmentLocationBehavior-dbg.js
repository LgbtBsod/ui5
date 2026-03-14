sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPersonInputRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailValueHelpRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (AttachmentUploadCore, DetailCommandPolicy, DetailPersonInputRuntime, DetailAttachmentRuntime, DetailValueHelpRuntime, ControllerViewStateRuntime, NavigationIntentService, RootIdRuntime, SchedulingRuntime) {
    "use strict";

    function createAttachmentHooks(oController) {
        return {
            attachmentDelete: function (mInput) { return DetailCommandPolicy.attachmentDelete(oController, RootIdRuntime.withCurrentRootId(oController, mInput)); },
            attachmentLoad: function () { return DetailCommandPolicy.attachmentLoad(oController, RootIdRuntime.withCurrentRootId(oController)); },
            scheduleAttachmentDropZoneBind: function () { if (typeof oController._scheduleAttachmentDropZoneBind === "function") { oController._scheduleAttachmentDropZoneBind(); } },
            unbindAttachmentDropZone: function () { if (typeof oController._unbindAttachmentDropZone === "function") { oController._unbindAttachmentDropZone(); } }
        };
    }

    function createValueHelpHooks(oController) {
        return {
            autosave: function (mInput) { return DetailCommandPolicy.autosave(oController, { rootId: oController._currentRootId(), field: mInput.field, value: mInput.value }); },
            clearSearchTimer: function () { oController._clearLocationValueHelpSearchTimer(); },
                consumeSuggestionSelection: function (sTarget, sValue) { return DetailPersonInputRuntime.consumeSuggestionSelection(oController, sTarget, sValue); },
            isEditMode: function () { return oController._isEditMode(); },
            personSuggest: function (mInput) { return DetailCommandPolicy.personSuggest(oController, mInput); },
                personTargetFromSource: function (oSource) { return DetailPersonInputRuntime.targetFromSource(oSource); },
            rememberDialogReturnFocus: function (sDialogKey, oSource) { oController._rememberDialogReturnFocus(sDialogKey, oSource); },
                rememberSuggestionSelection: function (sTarget, sValue) { DetailPersonInputRuntime.rememberSuggestionSelection(oController, sTarget, sValue); },
            restartSearchTimer: function (fnTask, iDelayMs) { oController._iLocationVhSearchTimer = SchedulingRuntime.restartTimer(0, function () { oController._iLocationVhSearchTimer = null; fnTask(); }, iDelayMs); },
            scheduleTableSync: function () { oController._scheduleLocationValueHelpTableSync(); },
            setViewFlag: function (sPath, bValue) { ControllerViewStateRuntime.setFlag(oController, sPath, bValue); },
            valueHelpLocation: function (mInput) { return DetailCommandPolicy.valueHelpLocation(oController, mInput); },
            withViewFlag: function (sPath, fnTask) { return oController._withViewFlag(sPath, fnTask); }
        };
    }

    return {
        onAttachmentUploadChange: function (oEvent) { return AttachmentUploadCore.onUploaderChange(this, oEvent); },
        onDeleteAttachment: function (oEvent) { return DetailAttachmentRuntime.deleteAttachment(this, oEvent, createAttachmentHooks(this)); },
        onToggleAttachmentsSection: function () { return DetailAttachmentRuntime.toggleAttachmentsSection(this, createAttachmentHooks(this)); },
        onOpenWorkflowAnalytics: function () { NavigationIntentService.navigateToAnalytics(this); return Promise.resolve(); },
        onOpenAttachment: function (oEvent) { return DetailAttachmentRuntime.openAttachment(this, oEvent); },
        onOpenLocationValueHelp: function (oEvent) { return DetailValueHelpRuntime.onOpenLocationValueHelp(this, oEvent, createValueHelpHooks(this)); },
        onCloseLocationValueHelp: function () { return DetailValueHelpRuntime.closeLocationValueHelp(this, createValueHelpHooks(this)); },
        onConfirmLocationValueHelp: function () { return DetailValueHelpRuntime.confirmLocationValueHelp(this, createValueHelpHooks(this)); },
        onLpcChange: function (oEvent) { return DetailValueHelpRuntime.onLpcChange(this, oEvent, createValueHelpHooks(this)); },
        onProfessionChange: function (oEvent) { return DetailValueHelpRuntime.onProfessionChange(this, oEvent, createValueHelpHooks(this)); },
        onChecksNumberChange: function (oEvent) { return DetailValueHelpRuntime.onChecksNumberChange(this, oEvent, createValueHelpHooks(this)); },
        onBarriersNumberChange: function (oEvent) { return DetailValueHelpRuntime.onBarriersNumberChange(this, oEvent, createValueHelpHooks(this)); },
        onPersonSuggest: function (oEvent) { return DetailValueHelpRuntime.onPersonSuggest(this, oEvent, createValueHelpHooks(this)); },
        onPersonSuggestionSelected: function (oEvent) { return DetailValueHelpRuntime.onPersonSuggestionSelected(this, oEvent, createValueHelpHooks(this)); },
        onPersonInputChange: function (oEvent) { return DetailValueHelpRuntime.onPersonInputChange(this, oEvent, createValueHelpHooks(this)); },
        onLocationValueHelpSearch: function (oEvent) { return DetailValueHelpRuntime.onLocationValueHelpSearch(this, oEvent, createValueHelpHooks(this)); },
        onLocationValueHelpSearchSubmit: function (oEvent) { return DetailValueHelpRuntime.onLocationValueHelpSearchSubmit(this, oEvent, createValueHelpHooks(this)); },
        onLocationTreeSelectionChange: function (oEvent) { return DetailValueHelpRuntime.onLocationTreeSelectionChange(this, oEvent, createValueHelpHooks(this)); }
    };
});
