sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/runtime/DetailOpenMatchRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/runtime/DetailEditSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/runtime/DetailSaveFlowRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/runtime/DetailAttachmentFlowRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/runtime/DetailFieldAssistRuntime"
], function (
    DetailOpenMatchRuntime,
    DetailEditSessionRuntime,
    DetailSaveFlowRuntime,
    DetailAttachmentFlowRuntime,
    DetailFieldAssistRuntime
) {
    "use strict";

    function DetailFacade(mDeps) {
        var d = mDeps || {};
        this._runtime = {
            openMatch: d.openMatchRuntime || new DetailOpenMatchRuntime(d),
            editSession: d.editSessionRuntime || new DetailEditSessionRuntime(d),
            saveFlow: d.saveFlowRuntime || new DetailSaveFlowRuntime(d),
            attachments: d.attachmentFlowRuntime || new DetailAttachmentFlowRuntime(d),
            fieldAssist: d.fieldAssistRuntime || new DetailFieldAssistRuntime(d)
        };
    }

    DetailFacade.prototype.open = function (i, c) { return this._runtime.openMatch.open(i, c); };
    DetailFacade.prototype.enterEdit = function (i, c) { return this._runtime.editSession.enterEdit(i, c); };
    DetailFacade.prototype.confirmTakeover = function (i, c) { return this._runtime.editSession.confirmTakeover(i, c); };
    DetailFacade.prototype.cancelEnterEdit = function (i, c) { return this._runtime.editSession.cancelEnterEdit(i, c); };
    DetailFacade.prototype.discardChanges = function (i, c) { return this._runtime.editSession.discardChanges(i, c); };
    DetailFacade.prototype.onLockLost = function (i, c) { return this._runtime.editSession.onLockLost(i, c); };
    DetailFacade.prototype.forceReadOnly = function (i, c) { return this._runtime.editSession.forceReadOnly(i, c); };
    DetailFacade.prototype.close = function (i, c) { return this._runtime.editSession.close(i, c); };
    DetailFacade.prototype.closeDetail = function (i, c) { return this.close(i, c); };
    DetailFacade.prototype.save = function (i, c) { return this._runtime.saveFlow.save(i, c); };
    DetailFacade.prototype.validate = function (i, c) { return this._runtime.saveFlow.validate(i, c); };
    DetailFacade.prototype.autosave = function (i, c) { return this._runtime.saveFlow.autosave(i, c); };
    DetailFacade.prototype.deleteChecklist = function (i, c) { return this._runtime.saveFlow.deleteChecklist(i, c); };
    DetailFacade.prototype.resolveConflict = function (i, c) { return this._runtime.saveFlow.resolveConflict(i, c); };
    DetailFacade.prototype.attachmentLoad = function (i, c) { return this._runtime.attachments.attachmentLoad(i, c); };
    DetailFacade.prototype.attachmentUpload = function (i, c) { return this._runtime.attachments.attachmentUpload(i, c); };
    DetailFacade.prototype.attachmentDelete = function (i, c) { return this._runtime.attachments.attachmentDelete(i, c); };
    DetailFacade.prototype.rowOps = function (i, c) { return this._runtime.fieldAssist.rowOps(i, c); };
    DetailFacade.prototype.valueHelpLocation = function (i, c) { return this._runtime.fieldAssist.valueHelpLocation(i, c); };
    DetailFacade.prototype.personSuggest = function (i, c) { return this._runtime.fieldAssist.personSuggest(i, c); };

    return DetailFacade;
});
