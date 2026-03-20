sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandContextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailUseCaseRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailEntryAdapterRuntime"
], function (
    ControllerCommandContextRuntime,
    DetailUseCaseRegistry,
    DetailEntryAdapterRuntime
) {
    "use strict";

    function DetailService(oController, mDeps) {
        this._controller = oController || null;
        this._uc = DetailUseCaseRegistry.create(mDeps);
    }

    DetailService.prototype._buildCtx = function () {
        return ControllerCommandContextRuntime.buildDefaultCtx(this._controller);
    };

    DetailService.prototype.openDetail = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.open, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.enterEdit = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.decorateEnterEdit(
            DetailEntryAdapterRuntime.executeUseCase(this._uc.enterEdit, mInput, mCtx || this._buildCtx()),
            mInput
        );
    };

    DetailService.prototype.saveDetail = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.save, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.autosaveDetail = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.autosave, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.loadAttachments = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.attachmentLoad, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.rowAction = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.rowOps, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.handleLockLost = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.lockLost, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.confirmTakeover = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.takeoverLock, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.cancelEnterEdit = function () {
        return DetailEntryAdapterRuntime.buildCancelEnterEditResult();
    };

    DetailService.prototype.discardChanges = function (_mInput, mCtx) {
        var oCtx = mCtx || this._buildCtx();
        return DetailEntryAdapterRuntime.buildDiscardResult(oCtx && oCtx.uiState);
    };

    DetailService.prototype.forceReadOnly = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.forceReadOnly, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.close = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.close, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.validate = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.validate, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.deleteChecklist = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.deleteChecklist, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.resolveConflict = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.resolveConflict, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.attachmentUpload = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.attachmentUpload, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.attachmentDelete = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.attachmentDelete, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.valueHelpLocation = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.valueHelpLocation, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.personSuggest = function (mInput, mCtx) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.personSuggest, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.open = DetailService.prototype.openDetail;
    DetailService.prototype.save = DetailService.prototype.saveDetail;
    DetailService.prototype.autosave = DetailService.prototype.autosaveDetail;
    DetailService.prototype.attachmentLoad = DetailService.prototype.loadAttachments;
    DetailService.prototype.rowOps = DetailService.prototype.rowAction;
    DetailService.prototype.onLockLost = DetailService.prototype.handleLockLost;

    return DetailService;
});
