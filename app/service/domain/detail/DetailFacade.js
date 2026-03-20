sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailUseCaseRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailEntryAdapterRuntime"
], function (
    DetailUseCaseRegistry,
    DetailEntryAdapterRuntime
) {
    "use strict";

    function DetailFacade(mDeps) {
        this._uc = DetailUseCaseRegistry.create(mDeps);
    }

    DetailFacade.prototype.open = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.open, i, c); };

    DetailFacade.prototype.enterEdit = function (i, c) {
        return DetailEntryAdapterRuntime.decorateEnterEdit(
            DetailEntryAdapterRuntime.executeUseCase(this._uc.enterEdit, i, c),
            i
        );
    };

    DetailFacade.prototype.confirmTakeover = function (i, c) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.takeoverLock, i, c);
    };

    DetailFacade.prototype.cancelEnterEdit = function (_i, _c) {
        return DetailEntryAdapterRuntime.buildCancelEnterEditResult();
    };

    DetailFacade.prototype.discardChanges = function (_i, c) {
        return DetailEntryAdapterRuntime.buildDiscardResult(c && c.uiState);
    };

    DetailFacade.prototype.onLockLost = function (i, c) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.lockLost, i, c);
    };

    DetailFacade.prototype.forceReadOnly = function (i, c) {
        return DetailEntryAdapterRuntime.executeUseCase(this._uc.forceReadOnly, i, c);
    };

    DetailFacade.prototype.close = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.close, i, c); };
    DetailFacade.prototype.closeDetail = function (i, c) { return this.close(i, c); };
    DetailFacade.prototype.save = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.save, i, c); };
    DetailFacade.prototype.validate = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.validate, i, c); };
    DetailFacade.prototype.autosave = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.autosave, i, c); };
    DetailFacade.prototype.deleteChecklist = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.deleteChecklist, i, c); };
    DetailFacade.prototype.resolveConflict = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.resolveConflict, i, c); };
    DetailFacade.prototype.attachmentLoad = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.attachmentLoad, i, c); };
    DetailFacade.prototype.attachmentUpload = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.attachmentUpload, i, c); };
    DetailFacade.prototype.attachmentDelete = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.attachmentDelete, i, c); };
    DetailFacade.prototype.rowOps = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.rowOps, i, c); };
    DetailFacade.prototype.valueHelpLocation = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.valueHelpLocation, i, c); };
    DetailFacade.prototype.personSuggest = function (i, c) { return DetailEntryAdapterRuntime.executeUseCase(this._uc.personSuggest, i, c); };

    return DetailFacade;
});
