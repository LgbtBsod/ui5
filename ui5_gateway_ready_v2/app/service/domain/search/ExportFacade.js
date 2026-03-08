sap.ui.define([
    "checklist/app/service/domain/search/usecases/ExportSearchUseCase"
], function (ExportSearchUseCase) {
    "use strict";

    function ExportFacade(mDeps) {
        var d = mDeps || {};
        this._useCase = d.exportUseCase || new ExportSearchUseCase();
    }

    ExportFacade.prototype.exportFlow = function (mInput, mCtx) {
        return this._useCase.execute(mInput || {}, mCtx || {});
    };

    ExportFacade.prototype.exportEntity = function (sEntity, mCtx) {
        return this.exportFlow({ entity: sEntity || "screen" }, mCtx || {});
    };

    return ExportFacade;
});
