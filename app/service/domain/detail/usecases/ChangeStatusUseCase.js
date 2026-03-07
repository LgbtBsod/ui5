sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/framework/Effects",
    "sap_ui5/service/domain/shared/UseCaseInputUtils",
    "sap_ui5/service/domain/shared/StatePaths",
    "sap_ui5/service/domain/detail/DetailStateAccess",
    "sap_ui5/service/domain/detail/DetailValidationSupport",
    "sap_ui5/util/ChecklistValidationService",
    "sap_ui5/util/DeltaPayloadBuilder"
], function (UseCase, Result, Effects, UseCaseInputUtils, StatePaths, DetailStateAccess, DetailValidationSupport, ChecklistValidationService, DeltaPayloadBuilder) {
    "use strict";

    function ChangeStatusUseCase() {
        UseCase.call(this, "ChangeStatusUseCase");
    }

    ChangeStatusUseCase.prototype = Object.create(UseCase.prototype);
    ChangeStatusUseCase.prototype.constructor = ChangeStatusUseCase;

    function clone(vValue) {
        return JSON.parse(JSON.stringify(vValue || {}));
    }

    function readSessionGuid(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && oUiState.get("state", StatePaths.SESSION_ID)) || "";
    }

    function readLockState(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE)) || "").toUpperCase();
    }

    ChangeStatusUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var sStatus = UseCaseInputUtils.text(mInput && mInput.status).toUpperCase();
        var oRepo = mCtx && mCtx.repo;
        var oChecklist = DetailStateAccess.readCurrentChecklist(mCtx);
        var oSnapshot = DetailStateAccess.readDetailSnapshot(mCtx);
        var oNextChecklist;
        var oDelta;
        var sSessionGuid = readSessionGuid(mCtx);
        var oValidation = ChecklistValidationService.validateRequiredFields(oChecklist, {
            requiredFields: DetailStateAccess.readRequiredFields(mCtx)
        });

        if (!sRootId || !sStatus || !oRepo || typeof oRepo.saveChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Status command invalid", code: "INVALID_STATUS_COMMAND" }));
        }
        if (sStatus !== "DRAFT" && oValidation.unavailable) {
            return Promise.resolve(Result.fail({
                message: "Status validation rules are not loaded yet",
                code: "REQUIRED_FIELDS_UNAVAILABLE"
            }, [
                Effects.modelPatch("view", "/validationShown", false),
                Effects.modelPatch("view", "/validationMissing", {}),
                Effects.toast("statusChangeValidationUnavailableToast", "warning")
            ]));
        }
        if (sStatus !== "DRAFT" && !oValidation.valid) {
            return Promise.resolve(Result.ok({
                blocked: true,
                status: sStatus,
                missingPaths: oValidation.missingPaths || []
            }, [
                Effects.modelPatch("view", "/validationShown", true),
                Effects.modelPatch("view", "/validationMissing", DetailValidationSupport.toMissingMap(oValidation.missingPaths)),
                Effects.toast("checklistValidationFailedToast", "warning")
            ]));
        }
        oNextChecklist = clone(oChecklist);
        oNextChecklist.root = Object.assign({}, oNextChecklist.root || {}, {
            status: sStatus,
            Status: sStatus
        });
        oDelta = DeltaPayloadBuilder.buildDeltaPayload(oNextChecklist, oSnapshot);

        if (!oDelta || !oDelta.client_version) {
            return Promise.resolve(Result.fail({ message: "Detail snapshot is stale; reload required", code: "MISSING_CLIENT_VERSION" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")
            ]));
        }
        if (!sSessionGuid || readLockState(mCtx) !== "LOCKED") {
            return Promise.resolve(Result.fail({ message: "Active lock is required before status change", code: "LOCK_REQUIRED" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")
            ]));
        }

        return Promise.resolve(oRepo.saveChecklist({
            rootId: sRootId,
            delta: oDelta,
            sessionGuid: sSessionGuid
        })).then(function (oSaved) {
            var sNow = new Date().toISOString();
            var oSavedSnapshot = (oSaved && oSaved.serverSnapshot) || oNextChecklist || {};

            return Result.ok({ status: sStatus, serverSnapshot: oSavedSnapshot }, [
                Effects.modelPatch("selected", "/", oSavedSnapshot),
                Effects.modelPatch("uiState", "/_detailCurrent", oSavedSnapshot),
                Effects.modelPatch("uiState", "/_detailSnapshot", oSavedSnapshot),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "SAVED"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, sNow),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.toast("statusChangeSuccess", "success")
            ]);
        }).catch(function (oError) {
            return Result.fail(oError);
        });
    };

    return ChangeStatusUseCase;
});
