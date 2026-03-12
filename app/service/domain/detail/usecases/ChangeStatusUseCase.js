sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailSaveSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/ValidationPathMap",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/ChecklistValidationService",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil"
], function (UseCase, Result, Effects, UseCaseValue, StatePaths, DetailStateAccess, DetailSaveSupport, ValidationPathMap, ChecklistValidationService, DeltaPayloadBuilder, CloneUtil) {
    "use strict";

    function ChangeStatusUseCase() {
        UseCase.call(this, "ChangeStatusUseCase");
    }

    ChangeStatusUseCase.prototype = Object.create(UseCase.prototype);
    ChangeStatusUseCase.prototype.constructor = ChangeStatusUseCase;

    function clone(vValue) {
        return CloneUtil.clone(vValue, {});
    }

    ChangeStatusUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseValue.rootId(mInput);
        var sStatus = UseCaseValue.text(mInput && mInput.status).toUpperCase();
        var oRepo = mCtx && mCtx.repo;
        var oChecklist = DetailStateAccess.readCurrentChecklist(mCtx);
        var oSnapshot = DetailStateAccess.readDetailSnapshot(mCtx);
        var oNextChecklist;
        var oDelta;
        var sSessionGuid = DetailSaveSupport.readSessionGuid(mCtx, StatePaths);
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
                Effects.modelPatch("view", ViewPathContracts.VALIDATION_SHOWN, false),
                Effects.modelPatch("view", ViewPathContracts.VALIDATION_MISSING, {}),
                Effects.toast("statusChangeValidationUnavailableToast", "warning")
            ]));
        }
        if (sStatus !== "DRAFT" && !oValidation.valid) {
            return Promise.resolve(Result.ok({
                blocked: true,
                status: sStatus,
                missingPaths: oValidation.missingPaths || []
            }, [
                Effects.modelPatch("view", ViewPathContracts.VALIDATION_SHOWN, true),
                Effects.modelPatch("view", ViewPathContracts.VALIDATION_MISSING, ValidationPathMap.toMissingMap(oValidation.missingPaths)),
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
        if (!sSessionGuid || DetailSaveSupport.readLockState(mCtx, StatePaths) !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
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
                Effects.modelPatch("snapshot", "/", oSavedSnapshot),
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
