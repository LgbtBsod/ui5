sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime"
], function (UseCase, Result, Effects, DetailSaveRuntime, DetailRuntimePayload, UseCaseValue, StatePaths, DeltaPayloadBuilder, CreateSentinel, WorkflowContracts, DetailAttachmentDeltaRuntime, DetailPersistenceRuntime) {
    "use strict";

    function AutosaveDetailUseCase() {
        UseCase.call(this, "AutosaveDetailUseCase");
    }

    function mapFieldDelta(mInput, oCurrent) {
        var mFieldMap = {
            LPC_KEY: "Lpc",
            PROF_KEY: "Profession",
            CHECKS_NUMBER: "ChecksNumber",
            BARRIERS_NUMBER: "BarriersNumber"
        };
        var sField = String((mInput && mInput.field) || "");
        if (!mFieldMap[sField]) { return null; }
        var oDraft = Object.assign({}, oCurrent || {});
        oDraft.basic = Object.assign({}, oDraft.basic || {});
        oDraft.basic[mFieldMap[sField]] = mInput.value;
        return oDraft;
    }

    function readSelectedChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && typeof oUiState.get === "function" && oUiState.get("selected", "/")) || null;
    }

    function readCurrentChecklist(mCtx) {
        return readSelectedChecklist(mCtx) || DetailSaveRuntime.readCurrentChecklist(mCtx);
    }

    function resolveDelta(mInput, mCtx) {
        if (mInput && mInput.delta && Object.keys(mInput.delta).length) {
            return mInput.delta;
        }
        var oCurrent = readCurrentChecklist(mCtx);
        var oSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
        var oMappedCurrent = mapFieldDelta(mInput, oCurrent) || oCurrent;
        return DeltaPayloadBuilder.buildDeltaPayload(oMappedCurrent, oSnapshot) || null;
    }

    function isAutosaveAllowed(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var sEditMode = WorkflowContracts.normalizeEditMode(oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        var sLockStatus = WorkflowContracts.normalizeLockState(oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE));
        var bDirty = !!(oUiState && oUiState.get("state", StatePaths.WORKFLOW_DIRTY));
        return WorkflowContracts.isEditLocked(sEditMode, sLockStatus) && bDirty;
    }

    function resolveClientVersion(oDelta, mCtx) {
        var oSnapshot = readCurrentChecklist(mCtx);
        var oCurrent = { root: { version_number: oDelta && oDelta.client_version } };
        return DetailSaveRuntime.resolveVersionNumber(
            oCurrent,
            oSnapshot
        );
    }

    AutosaveDetailUseCase.prototype = Object.create(UseCase.prototype);
    AutosaveDetailUseCase.prototype.constructor = AutosaveDetailUseCase;

    AutosaveDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseValue.rootId(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oDelta;
        var sSessionGuid = DetailSaveRuntime.readSessionGuid(mCtx, StatePaths);

        if (CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(Result.ok({ skipped: true, reason: "CREATE_DRAFT_PENDING" }, []));
        }
        if (!isAutosaveAllowed(mCtx)) {
            return Promise.resolve(Result.ok({ skipped: true, reason: "AUTOSAVE_GUARD" }, []));
        }
        if (!sRootId || !oRepo || typeof oRepo.autosaveChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Autosave unavailable", code: "AUTOSAVE_UNAVAILABLE" }));
        }

        oDelta = resolveDelta(mInput, mCtx);
        if (!oDelta) {
            return Promise.resolve(Result.fail({ message: "Autosave delta is empty", code: "AUTOSAVE_EMPTY_DELTA" }));
        }
        if (!oDelta.client_version) {
            oDelta = Object.assign({}, oDelta, { client_version: resolveClientVersion(oDelta, mCtx) });
        }
        if (!oDelta.client_version) {
            return Promise.resolve(Result.fail({ message: "Autosave requires valid client version", code: "AUTOSAVE_RELOAD_REQUIRED" }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")
            ]));
        }

        if (!sSessionGuid) {
            return Promise.resolve(Result.fail({ message: "Autosave requires active session lock", code: "LOCK_REQUIRED" }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")
            ]));
        }

        var oCurrentChecklist = readCurrentChecklist(mCtx);
        var aCurrentAttachments = Array.isArray((oCurrentChecklist && oCurrentChecklist.attachments) || null) ? oCurrentChecklist.attachments : [];

        return DetailAttachmentDeltaRuntime.serializeStagedAttachments(aCurrentAttachments, sRootId).then(function (aStagedPayload) {
            return Promise.resolve(oRepo.autosaveChecklist(DetailRuntimePayload.saveRequest({
                rootId: sRootId,
                delta: DetailAttachmentDeltaRuntime.mergeDeltaAttachments(oDelta, aStagedPayload),
                sessionGuid: sSessionGuid,
                attachments: []
            })));
        }).then(function (oSaved) {
            var sAt = (oSaved && oSaved.autosavedAt) || new Date().toISOString();
            oCurrentChecklist = readCurrentChecklist(mCtx);
            aCurrentAttachments = Array.isArray((oCurrentChecklist && oCurrentChecklist.attachments) || null) ? oCurrentChecklist.attachments : [];
        var oBaseSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
            var aSnapshotAttachments = Array.isArray((oBaseSnapshot && oBaseSnapshot.attachments) || null) ? oBaseSnapshot.attachments : [];
            var bHasPendingAttachments = DetailAttachmentDeltaRuntime.hasPendingStagedAttachments(aCurrentAttachments);
            var oSavedSnapshot = Object.assign({}, (oSaved && oSaved.serverSnapshot) || oCurrentChecklist, {
                attachments: bHasPendingAttachments ? aSnapshotAttachments : aCurrentAttachments
            });
            var oSelectedSnapshot = Object.assign({}, oSavedSnapshot, { attachments: aCurrentAttachments });
            return Result.ok({ autosavedAt: sAt }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, !bHasPendingAttachments),
                Effects.modelPatch("selected", "/", oSelectedSnapshot),
                Effects.modelPatch("selected", "/attachments", aCurrentAttachments),
                Effects.modelPatch("snapshot", "/", oSavedSnapshot)
            ].concat(DetailPersistenceRuntime.successEffects("auto", sAt, {
                state: bHasPendingAttachments ? DetailPersistenceRuntime.STATES.DIRTY : DetailPersistenceRuntime.STATES.SAVED,
                messageKey: bHasPendingAttachments ? "persistenceAutosavePendingAttachments" : "persistenceAutosaveSaved",
                hasValidLock: true,
                lockOwnerSessionMatches: true,
                lastLockRefreshAt: oSaved && oSaved.lock_refreshed ? sAt : null
            })));
        }).catch(function (oError) {
            var oClassification = DetailPersistenceRuntime.classifyError(oError);
            return Result.fail(oError, DetailPersistenceRuntime.failureEffects("auto", oError, {
                hasValidLock: !DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy),
                lockOwnerSessionMatches: !DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy)
            }).effects);
        });
    };

    return AutosaveDetailUseCase;
});
