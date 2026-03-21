sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil"
], function (Result, Effects, DetailSaveRuntime, DetailRuntimePayload, UseCaseValue, StatePaths, DeltaPayloadBuilder, CreateSentinel, WorkflowContracts, DetailAttachmentDeltaRuntime, DetailAttachmentSaveRuntime, DetailStateAccess, DetailPersistenceRuntime, ModelPathContracts, CloneUtil) {
    "use strict";

    function AutosaveDetailUseCase() {
        return {
            execute: execute
        };
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
        var oSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
        var oCurrent = { root: { version_number: oDelta && oDelta.client_version } };
        return DetailSaveRuntime.resolveVersionNumber(
            oCurrent,
            oSnapshot
        );
    }

    function writeDetailCache(oCacheWrite, sRootId, oSnapshot, mCtx) {
        if (!oCacheWrite || typeof oCacheWrite.execute !== "function" || !sRootId || !oSnapshot) {
            return Promise.resolve(null);
        }
        return Promise.resolve(oCacheWrite.execute({
            rootId: sRootId,
            snapshot: oSnapshot
        }, mCtx || {})).catch(function () {
            return null;
        });
    }

function execute(mInput, mCtx) {
        var sRootId = UseCaseValue.rootId(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oCacheWrite = mCtx && mCtx.cacheWrite;
        var oDelta;
        var sSessionGuid = DetailSaveRuntime.readSessionGuid(mCtx, StatePaths);
        var aSerializedAttachments = [];

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

        if (!sSessionGuid) {
            return Promise.resolve(Result.fail({ message: "Autosave requires active session lock", code: "LOCK_REQUIRED" }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.FAILED)
            ]));
        }

        var oCurrentChecklist = readCurrentChecklist(mCtx);
        var aCurrentAttachments = DetailStateAccess.readWorkingAttachments(mCtx);

        return DetailAttachmentDeltaRuntime.serializeStagedAttachments(aCurrentAttachments, sRootId).then(function (aStagedPayload) {
            aSerializedAttachments = Array.isArray(aStagedPayload) ? aStagedPayload : [];
            return Promise.resolve(oRepo.autosaveChecklist(DetailRuntimePayload.saveRequest({
                rootId: sRootId,
                delta: DetailAttachmentDeltaRuntime.mergeDeltaAttachments(oDelta, aSerializedAttachments),
                sessionGuid: sSessionGuid,
                attachments: []
            })));
        }).then(function (oSaved) {
            var sAt = (oSaved && oSaved.autosavedAt) || new Date().toISOString();
            oCurrentChecklist = readCurrentChecklist(mCtx);
            aCurrentAttachments = DetailStateAccess.readWorkingAttachments(mCtx);
            var oBaseSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
            var oSavedSnapshot = DetailSaveRuntime.normalizeOverallResult(
                DetailSaveRuntime.preserveBasicFields((oSaved && oSaved.serverSnapshot) || oCurrentChecklist, oCurrentChecklist, oBaseSnapshot)
            );
            return DetailAttachmentSaveRuntime.syncAfterSave({
                repo: oRepo,
                rootId: sRootId,
                createMode: false,
                currentAttachments: aCurrentAttachments,
                savedResult: oSaved,
                currentChecklist: oCurrentChecklist,
                savedSnapshot: oSavedSnapshot,
                baseSnapshot: oBaseSnapshot,
                ctx: mCtx,
                hasStagedPayload: aSerializedAttachments.length > 0
            }).then(function (oAttachmentSync) {
                return writeDetailCache(oCacheWrite, sRootId, oAttachmentSync.snapshot, mCtx).then(function () {
                    return Result.ok({ autosavedAt: sAt }, [
                        Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, oAttachmentSync.hasPendingAttachments),
                        Effects.modelPatch("snapshot", "/", CloneUtil.clone(oAttachmentSync.snapshot, {}))
                    ].concat(oAttachmentSync.effects, DetailPersistenceRuntime.successEffects("auto", sAt, {
                        state: oAttachmentSync.hasPendingAttachments ? DetailPersistenceRuntime.STATES.DIRTY : DetailPersistenceRuntime.STATES.SAVED,
                        messageKey: oAttachmentSync.hasPendingAttachments ? "persistenceAutosavePendingAttachments" : "persistenceAutosaveSaved",
                        hasValidLock: true,
                        lockOwnerSessionMatches: true,
                        lastLockRefreshAt: oSaved && oSaved.lock_refreshed ? sAt : null
                    })));
                });
            });
        }).catch(function (oError) {
            var oClassification = DetailPersistenceRuntime.classifyError(oError);
            return Result.fail(oError, DetailPersistenceRuntime.failureEffects("auto", oError, {
                hasValidLock: !DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy),
                lockOwnerSessionMatches: !DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy)
            }).effects);
        });
    }

    return AutosaveDetailUseCase;
});
