sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPayloadMapper",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (Result, Effects, DetailSaveRuntime, DetailRuntimePayload, UseCaseValue, StatePaths, DeltaPayloadBuilder, CreateSentinel, WorkflowContracts, DetailAttachmentSaveRuntime, DetailAttachmentDeltaRuntime, DetailStateAccess, DetailPersistenceRuntime, ModelPathContracts, CloneUtil, ODataChecklistPayloadMapper, ModelContracts, DetailContracts, MessageCodeConstants, MessageKeyConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL = MODELS.DETAIL;
    var STATE_MODEL = MODELS.STATE;
    var DETAIL_CODES = MessageCodeConstants.DETAIL;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.VIEW;
    var DETAIL_MODEL_PATHS = DetailContracts.MODEL_PATHS;
    var DETAIL_REASONS = DetailContracts.REASONS;

    function AutosaveDetailUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

    function applyAliasedBasicField(mInput, oCurrent) {
        var sField = String((mInput && mInput.field) || "");
        if (!sField) { return null; }
        var oDraft = Object.assign({}, oCurrent || {});
        oDraft.basic = Object.assign({}, oDraft.basic || {});
        oDraft.basic = ODataChecklistPayloadMapper.applyBasicFieldAlias(oDraft.basic, sField, mInput.value);
        return oDraft;
    }

    function readSelectedChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && typeof oUiState.get === "function" && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT)) || null;
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
        var oMappedCurrent = applyAliasedBasicField(mInput, oCurrent) || oCurrent;
        return DeltaPayloadBuilder.buildDeltaPayload(oMappedCurrent, oSnapshot) || null;
    }

    function isAutosaveAllowed(sDbKey, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return DetailPersistenceRuntime.canAutosaveFromState(oUiState, {
            dbKey: sDbKey,
            isCreateDraft: CreateSentinel.isCreateId(sDbKey)
        });
    }

    function resolveClientVersion(oDelta, mCtx) {
        var oSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
        var oCurrent = { root: { version_number: oDelta && oDelta.client_version } };
        return DetailSaveRuntime.resolveVersionNumber(
            oCurrent,
            oSnapshot
        );
    }

    function writeDetailCache(oCacheWrite, sDbKey, oSnapshot, mCtx) {
        if (!oCacheWrite || typeof oCacheWrite.execute !== "function" || !sDbKey || !oSnapshot) {
            return Promise.resolve(null);
        }
        return Promise.resolve(oCacheWrite.execute({
            dbKey: sDbKey,
            snapshot: oSnapshot
        }, mCtx || {})).catch(function () {
            return null;
        });
    }

    function mergeDeltaAttachments(oDelta, aCurrentAttachments, sDbKey) {
        var aPendingAttachmentRows = DetailAttachmentDeltaRuntime.listPendingStagedAttachments(aCurrentAttachments, sDbKey);
        if (!aPendingAttachmentRows.length) {
            return oDelta;
        }
        return DetailAttachmentDeltaRuntime.mergeDeltaAttachments(oDelta, aPendingAttachmentRows);
    }

    function execute(mInput, mCtx) {
        var sDbKey = UseCaseValue.dbKey(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oCacheWrite = mCtx && mCtx.cacheWrite;
        var oDelta;
        var sSessionGuid = DetailSaveRuntime.readSessionGuid(mCtx, StatePaths);
        if (CreateSentinel.isCreateId(sDbKey)) {
            return Promise.resolve(Result.ok({ skipped: true, reason: DETAIL_REASONS.CREATE_DRAFT_PENDING }, []));
        }
        if (!isAutosaveAllowed(sDbKey, mCtx)) {
            return Promise.resolve(Result.ok({ skipped: true, reason: DETAIL_REASONS.AUTOSAVE_GUARD }, []));
        }
        if (!sDbKey || !oRepo || typeof oRepo.autosaveChecklist !== "function") {
            return Promise.resolve(Result.fail({ code: DETAIL_CODES.AUTOSAVE_UNAVAILABLE, messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_TECHNICAL_ERROR }));
        }

        oDelta = resolveDelta(mInput, mCtx);
        if (!oDelta) {
            return Promise.resolve(Result.fail({ code: DETAIL_CODES.AUTOSAVE_EMPTY_DELTA, messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_NO_CHANGES }));
        }
        if (!oDelta.client_version) {
            oDelta = Object.assign({}, oDelta, { client_version: resolveClientVersion(oDelta, mCtx) });
        }

        if (!sSessionGuid) {
            return Promise.resolve(Result.fail({ code: DETAIL_CODES.LOCK_REQUIRED, messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_MISSING }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.FAILED)
            ]));
        }

        var oCurrentChecklist = readCurrentChecklist(mCtx);
        var aCurrentAttachments = DetailStateAccess.readWorkingAttachments(mCtx);

        oDelta = mergeDeltaAttachments(oDelta, aCurrentAttachments, sDbKey);

        return Promise.resolve(oRepo.autosaveChecklist(DetailRuntimePayload.saveRequest({
            dbKey: sDbKey,
            delta: oDelta,
            sessionGuid: sSessionGuid,
            attachments: []
        }))).then(function (oSaved) {
            var sAt = (oSaved && oSaved.autosavedAt) || new Date().toISOString();
            oCurrentChecklist = readCurrentChecklist(mCtx);
            aCurrentAttachments = DetailStateAccess.readWorkingAttachments(mCtx);
            var oBaseSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
            var oSavedSnapshot = DetailSaveRuntime.normalizeOverallResult(
                DetailSaveRuntime.preserveBasicFields((oSaved && oSaved.serverSnapshot) || oCurrentChecklist, oCurrentChecklist, oBaseSnapshot)
            );
                return DetailAttachmentSaveRuntime.syncAfterSave({
                    attachmentGateway: mCtx && mCtx.attachmentGateway,
                    repo: oRepo,
                    dbKey: sDbKey,
                    createMode: false,
                currentAttachments: aCurrentAttachments,
                sessionGuid: sSessionGuid,
                savedResult: oSaved,
                currentChecklist: oCurrentChecklist,
                savedSnapshot: oSavedSnapshot,
                baseSnapshot: oBaseSnapshot,
                ctx: mCtx,
                hasStagedPayload: false
            }).then(function (oAttachmentSync) {
                return writeDetailCache(oCacheWrite, sDbKey, oAttachmentSync.snapshot, mCtx).then(function () {
                    return Result.ok({ autosavedAt: sAt }, [
                        Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DIRTY, oAttachmentSync.hasPendingAttachments),
                        Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE, CloneUtil.clone(oAttachmentSync.snapshot, {}))
                    ].concat(oAttachmentSync.effects, DetailPersistenceRuntime.successEffects("auto", sAt, {
                        state: oAttachmentSync.hasPendingAttachments ? DetailPersistenceRuntime.STATES.DIRTY : DetailPersistenceRuntime.STATES.SAVED,
                        messageKey: oAttachmentSync.hasPendingAttachments ? DETAIL_MESSAGE_KEYS.PERSISTENCE_AUTOSAVE_PENDING_ATTACHMENTS : DETAIL_MESSAGE_KEYS.PERSISTENCE_AUTOSAVE_SAVED,
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
