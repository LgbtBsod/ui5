sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPostOpenRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts"
], function (Result, Effects, DetailSaveRuntime, DetailRuntimePayload, UseCaseValue, StatePaths, DeltaPayloadBuilder, CreateSentinel, DetailAttachmentSaveRuntime, DetailAttachmentDeltaRuntime, DetailStateAccess, WorkflowContracts, DetailPersistenceRuntime, DetailPostOpenRuntime, CloneUtil, ChecklistIdentity, SearchReturnRediscoveryRuntime, ModelContracts, DetailContracts, MessageCodeConstants, MessageKeyConstants, NavigationContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL = MODELS.DETAIL;
    var SHELL_MODEL = MODELS.SHELL;
    var STATE_MODEL = MODELS.STATE;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var DETAIL_MODEL_PATHS = DetailContracts.MODEL_PATHS;
    var DETAIL_REASONS = DetailContracts.REASONS;
    var DETAIL_CODES = MessageCodeConstants.DETAIL;
    var DETAIL_FLOW_CODES = MessageCodeConstants.FLOW;
    var DETAIL_DETAIL_KEYS = MessageKeyConstants.DETAIL;
    var DETAIL_VIEW_KEYS = MessageKeyConstants.VIEW;

    function SaveDetailUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

    function readSelectedChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && typeof oUiState.get === "function" && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT)) || null;
    }

    function readCurrentChecklist(mCtx) {
        return readSelectedChecklist(mCtx) || DetailSaveRuntime.readCurrentChecklist(mCtx);
    }

    function resolveChecklistDisplayId(oCurrentChecklist, oSelectedChecklist, oSavedSnapshot) {
        return ChecklistIdentity.extractChecklistDisplayId(oCurrentChecklist)
            || ChecklistIdentity.extractChecklistDisplayId(oSelectedChecklist)
            || ChecklistIdentity.extractChecklistDisplayId(oSavedSnapshot);
    }

    function buildSearchReturnContext(sMode, sDbKey, oCurrentChecklist, oSelectedChecklist, oSavedSnapshot) {
        return SearchReturnRediscoveryRuntime.buildContext({
            dbKey: sDbKey,
            checklistId: resolveChecklistDisplayId(oCurrentChecklist, oSelectedChecklist, oSavedSnapshot),
            reason: DETAIL_REASONS.DETAIL_SAVE_COMPLETED,
            mode: sMode,
            focusRequested: true,
            selectionRequested: true
        });
    }

    function writeDetailCache(oCacheWrite, sDbKey, oSnapshot, mCtx) {
        if (!oCacheWrite || typeof oCacheWrite.execute !== "function" || !sDbKey || !oSnapshot) {
            return Promise.resolve(null);
        }
        return Promise.resolve(oCacheWrite.execute({
            dbKey: sDbKey,
            snapshot: Object.assign({}, oSnapshot, {
                attachments: []
            })
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
        var oUiState = mCtx && mCtx.uiState;
        var oCurrent = readCurrentChecklist(mCtx);
        var oSelectedChecklist = readSelectedChecklist(mCtx);
        var oSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
        var oRepo = mCtx && mCtx.repo;
        var oCacheWrite = mCtx && mCtx.cacheWrite;
        var oLock = mCtx && mCtx.lock;
        var sMode = WorkflowContracts.normalizeEditMode(oUiState && oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        var bCreate = CreateSentinel.isCreateId(sDbKey) || sMode === WorkflowContracts.EDIT_MODES.CREATE;
        var oDelta = (mInput && mInput.delta) || (bCreate ? DeltaPayloadBuilder.buildCreatePayload(oCurrent) : DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oSnapshot));
        var iClientVersion = DetailSaveRuntime.resolveVersionNumber(oCurrent, oSnapshot);
        var sSessionGuid = DetailSaveRuntime.readSessionGuid(mCtx, StatePaths);
        var sLockState = DetailSaveRuntime.readLockState(mCtx, StatePaths);
        var aCurrentAttachments = DetailStateAccess.readWorkingAttachments(mCtx);

        if (!oRepo) {
            return Promise.resolve(Result.fail({ code: DETAIL_CODES.SAVE_HANDLER_MISSING, messageKey: DETAIL_VIEW_KEYS.GENERIC_OPERATION_FAILED }));
        }
        if (!bCreate && typeof oRepo.saveChecklist !== "function") {
            return Promise.resolve(Result.fail({ code: DETAIL_CODES.SAVE_HANDLER_MISSING, messageKey: DETAIL_VIEW_KEYS.GENERIC_OPERATION_FAILED }));
        }
        if (bCreate && typeof oRepo.createChecklist !== "function") {
            return Promise.resolve(Result.fail({ code: DETAIL_CODES.CREATE_HANDLER_MISSING, messageKey: DETAIL_VIEW_KEYS.GENERIC_OPERATION_FAILED }));
        }
        if (!bCreate && !oDelta) {
            return Promise.resolve(Result.ok({ saved: false, skipped: true, reason: DETAIL_FLOW_CODES.NO_CHANGES }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
            ].concat(DetailPersistenceRuntime.dirtyEffects(false, {
                messageKey: DETAIL_VIEW_KEYS.PERSISTENCE_NO_CHANGES,
                isManualSaveInFlight: false,
                isAutoSaveInFlight: false,
                currentWriteRequestId: ""
            }))));
        }
        if (!bCreate && (!oDelta || !oDelta.client_version) && !iClientVersion) {
            return Promise.resolve(Result.fail({ code: DETAIL_CODES.MISSING_CLIENT_VERSION, messageKey: DETAIL_VIEW_KEYS.PERSISTENCE_CONFLICT }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.FAILED)
            ]));
        }
        if (!bCreate && oDelta && !oDelta.client_version && iClientVersion) {
            oDelta = Object.assign({}, oDelta, { client_version: iClientVersion });
        }
        if (!bCreate && (!sSessionGuid || sLockState !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED)) {
            return Promise.resolve(Result.fail({ code: DETAIL_CODES.LOCK_REQUIRED, messageKey: DETAIL_VIEW_KEYS.PERSISTENCE_LOCK_MISSING }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.FAILED)
            ]));
        }

        oDelta = mergeDeltaAttachments(oDelta, aCurrentAttachments, sDbKey);

        return Promise.resolve().then(function () {
            var pSave = bCreate
                ? Promise.resolve(oRepo.createChecklist({ delta: oDelta, sessionGuid: sSessionGuid }))
                : Promise.resolve(oRepo.saveChecklist(DetailRuntimePayload.saveRequest({
                    dbKey: sDbKey,
                    delta: oDelta,
                    sessionGuid: sSessionGuid,
                    attachments: []
                })));

            return pSave.then(function (oSaved) {
                var sNow = new Date().toISOString();
                var oInitialSavedSnapshot = DetailSaveRuntime.normalizeOverallResult(
                    DetailSaveRuntime.preserveBasicFields((oSaved && oSaved.serverSnapshot) || oCurrent || {}, oCurrent, oSnapshot)
                );
                var sServerDbKey = String((oInitialSavedSnapshot && (oInitialSavedSnapshot.db_key || oInitialSavedSnapshot.DB_KEY || (oInitialSavedSnapshot.root && oInitialSavedSnapshot.root.id))) || "").trim();
                var pLockAcquire = Promise.resolve(null);
                if (bCreate && sServerDbKey && !CreateSentinel.isCreateId(sServerDbKey) && oLock && typeof oLock.acquire === "function" && sSessionGuid) {
                    pLockAcquire = Promise.resolve(oLock.acquire(DetailRuntimePayload.lockRequest({
                        dbKey: sServerDbKey,
                        sessionGuid: sSessionGuid
                    }, null, StatePaths))).catch(function () {
                        return null;
                    });
                }

                return Promise.all([
                    DetailAttachmentSaveRuntime.syncAfterSave({
                        attachmentGateway: mCtx && mCtx.attachmentGateway,
                        repo: oRepo,
                        dbKey: sDbKey,
                        createMode: bCreate,
                        currentAttachments: aCurrentAttachments,
                        sessionGuid: sSessionGuid,
                        savedResult: oSaved,
                        currentChecklist: oCurrent,
                        savedSnapshot: oInitialSavedSnapshot,
                        baseSnapshot: oSnapshot,
                        ctx: mCtx,
                        serverRootId: sServerDbKey,
                        hasStagedPayload: false
                    }),
                    pLockAcquire
                ]).then(function (aPostSave) {
                    var oAttachmentSync = aPostSave[0];
                    var oLockResult = aPostSave[1];
                    var oSavedSnapshot = DetailSaveRuntime.normalizeOverallResult(
                        DetailSaveRuntime.preserveBasicFields(oAttachmentSync.snapshot, oCurrent, oSnapshot)
                    );
                    var oSelectedSnapshot = CloneUtil.clone(oAttachmentSync.selectedSnapshot, {});
                    return writeDetailCache(oCacheWrite, sServerDbKey, oSavedSnapshot, mCtx).then(function () {
                        var aEffects = [
                            Effects.toast(DETAIL_DETAIL_KEYS.OBJECT_SAVED, "success"),
                            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DIRTY, false),
                            Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                            Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE, CloneUtil.clone(oSavedSnapshot, {}))
                        ];
                        aEffects = aEffects.concat(oAttachmentSync.effects);
                        aEffects = aEffects.concat(DetailPersistenceRuntime.successEffects("manual", sNow, {
                            hasValidLock: WorkflowContracts.normalizeEditMode(sMode) === WorkflowContracts.EDIT_MODES.EDIT,
                            lockOwnerSessionMatches: WorkflowContracts.normalizeEditMode(sMode) === WorkflowContracts.EDIT_MODES.EDIT,
                            lastLockRefreshAt: oSaved && oSaved.lock_refreshed ? sNow : null
                        }));
                        if (sServerDbKey && !CreateSentinel.isCreateId(sServerDbKey)) {
                            aEffects.push(Effects.modelPatch(
                                STATE_MODEL,
                                StatePaths.SEARCH_RETURN_CONTEXT,
                                buildSearchReturnContext(
                                    bCreate ? SearchReturnRediscoveryRuntime.MODES.CREATE : SearchReturnRediscoveryRuntime.MODES.SAVE,
                                    sServerDbKey,
                                    oCurrent,
                                    oSelectedChecklist,
                                    oSavedSnapshot
                                )
                            ));
                            aEffects.push(Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT_ID, sServerDbKey));
                            if (bCreate) {
                                var bLockAcquired = !!(oLockResult && oLockResult.ok);
                                if (bLockAcquired) {
                                    aEffects = aEffects.concat(DetailPostOpenRuntime.buildEditableDetailEffects(sServerDbKey, {
                                        snapshot: oSelectedSnapshot,
                                        autosaveEnabled: true
                                    }));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_HAS_VALID_LOCK, true));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES, true));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_LAST_LOCK_REFRESH_AT, sNow));
                                    aEffects.push(Effects.modelPatch(SHELL_MODEL, MODEL_PATHS.SHELL_LOCK, {
                                        ok: true,
                                        reason: WorkflowContracts.REASONS.OWNED_BY_YOU,
                                        isKilled: false
                                    }));
                                } else {
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.ACTIVE_OBJECT_ID, sServerDbKey));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.SELECTED_ID, sServerDbKey));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.POST_OPEN_HYDRATED_ROOT_ID, sServerDbKey));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false));
                                }
                                aEffects.push(Effects.navigate(NavigationContracts.ROUTES.DETAIL, { id: sServerDbKey }, true));
                            } else {
                                aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.ACTIVE_OBJECT_ID, sServerDbKey));
                                aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.SELECTED_ID, sServerDbKey));
                            }
                        }
                        return Result.ok({ serverSnapshot: oSavedSnapshot || {}, selectedSnapshot: oSelectedSnapshot || {}, savedAt: sNow, lock: oLockResult || null }, aEffects);
                    });
                });
            }).catch(function (oError) {
                var oClassification = DetailPersistenceRuntime.classifyError(oError);
                var oFailure = DetailPersistenceRuntime.failureEffects("manual", oError, {
                    hasValidLock: !DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy) && sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED,
                    lockOwnerSessionMatches: !DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy)
                });
                return Result.fail(oError, [
                    Effects.modelPatch(STATE_MODEL, StatePaths.SAVE_IN_FLIGHT, false),
                    Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
                ].concat(oFailure.effects));
            });
        });
    }

    return SaveDetailUseCase;
});
