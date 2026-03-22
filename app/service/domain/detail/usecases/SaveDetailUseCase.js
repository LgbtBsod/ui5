sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPostOpenRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants"
], function (Result, Effects, DetailSaveRuntime, DetailRuntimePayload, UseCaseValue, StatePaths, DeltaPayloadBuilder, CreateSentinel, DetailAttachmentDeltaRuntime, DetailAttachmentSaveRuntime, DetailStateAccess, ModelPathContracts, WorkflowContracts, DetailPersistenceRuntime, DetailPostOpenRuntime, CloneUtil, ChecklistIdentity, SearchReturnRediscoveryRuntime, ModelContracts, DetailContracts, NavigationContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL = MODELS.DETAIL;
    var STATE_MODEL = MODELS.STATE;
    var DETAIL_CODES = DetailContracts.CODES;
    var DETAIL_MODEL_PATHS = DetailContracts.MODEL_PATHS;
    var DETAIL_REASONS = DetailContracts.REASONS;

    function SaveDetailUseCase() {
        return {
            execute: execute
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

    function buildSearchReturnContext(sMode, sRootId, oCurrentChecklist, oSelectedChecklist, oSavedSnapshot) {
        return SearchReturnRediscoveryRuntime.buildContext({
            rootId: sRootId,
            checklistId: resolveChecklistDisplayId(oCurrentChecklist, oSelectedChecklist, oSavedSnapshot),
            reason: DETAIL_REASONS.DETAIL_SAVE_COMPLETED,
            mode: sMode,
            focusRequested: true,
            selectionRequested: true
        });
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
        var oUiState = mCtx && mCtx.uiState;
        var oCurrent = readCurrentChecklist(mCtx);
        var oSelectedChecklist = readSelectedChecklist(mCtx);
        var oSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
        var oRepo = mCtx && mCtx.repo;
        var oCacheWrite = mCtx && mCtx.cacheWrite;
        var oLock = mCtx && mCtx.lock;
        var sMode = WorkflowContracts.normalizeEditMode(oUiState && oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        var bCreate = CreateSentinel.isCreateId(sRootId) || sMode === WorkflowContracts.EDIT_MODES.CREATE;
        var oDelta = (mInput && mInput.delta) || (bCreate ? DeltaPayloadBuilder.buildCreatePayload(oCurrent) : DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oSnapshot));
        var iClientVersion = DetailSaveRuntime.resolveVersionNumber(oCurrent, oSnapshot);
        var sSessionGuid = DetailSaveRuntime.readSessionGuid(mCtx, StatePaths);
        var sLockState = DetailSaveRuntime.readLockState(mCtx, StatePaths);
        var aCurrentAttachments = DetailStateAccess.readWorkingAttachments(mCtx);

        if (!oRepo) {
            return Promise.resolve(Result.fail({ message: "Save handler unavailable", code: DETAIL_CODES.SAVE_HANDLER_MISSING }));
        }
        if (!bCreate && typeof oRepo.saveChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Save handler unavailable", code: DETAIL_CODES.SAVE_HANDLER_MISSING }));
        }
        if (bCreate && typeof oRepo.createChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Create handler unavailable", code: DETAIL_CODES.CREATE_HANDLER_MISSING }));
        }
        if (!bCreate && !oDelta) {
            return Promise.resolve(Result.ok({ saved: false, skipped: true, reason: DETAIL_CODES.NO_CHANGES }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
            ].concat(DetailPersistenceRuntime.dirtyEffects(false, {
                messageKey: DetailContracts.PERSISTENCE_NO_CHANGES,
                isManualSaveInFlight: false,
                isAutoSaveInFlight: false,
                currentWriteRequestId: ""
            }))));
        }
        if (!bCreate && (!oDelta || !oDelta.client_version) && !iClientVersion) {
            return Promise.resolve(Result.fail({ message: "Detail snapshot is stale; reload required", code: DETAIL_CODES.MISSING_CLIENT_VERSION }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.FAILED)
            ]));
        }
        if (!bCreate && oDelta && !oDelta.client_version && iClientVersion) {
            oDelta = Object.assign({}, oDelta, { client_version: iClientVersion });
        }
        if (!bCreate && (!sSessionGuid || sLockState !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED)) {
            return Promise.resolve(Result.fail({ message: "Active lock is required before save", code: DETAIL_CODES.LOCK_REQUIRED }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.FAILED)
            ]));
        }

        return DetailAttachmentDeltaRuntime.serializeStagedAttachments(aCurrentAttachments, bCreate ? "" : sRootId).then(function (aStagedPayload) {
            var oUnifiedDelta = DetailAttachmentDeltaRuntime.mergeDeltaAttachments(oDelta, aStagedPayload);
            var pSave = bCreate
                ? Promise.resolve(oRepo.createChecklist({ delta: oUnifiedDelta, sessionGuid: sSessionGuid }))
                : Promise.resolve(oRepo.saveChecklist(DetailRuntimePayload.saveRequest({
                    rootId: sRootId,
                    delta: oUnifiedDelta,
                    sessionGuid: sSessionGuid,
                    attachments: []
                })));

            return pSave.then(function (oSaved) {
                var sNow = new Date().toISOString();
                var oInitialSavedSnapshot = DetailSaveRuntime.normalizeOverallResult(
                    DetailSaveRuntime.preserveBasicFields((oSaved && oSaved.serverSnapshot) || oCurrent || {}, oCurrent, oSnapshot)
                );
                var sServerRootId = String((oInitialSavedSnapshot && (oInitialSavedSnapshot.pcct_uuid || oInitialSavedSnapshot.RootKey || oInitialSavedSnapshot.rootKey || oInitialSavedSnapshot.Key || (oInitialSavedSnapshot.root && oInitialSavedSnapshot.root.id))) || "").trim();
                var pLockAcquire = Promise.resolve(null);
                if (bCreate && sServerRootId && !CreateSentinel.isCreateId(sServerRootId) && oLock && typeof oLock.acquire === "function" && sSessionGuid) {
                    pLockAcquire = Promise.resolve(oLock.acquire(DetailRuntimePayload.lockRequest({
                        rootId: sServerRootId,
                        sessionGuid: sSessionGuid
                    }, null, StatePaths))).catch(function () {
                        return null;
                    });
                }

                return Promise.all([
                    DetailAttachmentSaveRuntime.syncAfterSave({
                        repo: oRepo,
                        rootId: sRootId,
                        createMode: bCreate,
                        currentAttachments: aCurrentAttachments,
                        savedResult: oSaved,
                        currentChecklist: oCurrent,
                        savedSnapshot: oInitialSavedSnapshot,
                        baseSnapshot: oSnapshot,
                        ctx: mCtx,
                        serverRootId: sServerRootId,
                        hasStagedPayload: aStagedPayload.length > 0
                    }),
                    pLockAcquire
                ]).then(function (aPostSave) {
                    var oAttachmentSync = aPostSave[0];
                    var aSyncedAttachments = oAttachmentSync.attachments;
                    var oLockResult = aPostSave[1];
                    var oSavedSnapshot = DetailSaveRuntime.normalizeOverallResult(
                        DetailSaveRuntime.preserveBasicFields(oAttachmentSync.snapshot, oCurrent, oSnapshot)
                    );
                    var oSelectedSnapshot = CloneUtil.clone(oAttachmentSync.selectedSnapshot, {});
                    return writeDetailCache(oCacheWrite, sServerRootId, oSavedSnapshot, mCtx).then(function () {
                        var aEffects = [
                            Effects.toast(DetailContracts.OBJECT_SAVED, "success"),
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
                        if (sServerRootId && !CreateSentinel.isCreateId(sServerRootId)) {
                            aEffects.push(Effects.modelPatch(
                                STATE_MODEL,
                                ModelPathContracts.SEARCH_RETURN_CONTEXT,
                                buildSearchReturnContext(
                                    bCreate ? SearchReturnRediscoveryRuntime.MODES.CREATE : SearchReturnRediscoveryRuntime.MODES.SAVE,
                                    sServerRootId,
                                    oCurrent,
                                    oSelectedChecklist,
                                    oSavedSnapshot
                                )
                            ));
                            aEffects.push(Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT_ID, sServerRootId));
                            if (bCreate) {
                                var bLockAcquired = !!(oLockResult && oLockResult.ok);
                                if (bLockAcquired) {
                                    aEffects = aEffects.concat(DetailPostOpenRuntime.buildEditableDetailEffects(sServerRootId, {
                                        snapshot: oSelectedSnapshot,
                                        autosaveEnabled: true
                                    }));
                                } else {
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, sServerRootId));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, ModelPathContracts.SELECTED_ID, sServerRootId));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, sServerRootId));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
                                    aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false));
                                }
                                aEffects.push(Effects.navigate(NavigationContracts.ROUTES.DETAIL, { id: sServerRootId }, true));
                            } else {
                                aEffects.push(Effects.modelPatch(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, sServerRootId));
                                aEffects.push(Effects.modelPatch(STATE_MODEL, ModelPathContracts.SELECTED_ID, sServerRootId));
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
