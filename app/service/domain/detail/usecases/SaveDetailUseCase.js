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
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/ChecklistValidationService",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/ValidationPathMap",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPostOpenRuntime"
], function (Result, Effects, DetailSaveRuntime, DetailRuntimePayload, UseCaseValue, StatePaths, DeltaPayloadBuilder, CreateSentinel, DetailAttachmentDeltaRuntime, DetailAttachmentSaveRuntime, DetailStateAccess, ChecklistValidationService, ValidationPathMap, ModelPathContracts, ViewPathContracts, WorkflowContracts, DetailPersistenceRuntime, DetailPostOpenRuntime) {
    "use strict";

    function SaveDetailUseCase() {
        return {
            execute: execute
        };
    }

function readSelectedChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && typeof oUiState.get === "function" && oUiState.get("selected", "/")) || null;
    }

    function readCurrentChecklist(mCtx) {
        return readSelectedChecklist(mCtx) || DetailSaveRuntime.readCurrentChecklist(mCtx);
    }

    function buildBlockedCreateValidationResult(mCtx, oValidation) {
        var aMissingPaths = (oValidation && oValidation.missingPaths) || [];
        var mMissing = ValidationPathMap.toMissingMap(aMissingPaths);
        var aMissingKeys = Object.keys(mMissing).filter(function (sKey) {
            return !!mMissing[sKey];
        });

        return Result.ok({
            blocked: true,
            reason: "VALIDATION_FAILED",
            missingPaths: aMissingPaths
        }, [
            Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
            Effects.modelPatch("state", StatePaths.SAVE_IN_FLIGHT, false),
            Effects.modelPatch("view", ViewPathContracts.VALIDATION_SHOWN, true),
            Effects.modelPatch("view", ViewPathContracts.VALIDATION_MISSING, mMissing),
            Effects.modelPatch("state", StatePaths.VALIDATION_SUMMARY, {
                hasErrors: true,
                missingPaths: aMissingPaths,
                missingKeys: aMissingKeys,
                source: "save",
                firstMissingPath: aMissingPaths[0] || "",
                firstMissingKey: aMissingKeys[0] || ""
            }),
            Effects.toast("checklistValidationFailedToast", "warning")
        ]);
    }

    function execute(mInput, mCtx) {
        var sRootId = UseCaseValue.rootId(mInput);
        var oUiState = mCtx && mCtx.uiState;
        var oCurrent = readCurrentChecklist(mCtx);
        var oSnapshot = DetailSaveRuntime.readBaseSnapshot(mCtx);
        var oRepo = mCtx && mCtx.repo;
        var oLock = mCtx && mCtx.lock;
        var sMode = WorkflowContracts.normalizeEditMode(oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        var bCreate = CreateSentinel.isCreateId(sRootId) || sMode === "CREATE";
        var oDelta = (mInput && mInput.delta) || (bCreate ? DeltaPayloadBuilder.buildCreatePayload(oCurrent) : DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oSnapshot));
        var iClientVersion = DetailSaveRuntime.resolveVersionNumber(oCurrent, oSnapshot);
        var sSessionGuid = DetailSaveRuntime.readSessionGuid(mCtx, StatePaths);
        var sLockState = DetailSaveRuntime.readLockState(mCtx, StatePaths);
        var aCurrentAttachments = DetailStateAccess.readWorkingAttachments(mCtx);
        var oValidation = null;

        if (!oRepo) {
            return Promise.resolve(Result.fail({ message: "Save handler unavailable", code: "SAVE_HANDLER_MISSING" }));
        }
        if (!bCreate && typeof oRepo.saveChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Save handler unavailable", code: "SAVE_HANDLER_MISSING" }));
        }
        if (bCreate && typeof oRepo.createChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Create handler unavailable", code: "CREATE_HANDLER_MISSING" }));
        }
        if (bCreate) {
            oValidation = ChecklistValidationService.validateRequiredFields(oCurrent, {
                requiredFields: DetailStateAccess.readRequiredFields(mCtx)
            });
            if (oValidation.unavailable) {
                return Promise.resolve(Result.fail({
                    message: "Validation rules are not loaded yet",
                    code: "REQUIRED_FIELDS_UNAVAILABLE"
                }, [
                    Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch("state", StatePaths.SAVE_IN_FLIGHT, false),
                    Effects.modelPatch("view", ViewPathContracts.VALIDATION_SHOWN, false),
                    Effects.modelPatch("view", ViewPathContracts.VALIDATION_MISSING, {}),
                    Effects.toast("checklistValidationUnavailableToast", "warning")
                ]));
            }
            if (!oValidation.valid) {
                return Promise.resolve(buildBlockedCreateValidationResult(mCtx, oValidation));
            }
        }
        if (!bCreate && !oDelta) {
            return Promise.resolve(Result.ok({ saved: false, skipped: true, reason: "NO_CHANGES" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false)
            ].concat(DetailPersistenceRuntime.dirtyEffects(false, {
                messageKey: "persistenceNoChanges",
                isManualSaveInFlight: false,
                isAutoSaveInFlight: false,
                currentWriteRequestId: ""
            }))));
        }
        if (!bCreate && (!oDelta || !oDelta.client_version) && !iClientVersion) {
            return Promise.resolve(Result.fail({ message: "Detail snapshot is stale; reload required", code: "MISSING_CLIENT_VERSION" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.FAILED)
            ]));
        }
        if (!bCreate && oDelta && !oDelta.client_version && iClientVersion) {
            oDelta = Object.assign({}, oDelta, { client_version: iClientVersion });
        }
        if (!bCreate && (!sSessionGuid || sLockState !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED)) {
            return Promise.resolve(Result.fail({ message: "Active lock is required before save", code: "LOCK_REQUIRED" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.FAILED)
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
                var bNeedsAttachmentReload = bCreate || aStagedPayload.length > 0;

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
                    var oSelectedSnapshot = Object.assign({}, oAttachmentSync.selectedSnapshot);
                    var aEffects = [
                        Effects.toast("objectSaved", "success"),
                        Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
                        Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                        Effects.modelPatch("snapshot", "/", oSavedSnapshot)
                    ];
                    aEffects = aEffects.concat(oAttachmentSync.effects);
                    aEffects = aEffects.concat(DetailPersistenceRuntime.successEffects("manual", sNow, {
                        hasValidLock: WorkflowContracts.normalizeEditMode(sMode) === WorkflowContracts.EDIT_MODES.EDIT,
                        lockOwnerSessionMatches: WorkflowContracts.normalizeEditMode(sMode) === WorkflowContracts.EDIT_MODES.EDIT,
                        lastLockRefreshAt: oSaved && oSaved.lock_refreshed ? sNow : null
                    }));
                    if (sServerRootId && !CreateSentinel.isCreateId(sServerRootId)) {
                        aEffects.push(Effects.modelPatch("selected", "/root/id", sServerRootId));
                        if (bCreate) {
                            var bLockAcquired = !!(oLockResult && oLockResult.ok);
                            if (bLockAcquired) {
                                aEffects = aEffects.concat(DetailPostOpenRuntime.buildEditableDetailEffects(sServerRootId, {
                                    snapshot: oSelectedSnapshot,
                                    autosaveEnabled: true
                                }));
                            } else {
                                aEffects.push(Effects.modelPatch("state", ModelPathContracts.ACTIVE_OBJECT_ID, sServerRootId));
                                aEffects.push(Effects.modelPatch("state", ModelPathContracts.SELECTED_ID, sServerRootId));
                                aEffects.push(Effects.modelPatch("state", ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, sServerRootId));
                                aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
                                aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
                                aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false));
                            }
                            aEffects.push(Effects.navigate("detail", { id: sServerRootId }, true));
                        } else {
                            aEffects.push(Effects.modelPatch("state", ModelPathContracts.ACTIVE_OBJECT_ID, sServerRootId));
                            aEffects.push(Effects.modelPatch("state", ModelPathContracts.SELECTED_ID, sServerRootId));
                        }
                    }
                return Result.ok({ serverSnapshot: oSavedSnapshot || {}, selectedSnapshot: oSelectedSnapshot || {}, savedAt: sNow, lock: oLockResult || null }, aEffects);
                });
            }).catch(function (oError) {
                var oClassification = DetailPersistenceRuntime.classifyError(oError);
                var oFailure = DetailPersistenceRuntime.failureEffects("manual", oError, {
                    hasValidLock: !DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy) && sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED,
                    lockOwnerSessionMatches: !DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy)
                });
                return Result.fail(oError, [
                    Effects.modelPatch("state", StatePaths.SAVE_IN_FLIGHT, false),
                    Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false)
                ].concat(oFailure.effects));
            });
        });
    }

    return SaveDetailUseCase;
});