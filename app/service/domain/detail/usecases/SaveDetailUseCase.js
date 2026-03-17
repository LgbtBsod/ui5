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
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime"
], function (UseCase, Result, Effects, DetailSaveRuntime, DetailRuntimePayload, UseCaseValue, StatePaths, DeltaPayloadBuilder, CreateSentinel, DetailAttachmentDeltaRuntime, ModelPathContracts, WorkflowContracts, DetailPersistenceRuntime) {
    "use strict";

    function SaveDetailUseCase() {
        UseCase.call(this, "SaveDetailUseCase");
    }

    SaveDetailUseCase.prototype = Object.create(UseCase.prototype);
    SaveDetailUseCase.prototype.constructor = SaveDetailUseCase;

    function readSelectedChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && typeof oUiState.get === "function" && oUiState.get("selected", "/")) || null;
    }

    function readCurrentChecklist(mCtx) {
        return readSelectedChecklist(mCtx) || DetailSaveRuntime.readCurrentChecklist(mCtx);
    }

    SaveDetailUseCase.prototype.execute = function (mInput, mCtx) {
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
        var aCurrentAttachments = Array.isArray((oCurrent && oCurrent.attachments) || null) ? oCurrent.attachments : [];

        if (!oRepo) {
            return Promise.resolve(Result.fail({ message: "Save handler unavailable", code: "SAVE_HANDLER_MISSING" }));
        }
        if (!bCreate && typeof oRepo.saveChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Save handler unavailable", code: "SAVE_HANDLER_MISSING" }));
        }
        if (bCreate && typeof oRepo.createChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Create handler unavailable", code: "CREATE_HANDLER_MISSING" }));
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
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")
            ]));
        }
        if (!bCreate && oDelta && !oDelta.client_version && iClientVersion) {
            oDelta = Object.assign({}, oDelta, { client_version: iClientVersion });
        }
        if (!bCreate && (!sSessionGuid || sLockState !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED)) {
            return Promise.resolve(Result.fail({ message: "Active lock is required before save", code: "LOCK_REQUIRED" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")
            ]));
        }

        return DetailAttachmentDeltaRuntime.serializeStagedAttachments(aCurrentAttachments, bCreate ? "" : sRootId).then(function (aStagedPayload) {
            var oUnifiedDelta = DetailAttachmentDeltaRuntime.mergeDeltaAttachments(oDelta, aStagedPayload);
            var pSave = bCreate
                ? Promise.resolve(oRepo.createChecklist({ delta: oUnifiedDelta }))
                : Promise.resolve(oRepo.saveChecklist(DetailRuntimePayload.saveRequest({
                    rootId: sRootId,
                    delta: oUnifiedDelta,
                    sessionGuid: sSessionGuid,
                    attachments: []
                })));

            return pSave.then(function (oSaved) {
                var sNow = new Date().toISOString();
        var oInitialSavedSnapshot = DetailSaveRuntime.preserveBasicFields((oSaved && oSaved.serverSnapshot) || oCurrent || {}, oCurrent, oSnapshot);
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
                    DetailAttachmentDeltaRuntime.refreshAttachments(oRepo, sServerRootId || sRootId, aCurrentAttachments, bNeedsAttachmentReload),
                    pLockAcquire
                ]).then(function (aPostSave) {
                    var aSyncedAttachments = DetailAttachmentDeltaRuntime.stripStagedAttachmentInternals(aPostSave[0]);
                    var oLockResult = aPostSave[1];
        var oSavedSnapshot = DetailSaveRuntime.preserveBasicFields(oInitialSavedSnapshot, oCurrent, oSnapshot);
                    var oSelectedSnapshot = Object.assign({}, oSavedSnapshot, { attachments: aSyncedAttachments });
                    var aEffects = [
                        Effects.toast("objectSaved", "success"),
                        Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
                        Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                        Effects.modelPatch("snapshot", "/", oSavedSnapshot),
                        Effects.modelPatch("selected", "/", oSelectedSnapshot),
                        Effects.modelPatch("selected", "/attachments", aSyncedAttachments)
                    ];
                    aEffects = aEffects.concat(DetailPersistenceRuntime.successEffects("manual", sNow, {
                        hasValidLock: WorkflowContracts.normalizeEditMode(sMode) === WorkflowContracts.EDIT_MODES.EDIT,
                        lockOwnerSessionMatches: WorkflowContracts.normalizeEditMode(sMode) === WorkflowContracts.EDIT_MODES.EDIT,
                        lastLockRefreshAt: oSaved && oSaved.lock_refreshed ? sNow : null
                    }));
                    DetailAttachmentDeltaRuntime.cleanupStagedAttachmentUrls(aCurrentAttachments);
                    if (sServerRootId && !CreateSentinel.isCreateId(sServerRootId)) {
                        aEffects.push(Effects.modelPatch("state", ModelPathContracts.ACTIVE_OBJECT_ID, sServerRootId));
                        aEffects.push(Effects.modelPatch("state", ModelPathContracts.SELECTED_ID, sServerRootId));
                        aEffects.push(Effects.modelPatch("selected", "/root/id", sServerRootId));
                        if (bCreate) {
                            var bLockAcquired = !!(oLockResult && oLockResult.ok);
                            aEffects.push(Effects.modelPatch("state", ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, sServerRootId));
                            aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, bLockAcquired ? WorkflowContracts.EDIT_MODES.EDIT : WorkflowContracts.EDIT_MODES.READ));
                            aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, bLockAcquired ? WorkflowContracts.LOCK_STATES.EDIT_LOCKED : WorkflowContracts.LOCK_STATES.READ_ONLY));
                            aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, bLockAcquired));
                            aEffects.push(Effects.navigate("detail", { id: sServerRootId }, true));
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
    };

    return SaveDetailUseCase;
});
