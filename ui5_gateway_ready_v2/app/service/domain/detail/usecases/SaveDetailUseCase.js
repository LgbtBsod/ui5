sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/service/domain/detail/DetailSaveRuntimeSupport",
    "checklist/app/service/domain/shared/UseCaseInputUtils",
    "checklist/app/service/domain/shared/StatePaths",
    "checklist/app/util/DeltaPayloadBuilder",
    "checklist/app/util/CreateSentinel"
], function (UseCase, Result, Effects, DetailSaveRuntimeSupport, UseCaseInputUtils, StatePaths, DeltaPayloadBuilder, CreateSentinel) {
    "use strict";

    function SaveDetailUseCase() {
        UseCase.call(this, "SaveDetailUseCase");
    }

    SaveDetailUseCase.prototype = Object.create(UseCase.prototype);
    SaveDetailUseCase.prototype.constructor = SaveDetailUseCase;

    function stagedAttachments(oChecklist) {
        return ((oChecklist && oChecklist.attachments) || []).filter(function (oAttachment) {
            return !!(oAttachment && oAttachment.staged && oAttachment._file);
        });
    }

    function syncDraftAttachments(oRepo, sRootId, aAttachments) {
        var aPending = Array.isArray(aAttachments) ? aAttachments : [];
        if (!sRootId || !oRepo || typeof oRepo.uploadAttachment !== "function" || !aPending.length) {
            return Promise.resolve([]);
        }

        function revokeLocalUrl(oAttachment) {
            var sUrl = oAttachment && oAttachment.localObjectUrl;
            if (sUrl && typeof window !== "undefined" && window.URL && typeof window.URL.revokeObjectURL === "function") {
                window.URL.revokeObjectURL(sUrl);
            }
        }

        return aPending.reduce(function (oPromise, oAttachment) {
            return oPromise.then(function () {
                return Promise.resolve(oRepo.uploadAttachment({
                    rootId: sRootId,
                    clientRowId: oAttachment.client_row_id || oAttachment.AttachmentKey || oAttachment.Key,
                    file: oAttachment._file,
                    fileMeta: {
                        fileName: oAttachment.FileName || oAttachment.fileName || "",
                        mimeType: oAttachment.MimeType || oAttachment.mimeType || "application/octet-stream",
                        fileSize: oAttachment.FileSize || oAttachment.fileSize || 0,
                        categoryKey: oAttachment.CategoryKey || oAttachment.categoryKey || "GEN",
                        folderKey: oAttachment.FolderKey || sRootId
                    }
                }));
            });
        }, Promise.resolve(null)).catch(function () {
            return null;
        }).then(function () {
            aPending.forEach(revokeLocalUrl);
            if (typeof oRepo.loadAttachments === "function") {
                return oRepo.loadAttachments({ rootId: sRootId }).then(function (oLoaded) {
                    return (oLoaded && oLoaded.attachments) || [];
                }).catch(function () {
                    return [];
                });
            }
            return [];
        });
    }

    function readSelectedChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && typeof oUiState.get === "function" && oUiState.get("selected", "/")) || null;
    }

    function readCurrentChecklist(mCtx) {
        return readSelectedChecklist(mCtx) || DetailSaveRuntimeSupport.readCurrentChecklist(mCtx);
    }

    SaveDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var oUiState = mCtx && mCtx.uiState;
        var oCurrent = readCurrentChecklist(mCtx);
        var oSnapshot = DetailSaveRuntimeSupport.readBaseSnapshot(mCtx);
        var oDelta = (mInput && mInput.delta) || DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oSnapshot);
        var oRepo = mCtx && mCtx.repo;
        var oLock = mCtx && mCtx.lock;
        var sMode = String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE)) || "").toUpperCase();
        var bCreate = CreateSentinel.isCreateId(sRootId) || sMode === "CREATE";
        var iClientVersion = DetailSaveRuntimeSupport.resolveVersionNumber(oCurrent, oSnapshot);
        var sSessionGuid = DetailSaveRuntimeSupport.readSessionGuid(mCtx, StatePaths);
        var sLockState = DetailSaveRuntimeSupport.readLockState(mCtx, StatePaths);
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
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "SAVED"),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false)
            ]));
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
        if (!bCreate && (!sSessionGuid || sLockState !== "LOCKED")) {
            return Promise.resolve(Result.fail({ message: "Active lock is required before save", code: "LOCK_REQUIRED" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")
            ]));
        }

        var pSave = bCreate
            ? Promise.resolve(oRepo.createChecklist({ delta: oCurrent }))
            : Promise.resolve(oRepo.saveChecklist({ rootId: sRootId, delta: oDelta, sessionGuid: sSessionGuid }));

        return pSave.then(function (oSaved) {
            var sNow = new Date().toISOString();
            var oInitialSavedSnapshot = DetailSaveRuntimeSupport.preserveBasicFields((oSaved && oSaved.serverSnapshot) || oCurrent || {}, oCurrent, oSnapshot);
            var sServerRootId = String((oInitialSavedSnapshot && (oInitialSavedSnapshot.pcct_uuid || oInitialSavedSnapshot.RootKey || oInitialSavedSnapshot.rootKey || oInitialSavedSnapshot.Key || (oInitialSavedSnapshot.root && oInitialSavedSnapshot.root.id))) || "").trim();
            var pLockAcquire = Promise.resolve(null);
            var pAttachmentSync = Promise.resolve(aCurrentAttachments);

            if (bCreate && sServerRootId && !CreateSentinel.isCreateId(sServerRootId)) {
                pAttachmentSync = syncDraftAttachments(oRepo, sServerRootId, stagedAttachments(oCurrent));
            }

            if (bCreate && sServerRootId && !CreateSentinel.isCreateId(sServerRootId) && oLock && typeof oLock.acquire === "function" && sSessionGuid) {
                pLockAcquire = Promise.resolve(oLock.acquire({
                    rootId: sServerRootId,
                    sessionGuid: sSessionGuid
                })).catch(function () {
                    return null;
                });
            }

            return Promise.all([pAttachmentSync, pLockAcquire]).then(function (aPostSave) {
                var aSyncedAttachments = Array.isArray(aPostSave[0]) ? aPostSave[0] : aCurrentAttachments;
                var oLockResult = aPostSave[1];
                var oSavedSnapshot = DetailSaveRuntimeSupport.preserveBasicFields(oInitialSavedSnapshot, oCurrent, oSnapshot);
                var oSelectedSnapshot = Object.assign({}, oSavedSnapshot, { attachments: aSyncedAttachments });
                var aEffects = [
                    Effects.toast("objectSaved", "success"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "SAVED"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, sNow),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
                    Effects.modelPatch("state", StatePaths.SAVE_IN_FLIGHT, false),
                    Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch("uiState", "/_detailSnapshot", oSavedSnapshot),
                    Effects.modelPatch("uiState", "/_detailCurrent", oSavedSnapshot),
                    Effects.modelPatch("selected", "/", oSelectedSnapshot),
                    Effects.modelPatch("selected", "/attachments", aSyncedAttachments)
                ];
                if (sServerRootId && !CreateSentinel.isCreateId(sServerRootId)) {
                    aEffects.push(Effects.modelPatch("state", "/activeObjectId", sServerRootId));
                    aEffects.push(Effects.modelPatch("state", "/selectedId", sServerRootId));
                    aEffects.push(Effects.modelPatch("selected", "/root/id", sServerRootId));
                    if (bCreate) {
                        var bLockAcquired = !!(oLockResult && oLockResult.ok);
                        aEffects.push(Effects.modelPatch("state", "/postOpenHydratedRootId", sServerRootId));
                        aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, bLockAcquired ? "EDIT" : "READ"));
                        aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, bLockAcquired ? "LOCKED" : "IDLE"));
                        aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, bLockAcquired));
                        aEffects.push(Effects.navigate("detail", { id: sServerRootId }, true));
                    }
                }
                return Result.ok({ serverSnapshot: oSavedSnapshot || {}, selectedSnapshot: oSelectedSnapshot || {}, savedAt: sNow, lock: oLockResult || null }, aEffects);
            });
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("state", StatePaths.SAVE_IN_FLIGHT, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR"),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false)
            ]);
        });
    };

    return SaveDetailUseCase;
});