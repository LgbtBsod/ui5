sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailSaveSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseInputUtils",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/util/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/util/AttachmentValueCodec",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DomainStatePaths"
], function (UseCase, Result, Effects, DetailSaveSupport, DetailRuntimePayload, UseCaseInputUtils, StatePaths, DeltaPayloadBuilder, CreateSentinel, AttachmentValueCodec, DomainStatePaths) {
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

    function revokeLocalUrl(oAttachment) {
        var sUrl = oAttachment && oAttachment.localObjectUrl;
        if (sUrl && typeof window !== "undefined" && window.URL && typeof window.URL.revokeObjectURL === "function") {
            window.URL.revokeObjectURL(sUrl);
        }
    }

    function buildStagedAttachmentPayload(oAttachment, sRootId) {
        return AttachmentValueCodec.fileToBase64(oAttachment && oAttachment._file).then(function (sValue) {
            return {
                Key: String((oAttachment && (oAttachment.client_row_id || oAttachment.AttachmentKey || oAttachment.Key)) || "").trim(),
                ParentKey: String((oAttachment && (oAttachment.RootKey || oAttachment.rootKey)) || sRootId || "").trim(),
                RootKey: String(sRootId || "").trim(),
                FolderKey: String((oAttachment && (oAttachment.FolderKey || oAttachment.folderKey)) || sRootId || "").trim(),
                CategoryKey: String((oAttachment && (oAttachment.CategoryKey || oAttachment.categoryKey)) || "GEN").trim() || "GEN",
                Type: String((oAttachment && (oAttachment.CategoryKey || oAttachment.categoryKey)) || "GEN").trim() || "GEN",
                FileName: String((oAttachment && (oAttachment.FileName || oAttachment.fileName)) || "").trim(),
                Name: String((oAttachment && (oAttachment.FileName || oAttachment.fileName)) || "").trim(),
                MimeType: String((oAttachment && (oAttachment.MimeType || oAttachment.mimeType)) || "application/octet-stream").trim() || "application/octet-stream",
                Description: String((oAttachment && (oAttachment.Description || oAttachment.description || oAttachment.Desc || oAttachment.desc)) || "").trim(),
                FileSize: Number((oAttachment && (oAttachment.FileSize || oAttachment.fileSize)) || 0) || 0,
                FileSizeContent: Number((oAttachment && (oAttachment.FileSize || oAttachment.fileSize)) || 0) || 0,
                Value: sValue
            };
        });
    }

    function serializeStagedAttachments(aAttachments, sRootId) {
        var aPending = stagedAttachments({ attachments: aAttachments });
        if (!aPending.length) {
            return Promise.resolve([]);
        }
        return Promise.all(aPending.map(function (oAttachment) {
            return buildStagedAttachmentPayload(oAttachment, sRootId);
        }));
    }

    function refreshAttachments(oRepo, sRootId, aCurrentAttachments, bForceReload) {
        if (!sRootId || !oRepo || typeof oRepo.loadAttachments !== "function" || !bForceReload) {
            return Promise.resolve(Array.isArray(aCurrentAttachments) ? aCurrentAttachments : []);
        }
        return oRepo.loadAttachments({ rootId: sRootId }).then(function (oLoaded) {
            return (oLoaded && oLoaded.attachments) || [];
        }).catch(function () {
            return Array.isArray(aCurrentAttachments) ? aCurrentAttachments : [];
        });
    }

    function cleanupStagedAttachmentUrls(aAttachments) {
        (Array.isArray(aAttachments) ? aAttachments : []).forEach(revokeLocalUrl);
    }

    function stripStagedAttachmentInternals(aAttachments) {
        return (Array.isArray(aAttachments) ? aAttachments : []).map(function (oAttachment) {
            var oClean = Object.assign({}, oAttachment || {});
            delete oClean._file;
            delete oClean.localObjectUrl;
            delete oClean.staged;
            return oClean;
        });
    }

    function readSelectedChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && typeof oUiState.get === "function" && oUiState.get("selected", "/")) || null;
    }

    function readCurrentChecklist(mCtx) {
        return readSelectedChecklist(mCtx) || DetailSaveSupport.readCurrentChecklist(mCtx);
    }

    SaveDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var oUiState = mCtx && mCtx.uiState;
        var oCurrent = readCurrentChecklist(mCtx);
        var oSnapshot = DetailSaveSupport.readBaseSnapshot(mCtx);
        var oDelta = (mInput && mInput.delta) || DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oSnapshot);
        var oRepo = mCtx && mCtx.repo;
        var oLock = mCtx && mCtx.lock;
        var sMode = String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE)) || "").toUpperCase();
        var bCreate = CreateSentinel.isCreateId(sRootId) || sMode === "CREATE";
        var iClientVersion = DetailSaveSupport.resolveVersionNumber(oCurrent, oSnapshot);
        var sSessionGuid = DetailSaveSupport.readSessionGuid(mCtx, StatePaths);
        var sLockState = DetailSaveSupport.readLockState(mCtx, StatePaths);
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
        if (!bCreate && (!sSessionGuid || sLockState !== "EDIT_LOCKED")) {
            return Promise.resolve(Result.fail({ message: "Active lock is required before save", code: "LOCK_REQUIRED" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")
            ]));
        }

        return serializeStagedAttachments(aCurrentAttachments, bCreate ? "" : sRootId).then(function (aStagedPayload) {
            var pSave = bCreate
                ? Promise.resolve(oRepo.createChecklist({ delta: oCurrent, attachments: aStagedPayload }))
                : Promise.resolve(oRepo.saveChecklist(DetailRuntimePayload.saveRequest({
                    rootId: sRootId,
                    delta: oDelta,
                    sessionGuid: sSessionGuid,
                    attachments: aStagedPayload
                })));

            return pSave.then(function (oSaved) {
                var sNow = new Date().toISOString();
                var oInitialSavedSnapshot = DetailSaveSupport.preserveBasicFields((oSaved && oSaved.serverSnapshot) || oCurrent || {}, oCurrent, oSnapshot);
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
                    refreshAttachments(oRepo, sServerRootId || sRootId, aCurrentAttachments, bNeedsAttachmentReload),
                    pLockAcquire
                ]).then(function (aPostSave) {
                    var aSyncedAttachments = stripStagedAttachmentInternals(aPostSave[0]);
                    var oLockResult = aPostSave[1];
                    var oSavedSnapshot = DetailSaveSupport.preserveBasicFields(oInitialSavedSnapshot, oCurrent, oSnapshot);
                    var oSelectedSnapshot = Object.assign({}, oSavedSnapshot, { attachments: aSyncedAttachments });
                    var aEffects = [
                    Effects.toast("objectSaved", "success"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "SAVED"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, sNow),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
                    Effects.modelPatch("state", StatePaths.SAVE_IN_FLIGHT, false),
                    Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch("snapshot", "/", oSavedSnapshot),
                    Effects.modelPatch("selected", "/", oSelectedSnapshot),
                    Effects.modelPatch("selected", "/attachments", aSyncedAttachments)
                    ];
                    cleanupStagedAttachmentUrls(aCurrentAttachments);
                    if (sServerRootId && !CreateSentinel.isCreateId(sServerRootId)) {
                        aEffects.push(Effects.modelPatch("state", DomainStatePaths.ACTIVE_OBJECT_ID, sServerRootId));
                        aEffects.push(Effects.modelPatch("state", DomainStatePaths.SELECTED_ID, sServerRootId));
                        aEffects.push(Effects.modelPatch("selected", "/root/id", sServerRootId));
                        if (bCreate) {
                            var bLockAcquired = !!(oLockResult && oLockResult.ok);
                            aEffects.push(Effects.modelPatch("state", DomainStatePaths.POST_OPEN_HYDRATED_ROOT_ID, sServerRootId));
                            aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, bLockAcquired ? "EDIT" : "READ"));
                            aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, bLockAcquired ? "EDIT_LOCKED" : "READ_ONLY"));
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
        });
    };

    return SaveDetailUseCase;
});
