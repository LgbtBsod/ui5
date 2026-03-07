sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/framework/Effects",
    "sap_ui5/service/domain/detail/DetailSaveRuntimeSupport",
    "sap_ui5/service/domain/shared/UseCaseInputUtils",
    "sap_ui5/service/domain/shared/StatePaths",
    "sap_ui5/util/DeltaPayloadBuilder"
], function (UseCase, Result, Effects, DetailSaveRuntimeSupport, UseCaseInputUtils, StatePaths, DeltaPayloadBuilder) {
    "use strict";

    function AutosaveDetailUseCase() {
        UseCase.call(this, "AutosaveDetailUseCase");
    }

    function mapFieldDelta(mInput, oCurrent) {
        var mFieldMap = { LPC_KEY: "Lpc", PROF_KEY: "Profession" };
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
        return readSelectedChecklist(mCtx) || DetailSaveRuntimeSupport.readCurrentChecklist(mCtx);
    }

    function resolveDelta(mInput, mCtx) {
        if (mInput && mInput.delta && Object.keys(mInput.delta).length) {
            return mInput.delta;
        }
        var oCurrent = readCurrentChecklist(mCtx);
        var oSnapshot = DetailSaveRuntimeSupport.readBaseSnapshot(mCtx);
        var oMappedCurrent = mapFieldDelta(mInput, oCurrent) || oCurrent;
        return DeltaPayloadBuilder.buildDeltaPayload(oMappedCurrent, oSnapshot) || null;
    }

    function isAutosaveAllowed(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var sEditMode = String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_EDIT_MODE)) || "").toUpperCase();
        var sLockStatus = String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_LOCK_STATUS)) || "").toUpperCase();
        var bDirty = !!(oUiState && oUiState.get("state", StatePaths.WORKFLOW_DIRTY));
        return sEditMode === "EDIT" && sLockStatus === "LOCKED" && bDirty;
    }

    function resolveClientVersion(oDelta, mCtx) {
        var oSnapshot = readCurrentChecklist(mCtx);
        var oCurrent = { root: { version_number: oDelta && oDelta.client_version } };
        return DetailSaveRuntimeSupport.resolveVersionNumber(
            oCurrent,
            oSnapshot
        );
    }

    AutosaveDetailUseCase.prototype = Object.create(UseCase.prototype);
    AutosaveDetailUseCase.prototype.constructor = AutosaveDetailUseCase;

    AutosaveDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oDelta;
        var sSessionGuid = DetailSaveRuntimeSupport.readSessionGuid(mCtx, StatePaths);

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

        return Promise.resolve(oRepo.autosaveChecklist({ rootId: sRootId, delta: oDelta, sessionGuid: sSessionGuid })).then(function (oSaved) {
            var sAt = (oSaved && oSaved.autosavedAt) || new Date().toISOString();
            var oCurrentChecklist = readCurrentChecklist(mCtx);
            var aCurrentAttachments = Array.isArray((oCurrentChecklist && oCurrentChecklist.attachments) || null) ? oCurrentChecklist.attachments : [];
            var oSavedSnapshot = (oSaved && oSaved.serverSnapshot) || oCurrentChecklist;
            var oSelectedSnapshot = Object.assign({}, oSavedSnapshot, { attachments: aCurrentAttachments });
            return Result.ok({ autosavedAt: sAt }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "SAVED"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, sAt),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
                Effects.modelPatch("selected", "/", oSelectedSnapshot),
                Effects.modelPatch("selected", "/attachments", aCurrentAttachments),
                Effects.modelPatch("uiState", "/_detailSnapshot", oSavedSnapshot),
                Effects.modelPatch("uiState", "/_detailCurrent", oSavedSnapshot),
                Effects.toast("autosaveSaved", "success")
            ]);
        }).catch(function (oError) {
            return Result.fail(oError, [Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "ERROR")]);
        });
    };

    return AutosaveDetailUseCase;
});