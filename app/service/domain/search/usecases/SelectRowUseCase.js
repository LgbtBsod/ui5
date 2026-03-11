sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchSelectionEffects",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (UseCase, Result, Effects, SearchSelectionEffects, CreateSentinel, StatePaths) {
    "use strict";

    function readSessionGuid(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String((oUiState && oUiState.get("state", StatePaths.SESSION_ID)) || "").trim();
    }

    function resolveCopiedRootId(oSnapshot) {
        return String(
            (((oSnapshot || {}).root || {}).id) ||
            (((oSnapshot || {}).root || {}).Key) ||
            (((oSnapshot || {}).root || {}).RootKey) ||
            (oSnapshot && oSnapshot.RootKey) ||
            (oSnapshot && oSnapshot.Key) ||
            ""
        ).trim();
    }

    function SelectRowUseCase() {
        UseCase.call(this, "SelectRowUseCase");
    }

    SelectRowUseCase.prototype = Object.create(UseCase.prototype);
    SelectRowUseCase.prototype.constructor = SelectRowUseCase;

    SelectRowUseCase.prototype.execute = function (mInput, mCtx) {
        var sIntent = String((mInput && mInput.intent) || "open");
        var oSmart = mCtx && mCtx.smartControls;
        var oRepo = mCtx && mCtx.repo;
        var sRootId = String((mInput && mInput.rootId) || (oSmart && oSmart.getSelectedRowKey && oSmart.getSelectedRowKey()) || "").trim();

        if (sIntent === "create") {
            return Promise.resolve(Result.ok({ mode: "create" }, [Effects.navigate("detail", { id: CreateSentinel.toRouteId() }, false)]));
        }

        if (!sRootId) {
            var sMissingKey = sIntent === "delete" ? "nothingToDelete" : "nothingToCopy";
            return Promise.resolve(Result.fail({ message: "No selected row", code: "NO_SELECTION" }, [Effects.toast(sMissingKey, "warning")]));
        }

        if (sIntent === "delete") {
            if (!oRepo || typeof oRepo.deleteChecklist !== "function") {
                return Promise.resolve(Result.fail({ message: "Delete unavailable", code: "DELETE_UNAVAILABLE" }));
            }
            return Promise.resolve(oRepo.checkChecklistPermission({
                rootId: sRootId,
                activity: "06"
            })).then(function (oPermission) {
                if (!oPermission || oPermission.canDelete !== true) {
                    return Result.fail({ message: "No permission to delete checklist", code: "NO_DELETE_PERMISSION" }, [
                        Effects.toast("detailDeletePermissionDenied", "warning")
                    ]);
                }
                return Promise.resolve(oRepo.deleteChecklist({ rootId: sRootId })).then(function () {
                    if (oSmart && typeof oSmart.rebindSearchTable === "function") {
                        oSmart.rebindSearchTable();
                    }
                    return Result.ok({ selectedRootId: sRootId, intent: sIntent }, SearchSelectionEffects.buildSelectionResetEffects().concat([
                        Effects.toast("checklistDeleted", "success")
                    ]));
                });
            }).catch(function (oError) {
                return Result.fail(oError);
            });
        }

        if (sIntent === "copy") {
            var sSessionGuid = readSessionGuid(mCtx);
            if (!sSessionGuid) {
                return Promise.resolve(Result.fail({ message: "Session unavailable", code: "SESSION_UNAVAILABLE" }, [Effects.warn("sessionUnavailableMessage")]));
            }
            if (!oRepo || typeof oRepo.copyChecklist !== "function") {
                return Promise.resolve(Result.fail({ message: "Copy unavailable", code: "COPY_UNAVAILABLE" }));
            }
            return Promise.resolve(oRepo.copyChecklist({ rootId: sRootId, sessionGuid: sSessionGuid })).then(function (oCopyResult) {
                var oSnapshot = (oCopyResult && oCopyResult.serverSnapshot) || {};
                var sCopiedRootId = resolveCopiedRootId(oSnapshot);
                if (!sCopiedRootId) {
                    return Result.fail({ message: "Copied checklist id missing", code: "COPY_INVALID_RESPONSE" });
                }
                if (oSmart && typeof oSmart.rebindSearchTable === "function") {
                    oSmart.rebindSearchTable();
                }
                return Result.ok({ selectedRootId: sRootId, intent: sIntent }, [
                    Effects.modelPatch("selected", "/", oSnapshot),
                    Effects.modelPatch("snapshot", "/", oSnapshot),
                    Effects.modelPatch("state", "/activeObjectId", sCopiedRootId),
                    Effects.modelPatch("state", "/selectedId", sCopiedRootId),
                    Effects.modelPatch("state", "/postOpenHydratedRootId", sCopiedRootId),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "EDIT"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "LOCKED"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, true),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
                    Effects.toast("checklistCopied", "success"),
                    Effects.navigate("detail", { id: sCopiedRootId }, false)
                ]);
            }).catch(function (oError) {
                return Result.fail(oError);
            });
        }

        return Promise.resolve(Result.ok({ selectedRootId: sRootId, intent: sIntent }, [
            Effects.navigate("detail", { id: sRootId }, false)
        ]));
    };

    return SelectRowUseCase;
});
