sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchSelectionEffects",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPostOpenRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailUseCaseConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailMessageKeyConstants"
], function (Result, Effects, SearchSelectionEffects, CreateSentinel, StatePaths, DetailPostOpenRuntime, NavigationContracts, ModelContracts, DetailUseCaseConstants, DetailMessageKeyConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_CODES = DetailUseCaseConstants.CODES;
    var DETAIL_MESSAGE_KEYS = DetailMessageKeyConstants;
    var ACCESS_OPERATIONS = DetailUseCaseConstants.ACCESS_OPERATIONS;

    function readSessionGuid(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String((oUiState && oUiState.get(STATE_MODEL, StatePaths.SESSION_ID)) || "").trim();
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
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var sIntent = String((mInput && mInput.intent) || "open");
        var oSmart = mCtx && mCtx.smartControls;
        var oRepo = mCtx && mCtx.repo;
        var sRootId = String((mInput && mInput.rootId) || (oSmart && oSmart.getSelectedRowKey && oSmart.getSelectedRowKey()) || "").trim();

        if (sIntent === "create") {
            return Promise.resolve(Result.ok({ mode: "create" }, [Effects.navigate(NavigationContracts.ROUTES.DETAIL, { id: CreateSentinel.toRouteId() }, false)]));
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
                activity: ACCESS_OPERATIONS.DELETE
            })).then(function (oPermission) {
                if (!oPermission || oPermission.canDelete !== true) {
                    return Result.fail({ message: "No permission to delete checklist", code: DETAIL_CODES.NO_DELETE_PERMISSION }, [
                        Effects.toast(DETAIL_MESSAGE_KEYS.DETAIL_DELETE_PERMISSION_DENIED, "warning")
                    ]);
                }
                return Promise.resolve(oRepo.deleteChecklist({ rootId: sRootId })).then(function () {
                    if (oSmart && typeof oSmart.rebindSearchTable === "function") {
                        oSmart.rebindSearchTable();
                    }
                    return Result.ok({ selectedRootId: sRootId, intent: sIntent }, SearchSelectionEffects.buildSelectionResetEffects().concat([
                        Effects.toast(DETAIL_MESSAGE_KEYS.CHECKLIST_DELETED, "success")
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
                return Result.ok({ selectedRootId: sRootId, intent: sIntent }, DetailPostOpenRuntime.buildEditableDetailEffects(sCopiedRootId, {
                    snapshot: oSnapshot
                }).concat([
                    Effects.toast("checklistCopied", "success"),
                    Effects.navigate(NavigationContracts.ROUTES.DETAIL, { id: sCopiedRootId }, false)
                ]));
            }).catch(function (oError) {
                return Result.fail(oError);
            });
        }

        return Promise.resolve(Result.ok({ selectedRootId: sRootId, intent: sIntent }, [
            Effects.navigate(NavigationContracts.ROUTES.DETAIL, { id: sRootId }, false)
        ]));
    }

    return SelectRowUseCase;
});
