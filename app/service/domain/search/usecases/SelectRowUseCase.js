sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchSelectionEffects",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPostOpenRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (Result, Effects, SearchSelectionEffects, CreateSentinel, StatePaths, DetailPostOpenRuntime, NavigationContracts, ModelContracts, DetailContracts, MessageCodeConstants, MessageKeyConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_CODES = MessageCodeConstants.DETAIL;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;
    var DETAIL_VIEW_KEYS = MessageKeyConstants.VIEW;
    var SEARCH_MESSAGE_KEYS = MessageKeyConstants.SEARCH;
    var ACCESS_OPERATIONS = DetailContracts.ACCESS_OPERATIONS;

    function readSessionGuid(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String((oUiState && oUiState.get(STATE_MODEL, StatePaths.SESSION_ID)) || "").trim();
    }

    function resolveCopiedDbKey(oSnapshot) {
        return String(
            (((oSnapshot || {}).root || {}).id) ||
            (((oSnapshot || {}).root || {}).Key) ||
            (((oSnapshot || {}).root || {}).DB_KEY) ||
            (oSnapshot && oSnapshot.DB_KEY) ||
            (oSnapshot && oSnapshot.Key) ||
            ""
        ).trim();
    }

    function SelectRowUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

    function execute(mInput, mCtx) {
        var sIntent = String((mInput && mInput.intent) || "open");
        var oSmart = mCtx && mCtx.smartControls;
        var oRepo = mCtx && mCtx.repo;
        var sDbKey = String((mInput && (mInput.dbKey || mInput.DB_KEY || mInput.rootId)) || (oSmart && oSmart.getSelectedRowKey && oSmart.getSelectedRowKey()) || "").trim();

        if (sIntent === "create") {
            return Promise.resolve(Result.ok({ mode: "create" }, [Effects.navigate(NavigationContracts.ROUTES.DETAIL, { id: CreateSentinel.toRouteId() }, false)]));
        }

        if (!sDbKey) {
            var sMissingKey = sIntent === "delete" ? DETAIL_VIEW_KEYS.NOTHING_TO_DELETE : SEARCH_MESSAGE_KEYS.NOTHING_TO_COPY;
            return Promise.resolve(Result.fail({ messageKey: sMissingKey, code: "NO_SELECTION" }, [Effects.toast(sMissingKey, "warning")]));
        }

        if (sIntent === "delete") {
            if (!oRepo || typeof oRepo.deleteChecklist !== "function") {
                return Promise.resolve(Result.fail({ messageKey: DETAIL_MESSAGE_KEYS.DETAIL_DELETE_PERMISSION_DENIED, code: "DELETE_UNAVAILABLE" }));
            }
            return Promise.resolve(oRepo.checkChecklistPermission({
                dbKey: sDbKey,
                activity: ACCESS_OPERATIONS.DELETE
            })).then(function (oPermission) {
                if (!oPermission || oPermission.canDelete !== true) {
                    return Result.fail({ messageKey: DETAIL_MESSAGE_KEYS.DETAIL_DELETE_PERMISSION_DENIED, code: DETAIL_CODES.NO_DELETE_PERMISSION }, [
                        Effects.toast(DETAIL_MESSAGE_KEYS.DETAIL_DELETE_PERMISSION_DENIED, "warning")
                    ]);
                }
                return Promise.resolve(oRepo.deleteChecklist({ dbKey: sDbKey })).then(function () {
                    if (oSmart && typeof oSmart.rebindSearchTable === "function") {
                        oSmart.rebindSearchTable();
                    }
                    return Result.ok({ selectedDbKey: sDbKey, intent: sIntent }, SearchSelectionEffects.buildSelectionResetEffects().concat([
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
                return Promise.resolve(Result.fail({ messageKey: DETAIL_VIEW_KEYS.SESSION_UNAVAILABLE, code: "SESSION_UNAVAILABLE" }, [Effects.warn(DETAIL_VIEW_KEYS.SESSION_UNAVAILABLE)]));
            }
            if (!oRepo || typeof oRepo.copyChecklist !== "function") {
                return Promise.resolve(Result.fail({ messageKey: DETAIL_VIEW_KEYS.GENERIC_OPERATION_FAILED, code: "COPY_UNAVAILABLE" }));
            }
            return Promise.resolve(oRepo.copyChecklist({ dbKey: sDbKey, sessionGuid: sSessionGuid })).then(function (oCopyResult) {
                var oSnapshot = (oCopyResult && oCopyResult.serverSnapshot) || {};
                var sCopiedDbKey = resolveCopiedDbKey(oSnapshot);
                if (!sCopiedDbKey) {
                    return Result.fail({ messageKey: DETAIL_VIEW_KEYS.GENERIC_OPERATION_FAILED, code: "COPY_INVALID_RESPONSE" });
                }
                if (oSmart && typeof oSmart.rebindSearchTable === "function") {
                    oSmart.rebindSearchTable();
                }
                return Result.ok({ selectedDbKey: sDbKey, intent: sIntent }, DetailPostOpenRuntime.buildEditableDetailEffects(sCopiedDbKey, {
                    snapshot: oSnapshot
                }).concat([
                    Effects.toast(SEARCH_MESSAGE_KEYS.CHECKLIST_COPIED, "success"),
                    Effects.navigate(NavigationContracts.ROUTES.DETAIL, { id: sCopiedDbKey }, false)
                ]));
            }).catch(function (oError) {
                return Result.fail(oError);
            });
        }

        return Promise.resolve(Result.ok({ selectedDbKey: sDbKey, intent: sIntent }, [
            Effects.navigate(NavigationContracts.ROUTES.DETAIL, { id: sDbKey }, false)
        ]));
    }

    return SelectRowUseCase;
});
