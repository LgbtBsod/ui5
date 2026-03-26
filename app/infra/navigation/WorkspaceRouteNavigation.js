sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "sap/ui/core/routing/HashChanger",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (CloneUtil, LayoutStateRuntime, ModelStateRuntime, ControllerModelRuntime, StatePaths, NavigationContracts, WorkflowContracts, HashChanger, JsRuntime) {
    "use strict";

    var TYPEOF = JsRuntime.TYPEOF;
    var METHODS = JsRuntime.METHODS;
    var HASH_CHANGER = JsRuntime.HASH_CHANGER;

    function cloneArgs(oArgs) {
        return CloneUtil.clone(oArgs, {});
    }

    function readStateModel(oController) {
        return ControllerModelRuntime.state(oController);
    }

    function buildFallbackIntent() {
        return {
            routeName: NavigationContracts.ROUTES.SEARCH,
            routeArgs: {}
        };
    }

    function isDetailIntent(oIntent) {
        var sRouteName = String(oIntent && oIntent.routeName || "").trim();

        return sRouteName === NavigationContracts.ROUTES.DETAIL;
    }

    function readHashChanger() {
        return HashChanger && HashChanger.getInstance ? HashChanger.getInstance() : null;
    }

    function buildHashFromIntent(oRouter, oIntent) {
        var sRouteName = String(oIntent && oIntent.routeName || "").trim();
        var oRouteArgs = cloneArgs(oIntent && oIntent.routeArgs);
        if (!oRouter || typeof oRouter[METHODS.GET_URL] !== TYPEOF.FUNCTION || !sRouteName || sRouteName === NavigationContracts.ROUTES.SEARCH) {
            return "";
        }
        return String(oRouter[METHODS.GET_URL](sRouteName, oRouteArgs || {}) || "");
    }

    function buildAnalyticsReturnSnapshot(oStateModel, oShellModel, oRouter) {
        var oHashChanger = readHashChanger();
        var oIntent = buildCurrentIntent(oStateModel, oShellModel);
        var sHash = oHashChanger && typeof oHashChanger[HASH_CHANGER.GET_HASH] === TYPEOF.FUNCTION
            ? String(oHashChanger[HASH_CHANGER.GET_HASH]() || "")
            : "";
        var sActiveObjectId = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "") || "").trim();
        var sSelectedId = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.SELECTED_ID, "") || "").trim();
        var sRootId = isDetailIntent(oIntent) ? (sActiveObjectId || sSelectedId) : "";

        if (!sHash) {
            sHash = buildHashFromIntent(oRouter, oIntent);
        }

        return {
            hash: sHash,
            activeObjectId: sRootId,
            selectedId: sRootId
        };
    }

    function buildCurrentIntent(oStateModel, oShellModel) {
        var sRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        var sActiveId = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "") || "").trim() ||
            String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.SELECTED_ID, "") || "").trim();
        var sLayout = LayoutStateRuntime.readLayout(oShellModel, NavigationContracts.LAYOUTS.ONE_COLUMN);

        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS && sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN
            && sActiveId) {
            return {
                routeName: NavigationContracts.ROUTES.DETAIL,
                routeArgs: {
                    id: sActiveId,
                    layout: NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN
                }
            };
        }
        if (sRouteName === NavigationContracts.ROUTES.DETAIL && sActiveId) {
            return {
                routeName: NavigationContracts.ROUTES.DETAIL,
                routeArgs: { id: sActiveId }
            };
        }
        return buildFallbackIntent();
    }

    function setAnalyticsReturnIntent(oController) {
        var oStateModel = readStateModel(oController);
        var oShellModel = ControllerModelRuntime.shell(oController);
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var oSnapshot = buildAnalyticsReturnSnapshot(oStateModel, oShellModel, oRouter);
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        var bRestoreEdit = !!(oSnapshot.activeObjectId && WorkflowContracts.isEditLocked(sMode, sLockState));

        ModelStateRuntime.writeOnModel(oStateModel, "/analyticsNavReturn", {
            hash: oSnapshot.hash,
            activeObjectId: oSnapshot.activeObjectId,
            selectedId: oSnapshot.selectedId,
            restoreEdit: bRestoreEdit
        });

        return oSnapshot;
    }

    function navigateToAnalytics(oController) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var oStateModel = readStateModel(oController);
        var sCurrentRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;

        if (sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            navigateToSearch(oController);
            return;
        }

        setAnalyticsReturnIntent(oController);
        if (oRouter && typeof oRouter[METHODS.NAV_TO] === TYPEOF.FUNCTION) {
            oRouter[METHODS.NAV_TO](NavigationContracts.ROUTES.ANALYTICS, {}, false);
        }
    }

    function navigateBackFromAnalytics(oController) {
        var oStateModel = readStateModel(oController);
        var oReturn = cloneArgs(ModelStateRuntime.readOnModel(oStateModel, "/analyticsNavReturn", {}) || {});
        var oHashChanger = readHashChanger();
        var sTargetRootId;
        var sTargetHash = String((oReturn && oReturn.hash) || "").trim();

        sTargetRootId = String((oReturn && (oReturn.activeObjectId || oReturn.selectedId)) || "").trim();
        if (oReturn && oReturn.restoreEdit && sTargetRootId) {
            ModelStateRuntime.writeOnModel(oStateModel, "/analyticsReturnRestoreEdit", {
                activeObjectId: sTargetRootId,
                selectedId: sTargetRootId,
                requestedAt: new Date().toISOString()
            });
        } else {
            ModelStateRuntime.writeOnModel(oStateModel, "/analyticsReturnRestoreEdit", null);
        }
        ModelStateRuntime.writeOnModel(oStateModel, "/analyticsNavReturn", null);
        if (!sTargetHash) {
            navigateToSearch(oController);
            return;
        }
        if (oHashChanger && typeof oHashChanger[HASH_CHANGER.REPLACE_HASH] === TYPEOF.FUNCTION) {
            oHashChanger[HASH_CHANGER.REPLACE_HASH](sTargetHash);
        }
    }

    function navigateToSearch(oController) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var oHashChanger = HashChanger && HashChanger.getInstance ? HashChanger.getInstance() : null;
        if (oRouter && typeof oRouter[METHODS.NAV_TO] === TYPEOF.FUNCTION) {
            oRouter[METHODS.NAV_TO](NavigationContracts.ROUTES.SEARCH, {}, false);
            return;
        }
        if (oHashChanger && typeof oHashChanger[HASH_CHANGER.REPLACE_HASH] === TYPEOF.FUNCTION) {
            oHashChanger[HASH_CHANGER.REPLACE_HASH]("");
            return;
        }
    }

    function navigateToDetail(oController, sRootId, sLayout) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();
        var sResolvedLayout = LayoutStateRuntime.normalizeLayout(sLayout);

        if (!oRouter || typeof oRouter[METHODS.NAV_TO] !== TYPEOF.FUNCTION || !sId) {
            return;
        }
        if (sResolvedLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
            oRouter[METHODS.NAV_TO](NavigationContracts.ROUTES.DETAIL, { id: sId, layout: NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN }, false);
            return;
        }
        oRouter[METHODS.NAV_TO](NavigationContracts.ROUTES.DETAIL, { id: sId }, false);
    }

    function buildDetailHash(oController, sRootId) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();

        if (!oRouter || typeof oRouter[METHODS.GET_URL] !== TYPEOF.FUNCTION || !sId) {
            return "";
        }
        return String(oRouter[METHODS.GET_URL](NavigationContracts.ROUTES.DETAIL, { id: sId }) || "");
    }

    return {
        buildCurrentIntent: buildCurrentIntent,
        buildDetailHash: buildDetailHash,
        navigateBackFromAnalytics: navigateBackFromAnalytics,
        navigateToAnalytics: navigateToAnalytics,
        navigateToDetail: navigateToDetail,
        navigateToSearch: navigateToSearch,
        setAnalyticsReturnIntent: setAnalyticsReturnIntent
    };
});
