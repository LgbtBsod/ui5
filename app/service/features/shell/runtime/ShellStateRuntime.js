sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/PermissionPresentation",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (LayoutStateRuntime, ControllerModelRuntime, RootIdRuntime, PermissionPresentation, CreateSentinel, ModelStateRuntime, NavigationContracts, WorkflowContracts, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var STATE_MODEL = MODELS.STATE;
    var LAYOUT_MODEL = MODELS.LAYOUT;
    var APP_VIEW_MODEL = MODELS.APP_VIEW;
    var PERMISSION_TEXT_KEY_MAP = {
        "01": "shellPermissionCreate",
        "02": "shellPermissionChange",
        "03": "shellPermissionDisplay",
        "06": "shellPermissionDelete"
    };

    function resolveText(mHooks, oController, sKey, aArgs, sFallback) {
        if (mHooks && typeof mHooks.getText === "function") {
            return mHooks.getText(oController, sKey, aArgs, sFallback);
        }
        return sFallback;
    }

    function buildPermissionScopeLabel(oController, oPermission) {
        var sScopeKind = String(oPermission && oPermission.scopeKind || "all").trim().toLowerCase() || "all";
        var sScopeValue = String(oPermission && oPermission.scopeValue || "ALL").trim() || "ALL";
        return sScopeKind === "bukrs" && sScopeValue.toUpperCase() !== "ALL"
            ? resolveText(null, oController, "shellPermissionScopeBukrs", [sScopeValue], "BUKRS " + sScopeValue)
            : resolveText(null, oController, "shellPermissionScopeAll", null, "ALL");
    }

    function buildPermissionSheets(oController, aPermissionRules, mHooks) {
        return PermissionPresentation.buildPermissionSheets(aPermissionRules, {
            codeOrder: ["01", "02", "03", "06"],
            scopeLabel: function (oRule) {
                var sScopeKind = String(oRule && oRule.scopeKind || "all").trim().toLowerCase() || "all";
                var sScopeValue = String(oRule && oRule.scopeValue || "ALL").trim() || "ALL";
                return sScopeKind === "bukrs" && sScopeValue.toUpperCase() !== "ALL"
                    ? resolveText(mHooks, oController, "shellPermissionScopeBukrs", [sScopeValue], "BUKRS " + sScopeValue)
                    : resolveText(mHooks, oController, "shellPermissionScopeAll", null, "ALL");
            },
            codeLabel: function (oRule) {
                return resolveText(mHooks, oController, PERMISSION_TEXT_KEY_MAP[oRule.code] || "shellPermissionUnknown", [oRule.code], oRule.code);
            }
        });
    }

    function buildUserSummaryText(oController, aPermissionSheets, sBackendSummary, mHooks) {
        return PermissionPresentation.buildSummaryText(
            sBackendSummary,
            aPermissionSheets,
            resolveText(mHooks, oController, "shellUserPermissionsEmpty", null, "No permissions assigned")
        );
    }

    function buildHeaderUserLabel(sFullName, aPermissionSheets) {
        return PermissionPresentation.buildHeaderLabel(sFullName, aPermissionSheets, " - ");
    }

    function hasResolvedCurrentUser(oCurrentUser) {
        return !!String((oCurrentUser && oCurrentUser.fullName) || "").trim();
    }

    function ensureAppViewDefaults(oController) {
        var mPatch = {};

        if (!ControllerModelRuntime.appView(oController)) {
            return;
        }
        mPatch[MODEL_PATHS.APP_VIEW_COMPACT_DENSITY] = !!ModelStateRuntime.read(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_COMPACT_DENSITY, false);
        mPatch[MODEL_PATHS.APP_VIEW_ANIMATION_ENABLED] = ModelStateRuntime.read(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_ANIMATION_ENABLED, true) !== false;
        mPatch[MODEL_PATHS.APP_VIEW_INVERTED_BLOCK_SCHEME] = !!ModelStateRuntime.read(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_INVERTED_BLOCK_SCHEME, false);
        mPatch[MODEL_PATHS.APP_VIEW_IS_PHONE_VIEWPORT] = !!ModelStateRuntime.read(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_IS_PHONE_VIEWPORT, false);
        mPatch[MODEL_PATHS.APP_VIEW_IS_TABLET_VIEWPORT] = !!ModelStateRuntime.read(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_IS_TABLET_VIEWPORT, false);
        if (!ModelStateRuntime.read(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_SHELL, null)) {
                mPatch[MODEL_PATHS.APP_VIEW_SHELL] = {
                eyebrow: "",
                productName: "",
                routeLabel: "",
                contextSubtitle: "",
                userLabel: "",
                userMeta: "",
                userSessionLabel: "",
                userSessionState: "Information",
                userActionVisible: false,
                userActionText: "",
                userActionIcon: "sap-icon://employee-lookup",
                userActionType: "Transparent",
                userActionKind: "",
                userActionHint: "",
                userActionPassiveText: "",
                userTooltip: "",
                userIcon: "sap-icon://employee",
                userSummaryText: "",
                userPermissions: [],
                userLoginLabel: "",
                userEnvironmentLabel: "",
                userRefreshBusy: false
            };
        }
        ModelStateRuntime.setMany(oController, APP_VIEW_MODEL, mPatch);
    }

    function syncShellState(oController, mHooks) {
        var oState = ControllerModelRuntime.state(oController);
        var mShellPatch = {};
        var sSelectedId;
        var sMode;
        var sCurrentRouteName;
        var oCurrentUser;
        var sFullName;
        var aPermissions;
        var aPermissionRules;
        var aPermissionSheets;
        var sUserSummaryText;
        var bShowHints;
        var sFrontendSource;
        var bSearchWorkspace;
        var bEditWorkspace;

        if (!ControllerModelRuntime.appView(oController) || !oState) {
            return;
        }

        ensureAppViewDefaults(oController);
        sSelectedId = RootIdRuntime.resolveFromStateModel(oState);
        sCurrentRouteName = String(ModelStateRuntime.read(oController, STATE_MODEL, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        sMode = LayoutStateRuntime.readMode(oState, WorkflowContracts.EDIT_MODES.READ);
        oCurrentUser = ModelStateRuntime.read(oController, STATE_MODEL, "/currentUser", {}) || {};
        sFullName = hasResolvedCurrentUser(oCurrentUser)
            ? String(oCurrentUser.fullName || "")
            : resolveText(mHooks, oController, "shellUserLoading", null, "Loading session profile...");
        aPermissions = Array.isArray(oCurrentUser.permissions) ? oCurrentUser.permissions.slice() : [];
        aPermissionRules = Array.isArray(oCurrentUser.permissionRules) ? oCurrentUser.permissionRules.slice() : [];
        aPermissionSheets = buildPermissionSheets(oController, aPermissionRules, mHooks);
        sUserSummaryText = buildUserSummaryText(oController, aPermissionSheets, oCurrentUser.summaryText, mHooks);
        bShowHints = !!ModelStateRuntime.read(oController, LAYOUT_MODEL, "/personalization/showHints", false);
        sFrontendSource = String(
            ModelStateRuntime.read(oController, STATE_MODEL, "/frontendConfigSource", "")
            || ModelStateRuntime.read(oController, STATE_MODEL, "/backendMode", "")
            || "gateway_runtime"
        );
        bSearchWorkspace = !sSelectedId;
        bEditWorkspace = !bSearchWorkspace && sMode === WorkflowContracts.EDIT_MODES.EDIT;

        mShellPatch["/shell/eyebrow"] = bSearchWorkspace
            ? resolveText(mHooks, oController, "appTitle", null, "Production Control Checklists")
            : resolveText(mHooks, oController, "detailWorkspaceSectionTitle", null, "Checklist Workspace");
        mShellPatch["/shell/productName"] = sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS
            ? resolveText(mHooks, oController, "shellProductNameAnalytics", null, "Analytics")
            : resolveText(mHooks, oController, "shellProductNameSearch", null, "Production control checklists");
        mShellPatch["/shell/routeLabel"] = sCurrentRouteName;
        mShellPatch["/shell/contextSubtitle"] = sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS
            ? resolveText(mHooks, oController, "shellContextAnalytics", null, "Gateway-backed workflow dashboard with operational totals and breakdowns.")
            : (!sSelectedId ? resolveText(mHooks, oController, "shellContextSearch", null, "Discover, filter, and open checklist flows.")
                : (CreateSentinel.isCreateId(sSelectedId) ? resolveText(mHooks, oController, "shellContextDraft", null, "Draft checklist workspace")
                    : resolveText(mHooks, oController, "shellContextDetail", [sSelectedId], "Checklist " + sSelectedId)));
        mShellPatch["/shell/userLabel"] = buildHeaderUserLabel(sFullName, aPermissionSheets);
        mShellPatch["/shell/userMeta"] = sFullName;
        mShellPatch["/shell/userLoginLabel"] = "";
        mShellPatch["/shell/userEnvironmentLabel"] = sFrontendSource;
        mShellPatch["/shell/userPermissions"] = aPermissionSheets;
        mShellPatch["/shell/userSummaryText"] = sUserSummaryText;
        mShellPatch["/shell/userSessionLabel"] = resolveText(mHooks, oController, "shellUserSessionManaged", null, "Live backend profile");
        mShellPatch["/shell/userSessionState"] = hasResolvedCurrentUser(oCurrentUser)
            ? (aPermissionRules.length || aPermissions.length ? "Success" : "Warning")
            : "Information";
        mShellPatch["/shell/userActionVisible"] = false;
        mShellPatch["/shell/userActionText"] = "";
        mShellPatch["/shell/userActionIcon"] = "sap-icon://employee";
        mShellPatch["/shell/userActionType"] = "Transparent";
        mShellPatch["/shell/userActionKind"] = "";
        mShellPatch["/shell/userActionPassiveText"] = bEditWorkspace ? resolveText(mHooks, oController, "shellLockLocked", null, "Lock owned by you") : "";
        mShellPatch["/shell/userActionHint"] = resolveText(mHooks, oController, "shellUserHintRuntime", null, "The backend resolves the current SAP user profile for this session.");
        mShellPatch["/shell/userTooltip"] = sUserSummaryText || resolveText(mHooks, oController, "shellUserTooltipStandalone", null, "Open user session controls");
        mShellPatch["/shell/userIcon"] = "sap-icon://employee";
        mShellPatch["/shell/showHints"] = bShowHints;
        ModelStateRuntime.setMany(oController, APP_VIEW_MODEL, mShellPatch);
        if (typeof oController._markStartupReady === "function") {
            oController._markStartupReady();
        }
    }

    return {
        ensureAppViewDefaults: ensureAppViewDefaults,
        syncShellState: syncShellState
    };
});
