sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/CurrentUserProfile",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/PermissionPresentation",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (ControllerTextRuntime, ActionContract, LayoutStateRuntime, ControllerModelRuntime, RootIdRuntime, CurrentUserProfile, PermissionPresentation, CreateSentinel, ModelStateRuntime) {
    "use strict";

    var getText = ControllerTextRuntime.getText;
    var PERMISSION_TEXT_KEY_MAP = {
        "01": "shellPermissionCreate",
        "02": "shellPermissionChange",
        "03": "shellPermissionDisplay",
        "06": "shellPermissionDelete"
    };

    function normalizePermissionRule(oPermission) {
        return CurrentUserProfile.normalizePermissionRule(oPermission);
    }

    function buildPermissionScopeLabel(oController, oPermission) {
        var sScopeKind = String(oPermission && oPermission.scopeKind || "all").trim().toLowerCase() || "all";
        var sScopeValue = String(oPermission && oPermission.scopeValue || "ALL").trim() || "ALL";
        return sScopeKind === "bukrs" && sScopeValue.toUpperCase() !== "ALL"
            ? getText(oController, "shellPermissionScopeBukrs", [sScopeValue], "BUKRS " + sScopeValue)
            : getText(oController, "shellPermissionScopeAll", null, "ALL");
    }

    function buildPermissionSheets(oController, aPermissionRules) {
        return PermissionPresentation.buildPermissionSheets(aPermissionRules, {
            codeOrder: ["01", "02", "03", "06"],
            scopeLabel: function (oRule) {
                return buildPermissionScopeLabel(oController, oRule);
            },
            codeLabel: function (oRule) {
                return getText(oController, PERMISSION_TEXT_KEY_MAP[oRule.code] || "shellPermissionUnknown", [oRule.code], oRule.code);
            }
        });
    }

    function buildUserSummaryText(oController, aPermissionSheets, sBackendSummary) {
        return PermissionPresentation.buildSummaryText(
            sBackendSummary,
            aPermissionSheets,
            getText(oController, "shellUserPermissionsEmpty", null, "No permissions assigned")
        );
    }

    function buildHeaderUserLabel(sFullName, aPermissionSheets) {
        return PermissionPresentation.buildHeaderLabel(sFullName, aPermissionSheets, " - ");
    }

    return {
        _ensureAppViewDefaults: function () {
            var mPatch = {};
            if (!ControllerModelRuntime.appView(this)) {
                return;
            }
            mPatch["/compactDensity"] = !!ModelStateRuntime.read(this, "appView", "/compactDensity", false);
            mPatch["/animationEnabled"] = ModelStateRuntime.read(this, "appView", "/animationEnabled", true) !== false;
            mPatch["/backgroundInteractive"] = ModelStateRuntime.read(this, "appView", "/backgroundInteractive", false) === true;
            mPatch["/invertedBlockScheme"] = !!ModelStateRuntime.read(this, "appView", "/invertedBlockScheme", false);
            mPatch["/isPhoneViewport"] = !!ModelStateRuntime.read(this, "appView", "/isPhoneViewport", false);
            mPatch["/isTabletViewport"] = !!ModelStateRuntime.read(this, "appView", "/isTabletViewport", false);
            if (!ModelStateRuntime.read(this, "appView", "/shell", null)) {
                mPatch["/shell"] = {
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
            ModelStateRuntime.setMany(this, "appView", mPatch);
        },

        _syncShellState: function () {
            var oState = ControllerModelRuntime.state(this);
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
            var bRuntimeManagedUser;
            var bSearchWorkspace;
            var bEditWorkspace;

            if (!ControllerModelRuntime.appView(this) || !oState) {
                return;
            }

            this._ensureAppViewDefaults();
            sSelectedId = RootIdRuntime.resolveFromStateModel(oState);
            sCurrentRouteName = String(ModelStateRuntime.read(this, "state", "/currentRouteName", "search") || "search").trim() || "search";
            sMode = LayoutStateRuntime.readMode(oState, "READ");
            oCurrentUser = ModelStateRuntime.read(this, "state", "/currentUser", {}) || {};
            sFullName = String(oCurrentUser.fullName || getText(this, "shellUserMissing", null, "Session profile unavailable"));
            aPermissions = Array.isArray(oCurrentUser.permissions) ? oCurrentUser.permissions.slice() : [];
            aPermissionRules = Array.isArray(oCurrentUser.permissionRules) ? oCurrentUser.permissionRules.slice() : [];
            aPermissionSheets = buildPermissionSheets(this, aPermissionRules);
            sUserSummaryText = buildUserSummaryText(this, aPermissionSheets, oCurrentUser.summaryText);
            bShowHints = !!ModelStateRuntime.read(this, "layout", "/personalization/showHints", false);
            sFrontendSource = String(
                ModelStateRuntime.read(this, "state", "/frontendConfigSource", "")
                || ModelStateRuntime.read(this, "state", "/backendMode", "")
                || "gateway_runtime"
            );
            bRuntimeManagedUser = true;
            bSearchWorkspace = !sSelectedId;
            bEditWorkspace = !bSearchWorkspace && sMode === "EDIT";

            mShellPatch["/shell/productName"] = sCurrentRouteName === "analytics"
                ? getText(this, "shellProductNameAnalytics", null, "Аналитика чек-листов производственного контроля")
                : getText(this, "shellProductNameSearch", null, "Чек-листы производственного контроля");
            mShellPatch["/shell/routeLabel"] = "";
            mShellPatch["/shell/contextSubtitle"] = sCurrentRouteName === "analytics"
                ? getText(this, "shellContextAnalytics", null, "Gateway-backed workflow dashboard with operational totals and breakdowns.")
                : (sCurrentRouteName === "accessDenied"
                    ? getText(this, "shellContextAccessDenied", [sSelectedId], "Checklist " + sSelectedId + " is restricted for the current session.")
                    : (!sSelectedId ? getText(this, "shellContextSearch", null, "Discover, filter, and open checklist flows.")
                        : (CreateSentinel.isCreateId(sSelectedId) ? getText(this, "shellContextDraft", null, "Draft checklist workspace")
                            : getText(this, "shellContextDetail", [sSelectedId], "Checklist " + sSelectedId))));
            mShellPatch["/shell/userLabel"] = buildHeaderUserLabel(sFullName, aPermissionSheets);
            mShellPatch["/shell/userMeta"] = sFullName;
            mShellPatch["/shell/userLoginLabel"] = "";
            mShellPatch["/shell/userEnvironmentLabel"] = sFrontendSource;
            mShellPatch["/shell/userPermissions"] = aPermissionSheets;
            mShellPatch["/shell/userSummaryText"] = sUserSummaryText;
            mShellPatch["/shell/userSessionLabel"] = bRuntimeManagedUser
                ? getText(this, "shellUserSessionManaged", null, "Live backend profile")
                : getText(this, "shellUserSessionTest", null, "Test session identity");
            mShellPatch["/shell/userSessionState"] = aPermissionRules.length || aPermissions.length ? "Success" : "Warning";
            mShellPatch["/shell/userActionVisible"] = false;
            mShellPatch["/shell/userActionText"] = "";
            mShellPatch["/shell/userActionIcon"] = "sap-icon://employee";
            mShellPatch["/shell/userActionType"] = "Transparent";
            mShellPatch["/shell/userActionKind"] = "";
            mShellPatch["/shell/userActionPassiveText"] = bEditWorkspace ? getText(this, "shellLockLocked", null, "Lock owned by you") : "";
            mShellPatch["/shell/userActionHint"] = getText(this, "shellUserHintRuntime", null, "The backend resolves the current SAP user profile for this session.");
            mShellPatch["/shell/userTooltip"] = sUserSummaryText || getText(this, "shellUserTooltipStandalone", null, "Open user session controls");
            mShellPatch["/shell/userIcon"] = "sap-icon://employee";
            mShellPatch["/shell/showHints"] = bShowHints;
            ModelStateRuntime.setMany(this, "appView", mShellPatch);
            if (typeof this._markStartupReady === "function") {
                this._markStartupReady();
            }
        }
    };
});
