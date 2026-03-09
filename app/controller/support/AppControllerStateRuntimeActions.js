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
        "01": "shellPermissionView",
        "02": "shellPermissionEditCreate",
        "03": "shellPermissionDelete"
    };
    var SHELL_MODE_STATE_MAP = {
        EDIT_LOCKED: "Success",
        PENDING: "Warning",
        ERROR: "Error",
        CREATE: "Information",
        DEFAULT: "Information"
    };
    var SHELL_LAYOUT_STATE_MAP = {
        single: "Information",
        split: "Success",
        detailOnly: "Warning",
        default: "Information"
    };
    var AUTOSAVE_STATE_MAP = {
        ERROR: "Error",
        SAVING: "Warning",
        SAVED: "Success",
        IDLE: "Information"
    };

    function resolveAutosaveNotificationState(sMode, sAutosaveState) {
        if (sMode !== "EDIT") {
            return "Information";
        }
        return AUTOSAVE_STATE_MAP[sAutosaveState] || AUTOSAVE_STATE_MAP.IDLE;
    }

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

    function mapPermissionPresentation(oController, oPermission) {
        var oRule = normalizePermissionRule(oPermission);
        var sTitle = getText(oController, PERMISSION_TEXT_KEY_MAP[oRule.code] || "shellPermissionUnknown", [oRule.code], oRule.code);
        var sScopeLabel = buildPermissionScopeLabel(oController, oRule);
        return {
            code: oRule.code,
            title: sScopeLabel,
            description: (oRule.code ? [oRule.code, sTitle].join(" ") : sTitle),
            scope: sScopeLabel,
            permissionTitle: sTitle
        };
    }

    function buildPermissionSheets(oController, aPermissionRules) {
        return PermissionPresentation.buildPermissionSheets(aPermissionRules, {
            codeOrder: ["01", "02", "03"],
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
            mPatch["/backgroundInteractive"] = ModelStateRuntime.read(this, "appView", "/backgroundInteractive", true) !== false;
            mPatch["/invertedBlockScheme"] = !!ModelStateRuntime.read(this, "appView", "/invertedBlockScheme", false);
            mPatch["/isPhoneViewport"] = !!ModelStateRuntime.read(this, "appView", "/isPhoneViewport", false);
            mPatch["/isTabletViewport"] = !!ModelStateRuntime.read(this, "appView", "/isTabletViewport", false);
            if (!ModelStateRuntime.read(this, "appView", "/shell", null)) {
                mPatch["/shell"] = {
                    routeLabel: "", layoutLabel: "", layoutState: "Information", modeLabel: "", modeState: "Information",
                    contextSubtitle: "", userLabel: "", userMeta: "", userSessionLabel: "", userSessionState: "Information",
                    userActionVisible: false, userActionText: "", userActionIcon: "sap-icon://employee-lookup",
                    userActionType: "Transparent", userActionKind: "", userActionHint: "", userActionPassiveText: "",
                    userTooltip: "", userIcon: "sap-icon://employee", userSummaryText: "", userPermissions: [],
                    userLoginLabel: "", userEnvironmentLabel: "", userRefreshBusy: false, notifications: []
                };
            }
            ModelStateRuntime.setMany(this, "appView", mPatch);
        },

        _syncShellState: function () {
            var oState = ControllerModelRuntime.state(this);
            var oLayout = ControllerModelRuntime.layout(this);
            var mShellPatch = {};
            var sSelectedId;
            var sLayoutKind;
            var sMode;
            var sLockState;
            var sAutosaveState;
            var sUser;
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
            sLayoutKind = LayoutStateRuntime.toLayoutKind(ModelStateRuntime.read(this, "state", "/layout", "OneColumn"));
            sMode = LayoutStateRuntime.readMode(oState, "READ");
            sLockState = LayoutStateRuntime.readLockState(oState, "IDLE");
            sAutosaveState = LayoutStateRuntime.readAutosaveState(oState, "IDLE");
            oCurrentUser = ModelStateRuntime.read(this, "state", "/currentUser", {}) || {};
            sUser = String(oCurrentUser.uname || "").trim();
            sFullName = String(oCurrentUser.fullName || sUser || getText(this, "shellUserMissing", null, "Session profile unavailable"));
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
            mShellPatch["/shell/routeLabel"] = sCurrentRouteName === "analytics"
                ? getText(this, "shellRouteAnalytics", null, "Analytics dashboard")
                : (sCurrentRouteName === "accessDenied"
                    ? getText(this, "shellRouteAccessDenied", null, "Restricted checklist")
                    : (sSelectedId ? getText(this, "shellRouteDetail", null, "Checklist workspace") : getText(this, "shellRouteSearch", null, "Search workspace")));
            mShellPatch["/shell/layoutLabel"] = ({
                single: getText(this, "shellLayoutSingle", null, "Search only"),
                split: getText(this, "shellLayoutSplit", null, "Split workspace"),
                detailOnly: getText(this, "shellLayoutDetailOnly", null, "Detail focus")
            })[sLayoutKind] || getText(this, "shellLayoutSingle", null, "Search only");
            mShellPatch["/shell/layoutState"] = this._shellLayoutState(sLayoutKind);
            mShellPatch["/shell/modeLabel"] = ({
                READ: getText(this, "shellModeRead", null, "Read mode"),
                EDIT: getText(this, "shellModeEdit", null, "Edit mode locked"),
                CREATE: getText(this, "shellModeCreate", null, "Draft mode")
            })[sMode] || getText(this, "shellModeRead", null, "Read mode");
            mShellPatch["/shell/modeState"] = this._shellModeState(sMode, sLockState);
            mShellPatch["/shell/contextSubtitle"] = sCurrentRouteName === "analytics"
                ? getText(this, "shellContextAnalytics", null, "Gateway-backed workflow dashboard with operational totals and breakdowns.")
                : (sCurrentRouteName === "accessDenied"
                    ? getText(this, "shellContextAccessDenied", [sSelectedId], "Checklist " + sSelectedId + " is restricted for the current session.")
                    : (!sSelectedId ? getText(this, "shellContextSearch", null, "Discover, filter, and open checklist flows.")
                        : (CreateSentinel.isCreateId(sSelectedId) ? getText(this, "shellContextDraft", null, "Draft checklist workspace")
                            : getText(this, "shellContextDetail", [sSelectedId], "Checklist " + sSelectedId))));
            mShellPatch["/shell/userLabel"] = buildHeaderUserLabel(sFullName, aPermissionSheets);
            mShellPatch["/shell/userMeta"] = sUser || getText(this, "shellUserLoginMissing", null, "Login is not set");
            mShellPatch["/shell/userLoginLabel"] = sUser || getText(this, "shellUserLoginMissing", null, "Login is not set");
            mShellPatch["/shell/userEnvironmentLabel"] = sFrontendSource;
            mShellPatch["/shell/userPermissions"] = aPermissionSheets;
            mShellPatch["/shell/userSummaryText"] = sUserSummaryText;
            mShellPatch["/shell/userSessionLabel"] = bRuntimeManagedUser ? getText(this, "shellUserSessionManaged", null, "Live backend profile") : getText(this, "shellUserSessionTest", null, "Test session identity");
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
            mShellPatch["/shell/notifications"] = this._buildShellNotifications(oState, sMode, sLockState, sAutosaveState);
            ModelStateRuntime.setMany(this, "appView", mShellPatch);
        },
        _buildShellNotifications: function (oState, sMode, sLockState, sAutosaveState) {
            var bOnline = ModelStateRuntime.read(this, "state", "/networkOnline", true) !== false;
            var bGrace = !!ModelStateRuntime.read(this, "state", "/networkGraceMode", false);
            var sConnectionText = bOnline ? (bGrace ? getText(this, "shellConnectionGrace", null, "Connection unstable") : getText(this, "shellConnectionOnline", null, "Online")) : getText(this, "shellConnectionOffline", null, "Offline");
            var sLockText = sMode === "EDIT" && sLockState === "LOCKED" ? getText(this, "shellLockLocked", null, "Lock owned by you")
                : (sLockState === "PENDING" ? getText(this, "shellLockPending", null, "Lock request in progress")
                    : (sLockState === "ERROR" ? getText(this, "shellLockConflict", null, "Locked elsewhere") : getText(this, "shellLockRead", null, "Read only")));
            var sAutosaveText = sMode !== "EDIT" ? getText(this, "shellAutosaveDisabled", null, "Disabled in read mode")
                : (sAutosaveState === "SAVING" ? getText(this, "shellAutosaveSaving", null, "Saving now")
                    : (sAutosaveState === "SAVED" ? getText(this, "shellAutosaveSaved", null, "Recently synchronized")
                        : getText(this, "shellAutosaveIdle", null, "Waiting for changes")));
            return [
                { icon: bOnline ? "sap-icon://accept" : "sap-icon://warning2", title: getText(this, "shellNotificationConnectivity", null, "Connectivity"), text: bOnline ? getText(this, "shellNotifConnectionHealthy", null, "Backend and UI channel available.") : getText(this, "shellNotifConnectionLost", null, "The workspace is offline and edit flows may be degraded."), status: sConnectionText, state: bOnline ? (bGrace ? "Warning" : "Success") : "Error" },
                { icon: "sap-icon://edit", title: getText(this, "shellNotificationLock", null, "Edit lock"), text: sMode === "EDIT" ? getText(this, "shellNotifLockEdit", null, "Detail workflow is operating in locked edit mode.") : getText(this, "shellNotifLockRead", null, "Detail workflow is currently read only."), status: sLockText, state: this._shellModeState(sMode, sLockState) },
                { icon: "sap-icon://synchronize", title: getText(this, "shellNotificationAutosave", null, "Autosave"), text: sMode === "EDIT" ? getText(this, "shellNotifAutosaveEdit", null, "Autosave watches draft changes while the lock is active.") : getText(this, "shellNotifAutosaveRead", null, "Autosave is intentionally inactive outside edit mode."), status: sAutosaveText, state: resolveAutosaveNotificationState(sMode, sAutosaveState) }
            ];
        },

        _shellModeState: function (sMode, sLockState) {
            if (sMode === "EDIT" && sLockState === "LOCKED") return SHELL_MODE_STATE_MAP.EDIT_LOCKED;
            if (sLockState === "PENDING") return SHELL_MODE_STATE_MAP.PENDING;
            if (sLockState === "ERROR") return SHELL_MODE_STATE_MAP.ERROR;
            if (sMode === "CREATE") return SHELL_MODE_STATE_MAP.CREATE;
            return SHELL_MODE_STATE_MAP.DEFAULT;
        },

        _shellLayoutState: function (sLayoutKind) {
            return SHELL_LAYOUT_STATE_MAP[sLayoutKind] || SHELL_LAYOUT_STATE_MAP.default;
        }
    };
});
