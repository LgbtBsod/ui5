sap.ui.define([
    "sap_ui5/controller/support/AppShellTextSupport",
    "sap_ui5/controller/support/AppShellPermissionPresentation",
    "sap_ui5/service/framework/ActionContract",
    "sap_ui5/util/CreateSentinel",
    "sap_ui5/controller/support/ControllerModelWriteSupport"
], function (AppShellTextSupport, AppShellPermissionPresentation, ActionContract, CreateSentinel, ControllerModelWriteSupport) {
    "use strict";
    var getText = AppShellTextSupport.getText;
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

    function setAppViewPatch(oController, mPatch) {
        return ControllerModelWriteSupport.setMany(oController, "appView", mPatch);
    }

    function normalizeLayout(vLayout) {
        var sLayout = String(vLayout || "").trim();
        if (sLayout === "MidColumnFullScreen") {
            return "MidColumnFullScreen";
        }
        if (sLayout === "TwoColumnsMidExpanded" || sLayout === "TwoColumnsBeginExpanded") {
            return "TwoColumnsMidExpanded";
        }
        return "OneColumn";
    }

    function toLayoutKind(vLayout) {
        var sLayout = normalizeLayout(vLayout);
        if (sLayout === "MidColumnFullScreen") {
            return "detailOnly";
        }
        if (sLayout === "OneColumn") {
            return "single";
        }
        return "split";
    }

    return {
        _ensureAppViewDefaults: function () {
            var mPatch = {};
            if (!this._getAppViewModel()) {
                return;
            }
            mPatch["/compactDensity"] = !!ControllerModelWriteSupport.get(this, "appView", "/compactDensity");
            mPatch["/animationEnabled"] = ControllerModelWriteSupport.get(this, "appView", "/animationEnabled") !== false;
            mPatch["/backgroundInteractive"] = ControllerModelWriteSupport.get(this, "appView", "/backgroundInteractive") !== false;
            mPatch["/isPhoneViewport"] = !!ControllerModelWriteSupport.get(this, "appView", "/isPhoneViewport");
            mPatch["/isTabletViewport"] = !!ControllerModelWriteSupport.get(this, "appView", "/isTabletViewport");
            if (!ControllerModelWriteSupport.get(this, "appView", "/shell")) {
                mPatch["/shell"] = {
                    routeLabel: "", layoutLabel: "", layoutState: "Information", modeLabel: "", modeState: "Information",
                    contextSubtitle: "", userLabel: "", userMeta: "", userSessionLabel: "", userSessionState: "Information",
                    userActionVisible: false, userActionText: "", userActionIcon: "sap-icon://employee-lookup",
                    userActionType: "Transparent", userActionKind: "", userActionHint: "", userActionPassiveText: "",
                    userTooltip: "", userIcon: "sap-icon://employee", userSummaryText: "", userPermissions: [],
                    userLoginLabel: "", userEnvironmentLabel: "", userRefreshBusy: false, notifications: []
                };
            }
            setAppViewPatch(this, mPatch);
        },

        _syncShellState: function () {
            var oState = this._getStateModel();
            var oLayout = this._getLayoutModel();
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
            var bShowHints;
            var sFrontendSource;
            var bRuntimeManagedUser;
            var bSearchWorkspace;
            var bEditWorkspace;
            if (!this._getAppViewModel() || !oState) {
                return;
            }
            this._ensureAppViewDefaults();
            sSelectedId = String(oState.getProperty("/selectedId") || oState.getProperty("/activeObjectId") || "").trim();
            sCurrentRouteName = String(oState.getProperty("/currentRouteName") || "search").trim() || "search";
            sLayoutKind = toLayoutKind(oState.getProperty("/layout"));
            sMode = String(oState.getProperty("/mode") || "READ").toUpperCase();
            sLockState = String(oState.getProperty("/lockOperationState") || "IDLE").toUpperCase();
            sAutosaveState = String(oState.getProperty("/autosaveState") || "IDLE").toUpperCase();
            sUser = String(oState.getProperty("/testUser") || "").trim();
            oCurrentUser = oState.getProperty("/currentUser") || {};
            sFullName = String(oCurrentUser.fullName || sUser || getText(this, "shellUserMissing", null, "User login required"));
            aPermissions = Array.isArray(oCurrentUser.permissions) ? oCurrentUser.permissions.slice() : [];
            aPermissionRules = Array.isArray(oCurrentUser.permissionRules) ? oCurrentUser.permissionRules.slice() : [];
            bShowHints = !!(oLayout && oLayout.getProperty && oLayout.getProperty("/personalization/showHints"));
            sFrontendSource = String(oState.getProperty("/frontendConfigSource") || oState.getProperty("/backendMode") || "gateway_runtime");
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
            mShellPatch["/shell/userLabel"] = sFullName;
            mShellPatch["/shell/userMeta"] = sUser || getText(this, "shellUserLoginMissing", null, "Login is not set");
            mShellPatch["/shell/userLoginLabel"] = sUser || getText(this, "shellUserLoginMissing", null, "Login is not set");
            mShellPatch["/shell/userEnvironmentLabel"] = sFrontendSource;
            mShellPatch["/shell/userPermissions"] = aPermissionRules.map(AppShellPermissionPresentation.mapPermissionPresentation.bind(null, this));
            mShellPatch["/shell/userSummaryText"] = String(oCurrentUser.summaryText || "").trim()
                || (mShellPatch["/shell/userPermissions"].length
                    ? getText(this, "shellUserPermissionSummary", [mShellPatch["/shell/userPermissions"].map(function (oRule) {
                        return oRule.title;
                    }).join(" / ")], "You have permissions for " + mShellPatch["/shell/userPermissions"].map(function (oRule) {
                        return oRule.title;
                    }).join(" / ") + ".")
                    : getText(this, "shellUserPermissionsEmpty", null, "No permissions assigned"));
            mShellPatch["/shell/userSessionLabel"] = bRuntimeManagedUser ? getText(this, "shellUserSessionManaged", null, "Live backend profile") : getText(this, "shellUserSessionTest", null, "Test session identity");
            mShellPatch["/shell/userSessionState"] = aPermissionRules.length || aPermissions.length ? "Success" : "Warning";
            mShellPatch["/shell/userActionVisible"] = false;
            mShellPatch["/shell/userActionText"] = "";
            mShellPatch["/shell/userActionIcon"] = "sap-icon://employee";
            mShellPatch["/shell/userActionType"] = "Transparent";
            mShellPatch["/shell/userActionKind"] = "";
            mShellPatch["/shell/userActionPassiveText"] = bEditWorkspace ? getText(this, "shellLockLocked", null, "Lock owned by you") : "";
            mShellPatch["/shell/userActionHint"] = getText(this, "shellUserHintRuntime", null, "The backend resolves the current SAP user profile for this session.");
            mShellPatch["/shell/userTooltip"] = getText(this, "shellUserTooltipStandalone", null, "Open user session controls");
            mShellPatch["/shell/userIcon"] = "sap-icon://employee";
            mShellPatch["/shell/showHints"] = bShowHints;
            mShellPatch["/shell/notifications"] = this._buildShellNotifications(oState, sMode, sLockState, sAutosaveState);
            setAppViewPatch(this, mShellPatch);
        },
        _buildShellNotifications: function (oState, sMode, sLockState, sAutosaveState) {
            var bOnline = oState.getProperty("/networkOnline") !== false;
            var bGrace = !!oState.getProperty("/networkGraceMode");
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
