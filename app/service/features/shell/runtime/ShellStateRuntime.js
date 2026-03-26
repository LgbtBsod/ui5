sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/PermissionPresentation",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (LayoutStateRuntime, ControllerModelRuntime, RootIdRuntime, PermissionPresentation, CreateSentinel, ModelStateRuntime, NavigationContracts, WorkflowContracts, ModelContracts, ModelPathContracts, MessageKeyConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var STATE_MODEL = MODELS.STATE;
    var SHELL_MODEL = MODELS.SHELL;
    var PERMISSION_TEXT_KEY_MAP = {
        "01": MessageKeyConstants.SHELL.PERMISSION_CREATE,
        "02": MessageKeyConstants.SHELL.PERMISSION_CHANGE,
        "03": MessageKeyConstants.SHELL.PERMISSION_DISPLAY,
        "06": MessageKeyConstants.SHELL.PERMISSION_DELETE
    };

    function resolveText(mHooks, oController, sKey, aArgs) {
        var oBundle;
        if (mHooks && typeof mHooks.getText === "function") {
            return mHooks.getText(oController, sKey, aArgs, "");
        }
        oBundle = oController && oController.getResourceBundle && oController.getResourceBundle();
        if (!sKey || !oBundle || !oBundle.getText) {
            return "";
        }
        return String(oBundle.getText(sKey, Array.isArray(aArgs) ? aArgs : []) || "");
    }

    function buildPermissionSheets(oController, aPermissionRules, mHooks) {
        return PermissionPresentation.buildPermissionSheets(aPermissionRules, {
            codeOrder: ["01", "02", "03", "06"],
            scopeLabel: function (oRule) {
                var sScopeKind = String(oRule && oRule.scopeKind || "all").trim().toLowerCase() || "all";
                var sScopeValue = String(oRule && oRule.scopeValue || "ALL").trim() || "ALL";
                return sScopeKind === "bukrs" && sScopeValue.toUpperCase() !== "ALL"
                    ? resolveText(mHooks, oController, MessageKeyConstants.SHELL.PERMISSION_SCOPE_BUKRS, [sScopeValue])
                    : resolveText(mHooks, oController, MessageKeyConstants.SHELL.PERMISSION_SCOPE_ALL);
            },
            codeLabel: function (oRule) {
                return resolveText(mHooks, oController, PERMISSION_TEXT_KEY_MAP[oRule.code] || MessageKeyConstants.SHELL.PERMISSION_UNKNOWN, [oRule.code]);
            }
        });
    }

    function buildUserSummaryText(oController, aPermissionSheets, sBackendSummary, mHooks) {
        return PermissionPresentation.buildSummaryText(
            sBackendSummary,
            aPermissionSheets,
            resolveText(mHooks, oController, MessageKeyConstants.SHELL.USER_PERMISSIONS_EMPTY)
        );
    }

    function buildHeaderUserLabel(sFullName, aPermissionSheets) {
        return PermissionPresentation.buildHeaderLabel(sFullName, aPermissionSheets, " - ");
    }

    function hasResolvedCurrentUser(oCurrentUser) {
        return !!String((oCurrentUser && oCurrentUser.fullName) || "").trim();
    }

    function ensureShellDefaults(oController) {
        var mPatch = {};

        if (!ControllerModelRuntime.shell(oController)) {
            return;
        }
        mPatch[MODEL_PATHS.SHELL_COMPACT_DENSITY] = !!ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_COMPACT_DENSITY, false);
        mPatch[MODEL_PATHS.SHELL_ANIMATION_ENABLED] = ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_ANIMATION_ENABLED, true) !== false;
        mPatch[MODEL_PATHS.SHELL_INVERTED_BLOCK_SCHEME] = !!ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_INVERTED_BLOCK_SCHEME, false);
        mPatch[MODEL_PATHS.SHELL_IS_PHONE_VIEWPORT] = !!ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_IS_PHONE_VIEWPORT, false);
        mPatch[MODEL_PATHS.SHELL_IS_TABLET_VIEWPORT] = !!ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_IS_TABLET_VIEWPORT, false);
        if (!ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_ROOT, null)) {
            mPatch[MODEL_PATHS.SHELL_ROOT] = {
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
        ModelStateRuntime.setMany(oController, SHELL_MODEL, mPatch);
    }

    function syncRuntimeShellState(oStateModel, oShellModel) {
        var oShellLockState;

        if (!oStateModel || !oShellModel) {
            return false;
        }
        oShellLockState = resolveShellLockState(oStateModel);
        return ModelStateRuntime.setManyOnModel(oShellModel, {
            [MODEL_PATHS.SHELL_BUSY]: !!(
                ModelStateRuntime.readOnModel(oStateModel, "/ui/busy/detail", false)
                || ModelStateRuntime.readOnModel(oStateModel, "/isLoading", false)
            ),
            [MODEL_PATHS.SHELL_CURRENT_ROOT_KEY]: oShellLockState.currentRootKey,
            [MODEL_PATHS.SHELL_SESSION_GUID]: String(
                ModelStateRuntime.readOnModel(oStateModel, "/sessionId", "") || ""
            ).trim(),
            [MODEL_PATHS.SHELL_LOCK]: oShellLockState.lock
        });
    }

    function resolveShellLockState(oStateModel) {
        var sActiveRootKey;
        var sEditMode;
        var sLockState;
        var bOwnsActiveLock;

        sActiveRootKey = String(
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, "")
            || ""
        ).trim();
        sEditMode = WorkflowContracts.normalizeEditMode(
            ModelStateRuntime.readOnModel(oStateModel, "/workflow/detail/editMode", WorkflowContracts.EDIT_MODES.READ)
        );
        sLockState = WorkflowContracts.normalizeLockState(
            ModelStateRuntime.readOnModel(oStateModel, "/workflow/detail/lock/state", WorkflowContracts.LOCK_STATES.IDLE)
        );
        bOwnsActiveLock = !!sActiveRootKey
            && sEditMode === WorkflowContracts.EDIT_MODES.EDIT
            && sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
        return {
            currentRootKey: sActiveRootKey,
            lock: {
                ok: bOwnsActiveLock,
                reason: bOwnsActiveLock ? WorkflowContracts.REASONS.OWNED_BY_YOU : WorkflowContracts.REASONS.FREE,
                isKilled: !!ModelStateRuntime.readOnModel(oStateModel, "/isKilled", false)
            }
        };
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
        var oShellLockState;

        if (!ControllerModelRuntime.shell(oController) || !oState) {
            return;
        }

        ensureShellDefaults(oController);
        oShellLockState = resolveShellLockState(oState);
        sSelectedId = RootIdRuntime.resolveFromStateModel(oState);
        sCurrentRouteName = String(ModelStateRuntime.read(oController, STATE_MODEL, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        sMode = LayoutStateRuntime.readMode(oState, WorkflowContracts.EDIT_MODES.READ);
        oCurrentUser = ModelStateRuntime.read(oController, STATE_MODEL, "/currentUser", {}) || {};
        sFullName = hasResolvedCurrentUser(oCurrentUser)
            ? String(oCurrentUser.fullName || "")
            : resolveText(mHooks, oController, MessageKeyConstants.SHELL.USER_LOADING);
        aPermissions = Array.isArray(oCurrentUser.permissions) ? oCurrentUser.permissions.slice() : [];
        aPermissionRules = Array.isArray(oCurrentUser.permissionRules) ? oCurrentUser.permissionRules.slice() : [];
        aPermissionSheets = buildPermissionSheets(oController, aPermissionRules, mHooks);
        sUserSummaryText = buildUserSummaryText(oController, aPermissionSheets, oCurrentUser.summaryText, mHooks);
        bShowHints = !!ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_PERSONALIZATION_SHOW_HINTS, false);
        sFrontendSource = String(
            ModelStateRuntime.read(oController, STATE_MODEL, "/frontendConfigSource", "")
            || ModelStateRuntime.read(oController, STATE_MODEL, ModelPathContracts.BACKEND_MODE, "")
            || "gateway_runtime"
        );
        bSearchWorkspace = !sSelectedId;
        bEditWorkspace = !bSearchWorkspace && sMode === WorkflowContracts.EDIT_MODES.EDIT;

        mShellPatch["/shell/eyebrow"] = bSearchWorkspace
            ? resolveText(mHooks, oController, MessageKeyConstants.SHELL.APP_TITLE)
            : resolveText(mHooks, oController, MessageKeyConstants.SHELL.DETAIL_WORKSPACE_SECTION_TITLE);
        mShellPatch["/shell/productName"] = sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS
            ? resolveText(mHooks, oController, MessageKeyConstants.SHELL.PRODUCT_NAME_ANALYTICS)
            : resolveText(mHooks, oController, MessageKeyConstants.SHELL.PRODUCT_NAME_SEARCH);
        mShellPatch["/shell/routeLabel"] = sCurrentRouteName;
        mShellPatch["/shell/contextSubtitle"] = sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS
            ? resolveText(mHooks, oController, MessageKeyConstants.SHELL.CONTEXT_ANALYTICS)
            : (!sSelectedId ? resolveText(mHooks, oController, MessageKeyConstants.SHELL.CONTEXT_SEARCH)
                : (CreateSentinel.isCreateId(sSelectedId) ? resolveText(mHooks, oController, MessageKeyConstants.SHELL.CONTEXT_DRAFT)
                    : resolveText(mHooks, oController, MessageKeyConstants.SHELL.CONTEXT_DETAIL, [sSelectedId])));
        mShellPatch["/shell/userLabel"] = buildHeaderUserLabel(sFullName, aPermissionSheets);
        mShellPatch["/shell/userMeta"] = sFullName;
        mShellPatch["/shell/userLoginLabel"] = "";
        mShellPatch["/shell/userEnvironmentLabel"] = sFrontendSource;
        mShellPatch["/shell/userPermissions"] = aPermissionSheets;
        mShellPatch["/shell/userSummaryText"] = sUserSummaryText;
        mShellPatch["/shell/userSessionLabel"] = resolveText(mHooks, oController, MessageKeyConstants.SHELL.USER_SESSION_MANAGED);
        mShellPatch["/shell/userSessionState"] = hasResolvedCurrentUser(oCurrentUser)
            ? (aPermissionRules.length || aPermissions.length ? "Success" : "Warning")
            : "Information";
        mShellPatch["/shell/userActionVisible"] = false;
        mShellPatch["/shell/userActionText"] = "";
        mShellPatch["/shell/userActionIcon"] = "sap-icon://employee";
        mShellPatch["/shell/userActionType"] = "Transparent";
        mShellPatch["/shell/userActionKind"] = "";
        mShellPatch["/shell/userActionPassiveText"] = bEditWorkspace ? resolveText(mHooks, oController, MessageKeyConstants.SHELL.LOCK_LOCKED) : "";
        mShellPatch["/shell/userActionHint"] = resolveText(mHooks, oController, MessageKeyConstants.SHELL.USER_HINT_RUNTIME);
        mShellPatch["/shell/userTooltip"] = sUserSummaryText || resolveText(mHooks, oController, MessageKeyConstants.SHELL.USER_TOOLTIP_STANDALONE);
        mShellPatch["/shell/userIcon"] = "sap-icon://employee";
        mShellPatch["/shell/showHints"] = bShowHints;
        mShellPatch[MODEL_PATHS.SHELL_CURRENT_ROOT_KEY] = oShellLockState.currentRootKey;
        mShellPatch[MODEL_PATHS.SHELL_SESSION_GUID] = String(ModelStateRuntime.read(oController, STATE_MODEL, "/sessionId", "") || "").trim();
        mShellPatch[MODEL_PATHS.SHELL_LOCK] = oShellLockState.lock;
        ModelStateRuntime.setMany(oController, SHELL_MODEL, mShellPatch);
        if (typeof oController._markStartupReady === "function") {
            oController._markStartupReady();
        }
    }

    return {
        ensureShellDefaults: ensureShellDefaults,
        syncRuntimeShellState: syncRuntimeShellState,
        syncShellState: syncShellState
    };
});


