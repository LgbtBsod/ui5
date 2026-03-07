sap.ui.define([
    "sap_ui5/controller/TestUserDialog.controller",
    "sap_ui5/controller/support/AppShellTextSupport",
    "sap_ui5/controller/support/AppRetryActionPolicy",
    "sap_ui5/controller/support/AppShellUserActionPolicy",
    "sap_ui5/service/framework/ClipboardRuntime",
    "sap_ui5/service/framework/FocusRuntime",
    "sap_ui5/service/framework/FeedbackBannerState",
    "sap_ui5/service/framework/FeedbackBannerRuntime",
    "sap_ui5/service/framework/SecurityTokenRefresh",
    "sap_ui5/service/framework/AppShellCoordinator",
    "sap_ui5/controller/support/AppShellUserRefreshSupport",
    "sap_ui5/util/CreateSentinel",
    "sap_ui5/controller/support/ControllerModelWriteSupport"
], function (TestUserDialogController, AppShellTextSupport, AppRetryActionPolicy, AppShellUserActionPolicy, ClipboardRuntime, FocusRuntime, FeedbackBannerState, FeedbackBannerRuntime, SecurityTokenRefresh, AppShellCoordinator, AppShellUserRefreshSupport, CreateSentinel, ControllerModelWriteSupport) {
    "use strict";

    var getText = AppShellTextSupport.getText;
    var SHELL_OVERLAY_FRAGMENTS = {
        notifications: "sap_ui5.view.fragment.ShellNotificationsPopover",
        help: "sap_ui5.view.fragment.ShellHelpPopover",
        settings: "sap_ui5.view.fragment.ShellSettingsPopover",
        user: "sap_ui5.view.fragment.ShellUserPopover"
    };

    function openShellOverlayByKey(oController, oEvent, sKey) {
        var sFragment = SHELL_OVERLAY_FRAGMENTS[sKey];
        if (!sFragment) {
            return Promise.resolve();
        }
        return oController._openShellOverlay(oEvent, sKey, sFragment);
    }

    function readSwitchState(oEvent) {
        return !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
    }

    function setAppViewBoolean(oController, sPath, bValue) {
        ControllerModelWriteSupport.set(oController, "appView", sPath, !!bValue);
        return !!bValue;
    }

    return {
        onToggleTheme: function (oEvent) {
            AppShellCoordinator.onToggleTheme(this, null);
            this._syncShellState();
        },

        onSelectThemeMode: function (oEvent) {
            var sMode = String(oEvent && oEvent.getParameter && oEvent.getParameter("key") || "").trim();
            if (!sMode) {
                return;
            }
            AppShellCoordinator.onSetThemeMode(this, sMode);
            this._syncShellState();
        },

        onConfirmTestUser: function () {
            return AppShellCoordinator.onConfirmTestUser(this, TestUserDialogController.confirm);
        },

        onDialogClosed: function () {
            AppShellCoordinator.onDialogClosed(this);
            this._restoreTestUserDialogFocus();
        },

        onOpenShellNotifications: function (oEvent) {
            return openShellOverlayByKey(this, oEvent, "notifications");
        },

        onOpenShellHelp: function (oEvent) {
            return openShellOverlayByKey(this, oEvent, "help");
        },

        onOpenShellSettings: function (oEvent) {
            return openShellOverlayByKey(this, oEvent, "settings");
        },

        onOpenShellAnalytics: function (oEvent) {
            var oSearchView = this.byId("searchPaneHost");
            var oSearchController = oSearchView && oSearchView.getController && oSearchView.getController();
            if (!oSearchController || typeof oSearchController.onOpenWorkflowAnalytics !== "function") {
                if (typeof this.showI18nToast === "function") {
                    this.showI18nToast("analyticsUnavailableToast");
                }
                return Promise.resolve();
            }
            return oSearchController.onOpenWorkflowAnalytics(oEvent);
        },

        onOpenShellUserMenu: function (oEvent) {
            return openShellOverlayByKey(this, oEvent, "user");
        },

        onGlobalBannerRetry: function () {
            var oState = this._getStateModel();
            var sAction = String(FeedbackBannerRuntime.getBannerProperty(oState, "global", "retryAction") || "").trim();
            return AppRetryActionPolicy.runRetry(this, sAction);
        },

        onCopyFeedbackCorrelationId: function () {
            var oState = this._getStateModel();
            var sCorrelationId = String(FeedbackBannerRuntime.getBannerProperty(oState, "global", "correlationId") || "").trim();
            if (!sCorrelationId) {
                return;
            }
            ClipboardRuntime.writeText(sCorrelationId).then(function (bCopied) {
                if (bCopied && typeof this.showI18nToast === "function") {
                    this.showI18nToast("correlationIdCopied");
                }
            }.bind(this));
        },

        onToggleShellHints: function (oEvent) {
            var bState = readSwitchState(oEvent);
            ControllerModelWriteSupport.set(this, "layout", "/personalization/showHints", bState);
            this._syncShellState();
        },

        onToggleCompactDensity: function (oEvent) {
            var bState = setAppViewBoolean(this, "/compactDensity", readSwitchState(oEvent));
            this._applyCompactDensityClass();
            return bState;
        },

        onToggleThemeAnimation: function (oEvent) {
            var bState = setAppViewBoolean(this, "/animationEnabled", readSwitchState(oEvent));
            AppShellCoordinator.onToggleThemeAnimation(this, bState);
            return bState;
        },

        onToggleBackgroundInteraction: function (oEvent) {
            var bState = setAppViewBoolean(this, "/backgroundInteractive", readSwitchState(oEvent));
            AppShellCoordinator.onToggleBackgroundInteractive(this, bState);
            return bState;
        },

        formatGlobalBannerType: function (sSeverity) {
            return FeedbackBannerState.toUi5MessageType(sSeverity);
        },

        onOpenTestUserDialog: function (oEvent) {
            var oSource = (oEvent && oEvent.getSource && oEvent.getSource()) || null;
            this._oTestUserDialogReturnFocus = (this._mShellOverlayTriggers && this._mShellOverlayTriggers.user) || oSource || this._oTestUserDialogReturnFocus || null;
            this._closeShellOverlay("user", true);
            AppShellCoordinator.requestTestUserDialog(this);
        },

        onShellUserPrimaryAction: function (oEvent) {
            var oAppView = this._getAppViewModel();
            var sActionKind = String(oAppView && oAppView.getProperty ? oAppView.getProperty("/shell/userActionKind") || "" : "").trim();
            return AppShellUserActionPolicy.runPrimaryAction(this, sActionKind, oEvent);
        },

        onRefreshShellUser: function () {
            return AppShellUserRefreshSupport.refreshCurrentUser(this);
        },

        _refreshShellUserContext: function () {
            var that = this;
            var oModel = this.getModel && this.getModel();
            var fnFinalize = function () {
                that._closeShellOverlay("user");
                that._syncShellState();
                that._syncShellMetrics();
            };
            return SecurityTokenRefresh.refresh(oModel).then(function () {
                return that.onRefreshShellUser().then(function () {
                    fnFinalize();
                });
            });
        },

        _restoreTestUserDialogFocus: function () {
            var oState = this._getStateModel();
            var oTarget;
            if (oState && oState.getProperty && oState.getProperty("/requiresUserLogin")) {
                return;
            }
            oTarget = this._oTestUserDialogReturnFocus || (this._mShellOverlayTriggers && this._mShellOverlayTriggers.user);
            FocusRuntime.focusSoon(oTarget);
            this._oTestUserDialogReturnFocus = null;
        }
    };
});
