sap.ui.define([
    "checklist/app/controller/support/AppShellTextSupport",
    "checklist/app/controller/support/AppRetryActionPolicy",
    "checklist/app/controller/support/AppShellUserActionPolicy",
    "checklist/app/service/framework/ClipboardRuntime",
    "checklist/app/service/framework/FocusRuntime",
    "checklist/app/service/framework/FeedbackBannerState",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/SecurityTokenRefresh",
    "checklist/app/service/framework/AppShellCoordinator",
    "checklist/app/controller/support/AppShellUserRefreshSupport",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/util/CreateSentinel",
    "checklist/app/service/framework/ModelStateRuntime"
], function (AppShellTextSupport, AppRetryActionPolicy, AppShellUserActionPolicy, ClipboardRuntime, FocusRuntime, FeedbackBannerState, FeedbackBannerRuntime, SecurityTokenRefresh, AppShellCoordinator, AppShellUserRefreshSupport, NavigationIntentService, CreateSentinel, ModelStateRuntime) {
    "use strict";

    var getText = AppShellTextSupport.getText;
    var SHELL_OVERLAY_FRAGMENTS = {
        notifications: "checklist.app.view.fragment.ShellNotificationsPopover",
        help: "checklist.app.view.fragment.ShellHelpPopover",
        settings: "checklist.app.view.fragment.ShellSettingsPopover",
        user: "checklist.app.view.fragment.ShellUserPopover"
    };

    function openShellOverlayByKey(oController, oEvent, sKey) {
        var sFragment = SHELL_OVERLAY_FRAGMENTS[sKey];
        if (!sFragment) {
            return Promise.resolve();
        }
        return oController._openShellOverlay(oEvent, sKey, sFragment);
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
            NavigationIntentService.navigateToAnalytics(this);
            return Promise.resolve();
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
            var bState = !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            ModelStateRuntime.write(this, "layout", "/personalization/showHints", bState);
            this._syncShellState();
        },

        onToggleCompactDensity: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(
                this,
                "appView",
                "/compactDensity",
                oEvent && oEvent.getParameter && oEvent.getParameter("state")
            );
            this._applyCompactDensityClass();
            return bState;
        },

        onToggleThemeAnimation: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(
                this,
                "appView",
                "/animationEnabled",
                oEvent && oEvent.getParameter && oEvent.getParameter("state")
            );
            AppShellCoordinator.onToggleThemeAnimation(this, bState);
            return bState;
        },

        onToggleBackgroundInteraction: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(
                this,
                "appView",
                "/backgroundInteractive",
                oEvent && oEvent.getParameter && oEvent.getParameter("state")
            );
            AppShellCoordinator.onToggleBackgroundInteractive(this, bState);
            return bState;
        },

        formatGlobalBannerType: function (sSeverity) {
            return FeedbackBannerState.toUi5MessageType(sSeverity);
        },

        onShellUserPrimaryAction: function (oEvent) {
            var sActionKind = String(ModelStateRuntime.read(this, "appView", "/shell/userActionKind", "") || "").trim();
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
            var oTarget = this._oTestUserDialogReturnFocus || (this._mShellOverlayTriggers && this._mShellOverlayTriggers.user);
            FocusRuntime.focusSoon(oTarget);
            this._oTestUserDialogReturnFocus = null;
        }
    };
});
