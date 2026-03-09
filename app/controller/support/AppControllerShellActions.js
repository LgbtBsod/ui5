sap.ui.define([
    "checklist/app/service/framework/ActionContract",
    "checklist/app/controller/support/AppShellActionRuntime",
    "checklist/app/service/framework/ClipboardRuntime",
    "checklist/app/service/framework/FocusRuntime",
    "checklist/app/service/framework/FeedbackBannerState",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/AppShellCoordinator",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/UiDecisionCoordinator"
], function (ActionContract, AppShellActionRuntime, ClipboardRuntime, FocusRuntime, FeedbackBannerState, FeedbackBannerRuntime, AppShellCoordinator, NavigationIntentService, ModelStateRuntime, UiDecisionCoordinator) {
    "use strict";

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
            return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "notifications");
        },

        onOpenShellHelp: function (oEvent) {
            return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "help");
        },

        onOpenShellSettings: function (oEvent) {
            return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "settings");
        },

        onOpenShellAnalytics: function (oEvent) {
            NavigationIntentService.navigateToAnalytics(this);
            return Promise.resolve();
        },

        onOpenShellUserMenu: function (oEvent) {
            return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "user");
        },

        onGlobalBannerRetry: function () {
            return AppShellActionRuntime.runGlobalBannerRetry(this);
        },

        onCopyFeedbackCorrelationId: function () {
            var oState = this._getStateModel();
            var sCorrelationId = String(FeedbackBannerRuntime.getBannerProperty(oState, "global", "correlationId") || "").trim();
            if (!sCorrelationId) {
                return;
            }
            ClipboardRuntime.writeText(sCorrelationId).then(function (bCopied) {
                if (bCopied) {
                    UiDecisionCoordinator.notifyCorrelationCopied({ controller: this });
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

        onToggleInvertedBlockScheme: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(
                this,
                "appView",
                "/invertedBlockScheme",
                oEvent && oEvent.getParameter && oEvent.getParameter("state")
            );
            this._applyInvertedBlockSchemeClass();
            return bState;
        },

        formatGlobalBannerType: function (sSeverity) {
            return FeedbackBannerState.toUi5MessageType(sSeverity);
        },

        onShellUserPrimaryAction: function (oEvent) {
            var sActionKind = String(ModelStateRuntime.read(this, "appView", "/shell/userActionKind", "") || "").trim();
            if (ActionContract.normalizeShellUserAction(sActionKind) === ActionContract.SHELL_USER_ACTIONS.REFRESH_CONTEXT) {
                return Promise.resolve(this._refreshShellUserContext());
            }
            return Promise.resolve();
        },

        onRefreshShellUser: function () {
            return AppShellActionRuntime.refreshCurrentUser(this);
        },

        _refreshShellUserContext: function () {
            return AppShellActionRuntime.refreshShellUserContext(this);
        },

        _restoreTestUserDialogFocus: function () {
            var oTarget = this._oTestUserDialogReturnFocus || (this._mShellOverlayTriggers && this._mShellOverlayTriggers.user);
            FocusRuntime.focusSoon(oTarget);
            this._oTestUserDialogReturnFocus = null;
        }
    };
});
