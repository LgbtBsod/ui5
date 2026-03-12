sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AppShellActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ClipboardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerState",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/AppShellCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts"
], function (ActionContract, AppShellActionRuntime, ClipboardRuntime, FocusRuntime, FeedbackBannerState, FeedbackBannerRuntime, AppShellCoordinator, NavigationIntentService, ModelStateRuntime, UiDecisionCoordinator, NavigationContracts) {
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

        onOpenShellHelp: function (oEvent) {
            return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "help");
        },

        onOpenShellSettings: function (oEvent) {
            return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "settings");
        },

        onOpenShellAnalytics: function (oEvent) {
            var oLayout = this.byId && this.byId("mainFcl");
            var oAnalyticsPage = this.byId && this.byId(NavigationContracts.MID_COLUMN_PAGE_IDS.ANALYTICS);
            ModelStateRuntime.write(this, "state", "/currentRouteName", NavigationContracts.ROUTES.ANALYTICS);
            ModelStateRuntime.write(this, "state", "/layout", NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN);
            if (oLayout && oAnalyticsPage && typeof oLayout.toMidColumnPage === "function") {
                oLayout.toMidColumnPage(oAnalyticsPage);
                if (typeof oLayout.setLayout === "function") {
                    oLayout.setLayout(NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN);
                }
            }
            NavigationIntentService.navigateToAnalytics(this);
            if (typeof this._syncLayoutState === "function") {
                this._syncLayoutState();
            }
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

        onToggleInvertedBlockScheme: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(this, "appView", "/invertedBlockScheme", false);
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
