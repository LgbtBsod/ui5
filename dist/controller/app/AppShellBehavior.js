sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/controller/app/AppShellActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ClipboardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerState",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/AppShellCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (ActionContract, AppShellActionRuntime, ClipboardRuntime, FocusRuntime, FeedbackBannerState, FeedbackBannerRuntime, AppShellCoordinator, NavigationIntentService, ModelStateRuntime, UiDecisionCoordinator, NavigationContracts, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var STATE_MODEL = MODELS.STATE;
    var APP_VIEW_MODEL = MODELS.APP_VIEW;
    var LAYOUT_MODEL = MODELS.LAYOUT;

    return {
        onToggleTheme: function () {
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
        onOpenShellHelp: function (oEvent) { return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "help"); },
        onOpenShellSettings: function (oEvent) { return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "settings"); },
        onOpenShellAnalytics: function () {
            NavigationIntentService.navigateToAnalytics(this);
        },
        onOpenShellUserMenu: function (oEvent) { return AppShellActionRuntime.openShellOverlayByKey(this, oEvent, "user"); },
        onGlobalBannerRetry: function () { return AppShellActionRuntime.runGlobalBannerRetry(this); },
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
            ModelStateRuntime.write(this, LAYOUT_MODEL, "/personalization/showHints", bState);
            this._syncShellState();
        },
        onToggleCompactDensity: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(this, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_COMPACT_DENSITY, oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            this._applyCompactDensityClass();
            return bState;
        },
        onToggleThemeAnimation: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(this, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_ANIMATION_ENABLED, oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            AppShellCoordinator.onToggleThemeAnimation(this, bState);
            return bState;
        },
        onToggleInvertedBlockScheme: function () {
            var bCurrent = !!ModelStateRuntime.read(this, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_INVERTED_BLOCK_SCHEME, false);
            var bState = ModelStateRuntime.writeBoolean(this, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_INVERTED_BLOCK_SCHEME, !bCurrent);
            this._applyInvertedBlockSchemeClass();
            return bState;
        },
        formatGlobalBannerVisible: function (bVisible, sScope) {
            return !!bVisible && String(sScope || "global").toLowerCase() === "global";
        },
        formatGlobalBannerCorrelationVisible: function (sCorrelationId, sScope) {
            return !!String(sCorrelationId || "").trim() && String(sScope || "global").toLowerCase() === "global";
        },
        formatRouteBannerVisible: function (bVisible, sScope) {
            return !!bVisible && String(sScope || "").toLowerCase() === "route";
        },
        formatGlobalBannerType: function (sSeverity) { return FeedbackBannerState.toUi5MessageType(sSeverity); },
        onShellUserPrimaryAction: function () {
            var sActionKind = String(ModelStateRuntime.read(this, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_SHELL_USER_ACTION_KIND, "") || "").trim();
            if (ActionContract.normalizeShellUserAction(sActionKind) === ActionContract.SHELL_USER_ACTIONS.REFRESH_CONTEXT) {
                return this._refreshShellUserContext();
            }
            return undefined;
        },
        onRefreshShellUser: function () { return AppShellActionRuntime.refreshCurrentUser(this); },
        _refreshShellUserContext: function () { return AppShellActionRuntime.refreshShellUserContext(this); },
        _restoreTestUserDialogFocus: function () {
            var oTarget = this._oTestUserDialogReturnFocus || (this._mShellOverlayTriggers && this._mShellOverlayTriggers.user);
            FocusRuntime.focusSoon(oTarget);
            this._oTestUserDialogReturnFocus = null;
        }
    };
});
