sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel",
    "sap/ui/core/Fragment",
    "sap_ui5/controller/TestUserDialog.controller",
    "sap_ui5/infra/navigation/RouteModeCoordinator",
    "sap_ui5/infra/ui/SplitterDragGuard"
], function (BaseController, JSONModel, Fragment, TestUserDialogController, RouteModeCoordinator, SplitterDragGuard) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.App", {

        onInit: function () {
            var oApplied = this.applyStoredTheme();
            this.setModel(new JSONModel({
                isDark: false
            }), "appView");
            this._syncThemeState("init", oApplied);

            var oState = this.getModel("state");
            if (oState) {
                if (!oState.getProperty("/splitLayoutMode")) {
                    oState.setProperty("/splitLayoutMode", "single");
                }
                if (!oState.getProperty("/selectedId")) {
                    oState.setProperty("/selectedId", null);
                }
                this._oRequiresLoginBinding = oState.bindProperty("/requiresUserLogin");
                this._oRequiresLoginBinding.attachChange(this._syncTestUserDialogState, this);
            }

            this._oRouteModeCoordinator = new RouteModeCoordinator({
                router: this.getRouter(),
                stateModel: oState,
                splitter: this.byId("mainSplitter"),
                searchPaneHost: this.byId("searchPaneHost"),
                detailPaneHost: this.byId("detailPaneHost")
            });
            this._oRouteModeCoordinator.start();

            this._oSplitterDragGuard = new SplitterDragGuard(this.byId("mainSplitter"));
            this._oSplitterDragGuard.start();

            this._syncTestUserDialogState();
        },

        onToggleTheme: function () {
            var oResult = this.toggleTheme();
            this._syncThemeState("toggle", oResult);
        },

        _syncThemeState: function (sSource, oThemeResult) {
            var oAppView = this.getView().getModel("appView");
            var sStoredTheme = this.getCurrentTheme();
            var bIsDark = sStoredTheme === "sap_horizon_dark";
            if (oAppView) {
                oAppView.setProperty("/isDark", bIsDark);
            }
            console.info("[theme] sync", {
                source: sSource || "unknown",
                storedTheme: sStoredTheme,
                appliedTheme: oThemeResult && oThemeResult.theme,
                isDark: bIsDark,
                expectedIcon: bIsDark ? "weather-sunny" : "clear-night"
            });
        },

        _syncTestUserDialogState: function () {
            var oState = this.getModel("state");
            if (!oState) {
                return;
            }

            var bRequiresLogin = !!oState.getProperty("/requiresUserLogin");
            if (bRequiresLogin) {
                this._openTestUserDialog();
                return;
            }

            if (this._oTestUserDialog) {
                this._oTestUserDialog.close();
            }
        },

        _openTestUserDialog: function () {
            if (this._oTestUserDialog) {
                this._oTestUserDialog.open();
                return;
            }

            Fragment.load({
                id: this.getView().getId(),
                name: "sap_ui5.view.fragment.TestUserDialog",
                controller: this
            }).then(function (oDialog) {
                this._oTestUserDialog = oDialog;
                this.getView().addDependent(oDialog);
                oDialog.open();
            }.bind(this));
        },

        onConfirmTestUser: function () {
            TestUserDialogController.confirm(this).then(function (bSuccess) {
                if (bSuccess && this._oTestUserDialog) {
                    this._oTestUserDialog.close();
                }
            }.bind(this));
        },

        onDialogClosed: function () {
            var oState = this.getModel("state");
            if (oState && oState.getProperty("/requiresUserLogin") && this._oTestUserDialog) {
                this._oTestUserDialog.open();
            }
        },

        onExit: function () {
            if (this._oRouteModeCoordinator) {
                this._oRouteModeCoordinator.stop();
                this._oRouteModeCoordinator = null;
            }

            if (this._oSplitterDragGuard) {
                this._oSplitterDragGuard.stop();
                this._oSplitterDragGuard = null;
            }

            if (this._oRequiresLoginBinding) {
                this._oRequiresLoginBinding.detachChange(this._syncTestUserDialogState, this);
                this._oRequiresLoginBinding.destroy();
                this._oRequiresLoginBinding = null;
            }
        }
    });
});
