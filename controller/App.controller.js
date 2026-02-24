sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel",
    "sap/ui/core/Fragment",
    "sap_ui5/controller/TestUserDialog.controller"
], function (BaseController, JSONModel, Fragment, TestUserDialogController) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.App", {

        onInit: function () {
            var oApplied = this.applyStoredTheme();
            this.setModel(new JSONModel({
                isDark: !!oApplied.isDark,
                themeTitle: (oApplied.meta && oApplied.meta.title) || "",
                themeSubtitle: (oApplied.meta && oApplied.meta.subtitle) || ""
            }), "appView");

            var oState = this.getModel("state");
            if (oState) {
                oState.bindProperty("/requiresUserLogin").attachChange(this._syncTestUserDialogState, this);
            }
            this._syncTestUserDialogState();
        },

        onToggleTheme: function () {
            var oResult = this.toggleTheme();
            var oAppView = this.getView().getModel("appView");
            oAppView.setProperty("/isDark", !!oResult.isDark);
            oAppView.setProperty("/themeTitle", (oResult.meta && oResult.meta.title) || "");
            oAppView.setProperty("/themeSubtitle", (oResult.meta && oResult.meta.subtitle) || "");
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
        }
    });
});
