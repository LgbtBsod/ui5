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
                isDark: !!oApplied.isDark
            }), "appView");

            var oState = this.getModel("state");
            if (oState) {
                this._oRequiresLoginBinding = oState.bindProperty("/requiresUserLogin");
                this._oRequiresLoginBinding.attachChange(this._syncTestUserDialogState, this);

                this._oLayoutBinding = oState.bindProperty("/layout");
                this._oLayoutBinding.attachChange(this._scheduleFlexibleColumnSplitApply, this);

                this._oSplitBinding = oState.bindProperty("/columnSplitPercent");
                this._oSplitBinding.attachChange(this._scheduleFlexibleColumnSplitApply, this);
            }
            this._syncTestUserDialogState();
            this._fnOnWindowResize = this._scheduleFlexibleColumnSplitApply.bind(this);
            window.addEventListener("resize", this._fnOnWindowResize);
            this._scheduleFlexibleColumnSplitApply();
        },


        onAfterRendering: function () {
            this._scheduleFlexibleColumnSplitApply();
        },

        _scheduleFlexibleColumnSplitApply: function () {
            if (this._iSplitRaf) {
                return;
            }

            this._iSplitRaf = window.requestAnimationFrame(function () {
                this._iSplitRaf = null;
                this._applyFlexibleColumnSplit();
            }.bind(this));
        },

        _isTwoColumnLayout: function (sLayout) {
            return String(sLayout || "").indexOf("TwoColumns") === 0;
        },

        _resolveSplitPercent: function () {
            var oState = this.getModel("state");
            var iPercent = Number(oState && oState.getProperty("/columnSplitPercent"));
            if (!Number.isFinite(iPercent)) {
                iPercent = 38;
            }
            return Math.max(20, Math.min(80, Math.round(iPercent)));
        },

        _applyFlexibleColumnSplit: function () {
            var oState = this.getModel("state");
            var oFcl = this.byId("fcl");
            if (!oState || !oFcl) {
                return;
            }

            var sLayout = oState.getProperty("/layout") || "OneColumn";
            var bTwoColumns = this._isTwoColumnLayout(sLayout);
            var iBegin = this._resolveSplitPercent();
            var iMid = Math.max(100 - iBegin, 20);
            var iViewportWidth = window.innerWidth || document.documentElement.clientWidth || 0;

            if (!bTwoColumns || iViewportWidth <= 1100) {
                oFcl.removeStyleClass("fclCustomSplitActive");
                this._sAppliedSplitSignature = "";
                return;
            }

            var sSignature = [sLayout, iBegin, iMid].join(":");
            if (this._sAppliedSplitSignature === sSignature) {
                return;
            }

            oFcl.addStyleClass("fclCustomSplitActive");
            var oDomRef = oFcl.getDomRef();
            if (!oDomRef) {
                return;
            }
            oDomRef.style.setProperty("--fcl-begin-col", iBegin + "%");
            oDomRef.style.setProperty("--fcl-mid-col", iMid + "%");
            this._sAppliedSplitSignature = sSignature;
        },

        onToggleTheme: function () {
            var oResult = this.toggleTheme();
            var oAppView = this.getView().getModel("appView");
            oAppView.setProperty("/isDark", !!oResult.isDark);
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
            if (this._iSplitRaf) {
                window.cancelAnimationFrame(this._iSplitRaf);
                this._iSplitRaf = null;
            }
            if (this._fnOnWindowResize) {
                window.removeEventListener("resize", this._fnOnWindowResize);
                this._fnOnWindowResize = null;
            }

            if (this._oRequiresLoginBinding) {
                this._oRequiresLoginBinding.detachChange(this._syncTestUserDialogState, this);
                this._oRequiresLoginBinding.destroy();
                this._oRequiresLoginBinding = null;
            }
            if (this._oLayoutBinding) {
                this._oLayoutBinding.detachChange(this._scheduleFlexibleColumnSplitApply, this);
                this._oLayoutBinding.destroy();
                this._oLayoutBinding = null;
            }
            if (this._oSplitBinding) {
                this._oSplitBinding.detachChange(this._scheduleFlexibleColumnSplitApply, this);
                this._oSplitBinding.destroy();
                this._oSplitBinding = null;
            }
        }
    });
});
