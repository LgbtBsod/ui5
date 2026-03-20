sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (Ui5RuntimeFacade) {
    "use strict";

    QUnit.module("framework/Ui5RuntimeFacade");

    QUnit.test("getI18nBundle returns bundle from named model when available", function (assert) {
        var oBundle = { key: "bundle" };
        var oCore = Ui5RuntimeFacade.getCore();
        var fnOriginalGetModel = oCore.getModel;

        oCore.getModel = function (sName) {
            if (sName !== "i18n") {
                return null;
            }
            return {
                getResourceBundle: function () {
                    return oBundle;
                }
            };
        };

        assert.strictEqual(Ui5RuntimeFacade.getI18nBundle(), oBundle, "Facade resolves the i18n resource bundle");

        oCore.getModel = fnOriginalGetModel;
    });

    QUnit.test("getLanguageTag tolerates missing configuration chain", function (assert) {
        var oCore = Ui5RuntimeFacade.getCore();
        var fnOriginalGetConfiguration = oCore.getConfiguration;

        oCore.getConfiguration = function () {
            return null;
        };

        assert.strictEqual(Ui5RuntimeFacade.getLanguageTag(), "", "Missing configuration returns an empty language tag");

        oCore.getConfiguration = fnOriginalGetConfiguration;
    });
});
