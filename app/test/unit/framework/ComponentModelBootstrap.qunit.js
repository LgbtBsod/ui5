sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentModelBootstrap"
], function (ComponentModelBootstrap) {
    "use strict";

    QUnit.module("framework/ComponentModelBootstrap");

    QUnit.test("creates json models and resolves manifest-owned mainService", function (assert) {
        var oModels = {
            stateModel: { id: "state" },
            uiStateModel: { id: "ui" }
        };
        var oMainServiceModel = { id: "mainService" };
        var oComponent = {
            getManifestEntry: function (sPath) {
                return sPath === "/sap.app/dataSources/mainService/uri" ? "/sap/opu/odata/sap/Z_SRV/" : "";
            }
        };

        var oResult = ComponentModelBootstrap.bootstrap(oComponent, {
            ComponentModelInitRuntime: {
                initializeModels: function () {
                    return oModels;
                }
            },
            ComponentMainServiceRuntime: {
                createMainServiceModel: function (oPassedComponent, mPassedDeps, sMainServiceUri) {
                    assert.strictEqual(oPassedComponent, oComponent, "component is forwarded to main service resolver");
                    assert.strictEqual(sMainServiceUri, "/sap/opu/odata/sap/Z_SRV/", "manifest main service uri is used");
                    return oMainServiceModel;
                }
            }
        });

        assert.strictEqual(oResult.models, oModels, "model stage returns initialized json models");
        assert.strictEqual(oResult.mainServiceModel, oMainServiceModel, "main service model is attached to stage result");
    });
});
