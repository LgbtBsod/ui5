sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentMainServiceRuntime"
], function (ComponentMainServiceRuntime) {
    "use strict";

    QUnit.module("framework/ComponentMainServiceRuntime");

    QUnit.test("uses manifest-owned mainService model instead of creating a new one", function (assert) {
        var oMainServiceModel = {
            sServiceUrl: "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/"
        };
        var oCaptured = null;
        var oComponent = {
            getModel: function (sName) {
                return sName === "mainService" ? oMainServiceModel : null;
            }
        };

        var oResolved = ComponentMainServiceRuntime.createMainServiceModel(oComponent, {
            GatewayClient: {
                setModel: function (oModel, mOptions) {
                    oCaptured = {
                        model: oModel,
                        options: mOptions
                    };
                }
            }
        }, "/unused");

        assert.strictEqual(oResolved, oMainServiceModel, "existing manifest model is reused");
        assert.strictEqual(oCaptured.model, oMainServiceModel, "gateway client receives the same model instance");
        assert.strictEqual(oCaptured.options.serviceUrl, "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV", "service url is normalized from manifest-owned model");
    });
});
