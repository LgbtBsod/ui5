sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts"
], function (JSONModel, AnalyticsLoadRuntime, ModelContracts, AnalyticsStateConstants) {
    "use strict";

    function createController() {
        var oViewModel = new JSONModel({
            selectedYear: "",
            compareYear: "",
            selectedSource: "ALL"
        });
        var oStateModel = new JSONModel({
            readiness: {
                analytics: {}
            }
        });

        return {
            getModel: function (sName) {
                return sName === ModelContracts.MODELS.VIEW ? oViewModel : oStateModel;
            }
        };
    }

    QUnit.module("AnalyticsLoadRuntime");

    QUnit.test("writes canonical error status when year input is invalid", function (assert) {
        var oController = createController();

        return AnalyticsLoadRuntime.loadAnalytics(oController, "manual", {
            buildCtx: function () { return {}; },
            buildYearOptions: function () { return []; },
            buildCompareYearOptions: function () { return []; },
            setCompareYearValidation: function () {},
            applyComparisonMetricSelection: function () {},
            applyBuilderSelection: function () {},
            syncAnalyticsContextHints: function () {}
        }).then(function (bLoaded) {
            assert.strictEqual(bLoaded, false, "load is blocked");
            assert.strictEqual(
                oController.getModel(ModelContracts.MODELS.STATE).getProperty("/readiness/analytics/status"),
                AnalyticsStateConstants.LOAD_STATUS.ERROR,
                "canonical analytics error status was written"
            );
        });
    });
});
