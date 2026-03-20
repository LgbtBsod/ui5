sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailStateActionRuntime"
], function (JSONModel, DetailStateActionRuntime) {
    "use strict";

    QUnit.module("framework/DetailStateActionRuntime");

    QUnit.test("workflowActions.save is blocked while detail is busy", function (assert) {
        var done = assert.async();
        var oController = {
            getModel: function (sName) {
                if (sName === "state") {
                    return new JSONModel({
                        ui: { busy: { detail: true } },
                        saveInFlight: false
                    });
                }
                return null;
            }
        };

        DetailStateActionRuntime.workflowActions.save(oController, {
            saveDetail: function () {
                assert.ok(false, "saveDetail must not be called while busy");
                return Promise.resolve(true);
            }
        }).then(function (bSaved) {
            assert.strictEqual(bSaved, false, "save is rejected while busy");
            done();
        });
    });

    QUnit.test("navigationActions.toggleFullscreen delegates layout application", function (assert) {
        var sAppliedLayout = "";
        var oController = {
            getModel: function (sName) {
                if (sName === "state") {
                    return new JSONModel({
                        layout: "TwoColumnsMidExpanded"
                    });
                }
                return null;
            }
        };

        DetailStateActionRuntime.navigationActions.toggleFullscreen(oController, {
            applyLayoutState: function (sLayout) {
                sAppliedLayout = sLayout;
            }
        });

        assert.strictEqual(sAppliedLayout, "MidColumnFullScreen", "fullscreen layout is applied through navigation action owner");
    });
});
