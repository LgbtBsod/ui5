sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBehaviorRuntime"
], function (DetailRowBehaviorRuntime) {
    "use strict";

    QUnit.module("framework/DetailRowBehaviorRuntime");

    QUnit.test("expand row delegates only through explicit hooks without workflow mutation", function (assert) {
        var done = assert.async();
        var aCalls = [];
        var oController = {};

        DetailRowBehaviorRuntime.onExpandRows(oController, "check", {
            getSource: function () {
                return { id: "trigger" };
            }
        }, {
            rememberDialogReturnFocus: function (sDialogId, oSource) {
                aCalls.push(["remember", sDialogId, !!oSource]);
            },
            withViewFlag: function (sPath, fnWork) {
                aCalls.push(["flag", sPath]);
                return Promise.resolve(fnWork());
            },
            rowOps: function (mInput) {
                aCalls.push(["rowOps", mInput.entity, mInput.op]);
                return Promise.resolve("ok");
            }
        }).then(function () {
            assert.deepEqual(aCalls, [
                ["remember", "checksExpanded", true],
                ["flag", "/checksExpandedBusy"],
                ["rowOps", "check", "expand"]
            ], "expand flow uses dialog focus + explicit row operation hooks only");
            done();
        });
    });
});
