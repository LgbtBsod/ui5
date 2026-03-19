sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBindingRuntime"
], function (DetailRowBindingRuntime) {
    "use strict";

    QUnit.module("framework/DetailRowBindingRuntime");

    QUnit.test("formats row numbers from entity spec", function (assert) {
        assert.strictEqual(
            DetailRowBindingRuntime.formatRowNumber({ ChecksNum: 5 }, "check"),
            5,
            "check number field resolves through helper"
        );
        assert.strictEqual(
            DetailRowBindingRuntime.formatRowNumber({ BarriersNum: 8 }, "barrier"),
            8,
            "barrier number field resolves through helper"
        );
    });

    QUnit.test("binds selected collection context from spec", function (assert) {
        var oBound = null;
        var oControl = {
            getBindingContext: function () {
                return null;
            },
            bindElement: function (mBinding) {
                oBound = mBinding;
            }
        };

        DetailRowBindingRuntime.bindSelectedCollectionContext(oControl, { rowsPath: "/checks" });

        assert.deepEqual(oBound, {
            path: "/checks",
            model: "selected"
        }, "selected collection binding is derived from helper spec");
    });
});
