sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/SmartControlsAdapter"
], function (SmartControlsAdapter) {
    "use strict";

    QUnit.module("framework/SmartControlsAdapter");

    QUnit.test("rebindSearchTable schedules rebind without private control data lock", function (assert) {
        var done = assert.async();
        var iCalls = 0;
        var oSmartTable = {
            rebindTable: function () {
                iCalls += 1;
            },
            getBusy: function () {
                return false;
            }
        };
        var oAdapter = SmartControlsAdapter.create({
            smartTable: oSmartTable,
            smartFilterBar: {
                isInitialised: function () {
                    return true;
                }
            }
        });

        assert.strictEqual(oAdapter.rebindSearchTable(), true, "rebind is accepted");
        assert.strictEqual(oAdapter.rebindSearchTable(), true, "rapid second call is coalesced");

        setTimeout(function () {
            assert.strictEqual(iCalls, 1, "only one debounced rebind reaches the control");
            done();
        }, 140);
    });
});
