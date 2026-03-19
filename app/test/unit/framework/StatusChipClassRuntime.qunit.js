sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/StatusChipClassRuntime"
], function (StatusChipClassRuntime) {
    "use strict";

    QUnit.module("StatusChipClassRuntime");

    function createChip(sState) {
        var aClasses = ["statusChipSemantic"];
        var mData = {};
        var oDelegate = null;
        return {
            _state: sState,
            isA: function (sName) { return sName === "sap.m.ObjectStatus"; },
            hasStyleClass: function (sName) { return aClasses.indexOf(sName) > -1; },
            addStyleClass: function (sName) {
                if (aClasses.indexOf(sName) === -1) {
                    aClasses.push(sName);
                }
            },
            removeStyleClass: function (sName) {
                aClasses = aClasses.filter(function (sClassName) {
                    return sClassName !== sName;
                });
            },
            getState: function () { return this._state; },
            setState: function (sNewState) { this._state = sNewState; },
            addEventDelegate: function (oNewDelegate) { oDelegate = oNewDelegate; },
            data: function (sKey, vValue) {
                if (arguments.length === 2) {
                    mData[sKey] = vValue;
                }
                return mData[sKey];
            },
            getDelegate: function () { return oDelegate; },
            getClasses: function () { return aClasses.slice(); }
        };
    }

    QUnit.test("syncView assigns chipState class and refreshes it after rerender", function (assert) {
        var oChip = createChip("Success");
        var oView = {
            findAggregatedObjects: function () {
                return [oChip];
            }
        };
        var oController = {
            getView: function () {
                return oView;
            }
        };

        StatusChipClassRuntime.syncView(oController);
        assert.ok(oChip.getClasses().indexOf("chipStateSuccess") > -1, "success class applied");

        oChip.setState("Warning");
        oChip.getDelegate().onAfterRendering();

        assert.ok(oChip.getClasses().indexOf("chipStateWarning") > -1, "warning class applied after rerender");
        assert.strictEqual(oChip.getClasses().indexOf("chipStateSuccess"), -1, "stale class removed");
    });
});
