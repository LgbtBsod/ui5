sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (JSONModel, AnalyticsRefreshRuntime, SchedulingRuntime) {
    "use strict";

    function createController() {
        return {
            _iAnalyticsRefreshPollToken: 0,
            _view: new JSONModel({ refreshState: {} })
        };
    }

    QUnit.module("AnalyticsRefreshRuntime", {
        beforeEach: function () {
            this._fnWait = SchedulingRuntime.wait;
            SchedulingRuntime.wait = function () {
                return Promise.resolve();
            };
        },
        afterEach: function () {
            SchedulingRuntime.wait = this._fnWait;
        }
    });

    QUnit.test("invalidatePolls stops recursive polling updates", function (assert) {
        var done = assert.async();
        var oController = createController();
        var iCalls = 0;

        AnalyticsRefreshRuntime.pollRefreshStateUntilSettled(oController, 2, function () {
            iCalls += 1;
            if (iCalls === 1) {
                AnalyticsRefreshRuntime.invalidatePolls(oController);
                return { status: "RUNNING", isRunning: true };
            }
            return { status: "DONE", isRunning: false };
        }).then(function (oState) {
            assert.strictEqual(iCalls, 1, "polling stops after invalidation");
            assert.strictEqual(String(oState.status || ""), "RUNNING", "current state returns without further recursion");
            done();
        });
    });
});
