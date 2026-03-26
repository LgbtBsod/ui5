sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLifecycleRuntime"
], function (JSONModel, ComponentLifecycleRuntime) {
    "use strict";

    QUnit.module("framework/ComponentLifecycleRuntime.boot");

    QUnit.test("finalizeBootSuccess does not overwrite cache state with synthetic empty snapshot", function (assert) {
        var oStateModel = new JSONModel({});
        var oCacheState = {
            pristineSnapshot: [{ id: "CHK-1" }],
            lastServerState: { fetchedAt: "existing" },
            keyMapping: { keep: true }
        };

        ComponentLifecycleRuntime.finalizeBootSuccess({
            stateModel: oStateModel,
            cacheState: oCacheState,
            cacheAt: "cache-at",
            readyAt: "2026-03-22T00:00:00.000Z",
            tabSessionId: "tab-1",
            serverState: null,
            checkLists: []
        });

        assert.deepEqual(oCacheState.pristineSnapshot, [{ id: "CHK-1" }], "existing snapshot is preserved");
        assert.deepEqual(oCacheState.lastServerState, { fetchedAt: "existing" }, "existing server state is preserved");
        assert.strictEqual(oStateModel.getProperty("/readiness/app/ready"), true, "boot readiness is still marked");
    });
});
