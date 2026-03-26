sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockEventsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (JSONModel, ComponentLockEventsRuntime, StatePaths) {
    "use strict";

    QUnit.module("framework/ComponentLockEventsRuntime.lifecycleBindings");

    QUnit.test("listener does not auto-resume pending navigation on settled save state", function (assert) {
        var iResumeCalls = 0;
        var oStateModel = new JSONModel({
            activeObjectId: "CHK-1",
            saveInFlight: false,
            isDirty: false,
            pendingNavigationIntent: {
                routeName: "analytics",
                routeArgs: {},
                owner: "navigationGuard",
                resumeMode: "afterGuardedSave"
            }
        });
        var oComponent = {};

        ComponentLockEventsRuntime.attachLifecycleBindings({
            component: oComponent,
            stateModel: oStateModel,
            shellModel: new JSONModel({}),
            statePaths: StatePaths,
            emitTelemetry: function () {},
            publishTabSignal: function () {},
            resumePendingNavigationIntent: function () { iResumeCalls += 1; },
            telemetryRuntime: {
                stateValue: function () { return {}; }
            },
            layoutStateRuntime: {
                readMode: function () { return "READ"; },
                readLockState: function () { return "READ_ONLY"; }
            }
        });

        oComponent._fnStateModelPropertyChange({
            getParameter: function (sName) {
                if (sName === "path") {
                    return StatePaths.SAVE_IN_FLIGHT;
                }
                if (sName === "value") {
                    return false;
                }
                return undefined;
            }
        });

        assert.strictEqual(iResumeCalls, 0, "listener does not resume navigation implicitly");
    });
});
