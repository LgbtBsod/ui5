sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/WorkflowTelemetry"
], function (WorkflowTelemetry) {
    "use strict";

    function create(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;

        return {
            track: function (mEvent) {
                return WorkflowTelemetry.emit((mEvent && mEvent.event) || "event", {
                    stateModel: oStateModel,
                    payload: Object.assign({
                        outcome: "tracked"
                    }, (mEvent && mEvent.payload) || {})
                });
            },

            snapshot: function (mArgsSnapshot) {
                return Promise.resolve(WorkflowTelemetry.emit("snapshot", {
                    stateModel: oStateModel,
                    payload: {
                        context: (mArgsSnapshot && mArgsSnapshot.context) || {},
                        outcome: "snapshot"
                    }
                }));
            }
        };
    }

    return {
        create: create
    };
});
