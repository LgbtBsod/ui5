sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/UxTelemetry"
], function (UxTelemetry) {
    "use strict";

    function create(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;

        return {
            track: function (mEvent) {
                var oSession = UxTelemetry.begin((mEvent && mEvent.event) || "event", (mEvent && mEvent.payload) || {});
                UxTelemetry.end(oSession, "tracked", oStateModel);
            },

            snapshot: function (mArgsSnapshot) {
                var oSession = UxTelemetry.begin("snapshot", (mArgsSnapshot && mArgsSnapshot.context) || {});
                UxTelemetry.end(oSession, "snapshot", oStateModel);
                return Promise.resolve({ ok: true });
            }
        };
    }

    return {
        create: create
    };
});
