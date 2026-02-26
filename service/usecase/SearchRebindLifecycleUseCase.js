sap.ui.define([], function () {
    "use strict";

    function runRebindLifecycle(mArgs) {
        var oBindingParams = mArgs && mArgs.bindingParams;
        if (!oBindingParams || typeof oBindingParams !== "object") {
            return { ok: false, reason: "missing_binding_params" };
        }

        var fnPrepareRebind = mArgs && mArgs.prepareRebind;
        if (typeof fnPrepareRebind !== "function") {
            return { ok: false, reason: "missing_rebind_adapter" };
        }

        var fnOnDataReceived = mArgs && mArgs.onDataReceived;

        fnPrepareRebind({
            bindingParams: oBindingParams,
            state: (mArgs && mArgs.state) || {},
            smartFilterData: (mArgs && mArgs.smartFilterData) || {},
            onDataReceived: function (oDataEvent) {
                if (typeof fnOnDataReceived === "function") {
                    fnOnDataReceived(oDataEvent);
                }
            }
        });

        return { ok: true, reason: "rebind_prepared" };
    }

    function runDataReceivedLifecycle(mArgs) {
        var oDataEvent = mArgs && mArgs.dataEvent;
        if (!oDataEvent || typeof oDataEvent.getParameter !== "function") {
            return { ok: false, reason: "malformed_data_received" };
        }

        var fnSyncLifecycle = mArgs && mArgs.syncLifecycle;
        if (typeof fnSyncLifecycle !== "function") {
            return { ok: false, reason: "missing_sync_adapter" };
        }

        fnSyncLifecycle(oDataEvent.getParameter("data"));
        return { ok: true, reason: "data_received_synced" };
    }

    return {
        runRebindLifecycle: runRebindLifecycle,
        runDataReceivedLifecycle: runDataReceivedLifecycle
    };
});
