sap.ui.define([], function () {
    "use strict";

    var STORE_KEY = "__pcctUxTelemetry";

    function _getStore() {
        if (typeof window === "undefined") {
            return { events: [] };
        }
        window[STORE_KEY] = window[STORE_KEY] || { events: [] };
        return window[STORE_KEY];
    }

    function begin(operation, tags) {
        return {
            operation: operation || "unknown",
            tags: tags || {},
            startedAt: Date.now()
        };
    }

    function end(session, outcome, stateModel) {
        var evt = {
            operation: session && session.operation || "unknown",
            tags: session && session.tags || {},
            outcome: outcome || "success",
            startedAt: session && session.startedAt || Date.now(),
            endedAt: Date.now()
        };
        evt.durationMs = Math.max(0, evt.endedAt - evt.startedAt);

        var store = _getStore();
        store.events.push(evt);
        if (store.events.length > 250) {
            store.events = store.events.slice(store.events.length - 250);
        }

        if (stateModel && typeof stateModel.setProperty === "function") {
            stateModel.setProperty("/uxTelemetry/events", store.events);
            stateModel.setProperty("/uxTelemetry/lastEvent", evt);
        }

        return evt;
    }

    return {
        begin: begin,
        end: end
    };
});
