sap.ui.define([], function () {
    "use strict";

    var STORE_KEY = "__pcctTelemetryBuffer";
    var MAX_EVENTS = 500;

    function getStore() {
        if (typeof window === "undefined") {
            return { events: [] };
        }
        window[STORE_KEY] = window[STORE_KEY] || { events: [] };
        return window[STORE_KEY];
    }

    function list() {
        return getStore().events.slice();
    }

    function last() {
        var aEvents = getStore().events;
        return aEvents.length ? aEvents[aEvents.length - 1] : null;
    }

    function push(oEvent) {
        var oStore = getStore();
        oStore.events.push(oEvent || {});
        if (oStore.events.length > MAX_EVENTS) {
            oStore.events = oStore.events.slice(oStore.events.length - MAX_EVENTS);
        }
        return {
            events: oStore.events.slice(),
            lastEvent: oStore.events.length ? oStore.events[oStore.events.length - 1] : null
        };
    }

    function clear() {
        getStore().events = [];
        return {
            events: [],
            lastEvent: null
        };
    }

    return {
        list: list,
        last: last,
        push: push,
        clear: clear
    };
});
