sap.ui.define([], function () {
    "use strict";

    function formatHumanDateTime(vDate) {
        var oDate = vDate instanceof Date ? vDate : new Date(vDate || Date.now());
        if (Number.isNaN(oDate.getTime())) {
            oDate = new Date();
        }
        return oDate.toLocaleString(undefined, {
            year: "numeric",
            month: "short",
            day: "2-digit",
            hour: "2-digit",
            minute: "2-digit"
        });
    }

    function eventPayload(oEvent) {
        return (oEvent && typeof oEvent.getParameters === "function" && oEvent.getParameters()) || {};
    }

    return {
        formatHumanDateTime: formatHumanDateTime,
        eventPayload: eventPayload
    };
});
