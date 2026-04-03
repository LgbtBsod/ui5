sap.ui.define([
    "sap/ui/core/format/DateFormat"
], function (DateFormat) {
    "use strict";

    var oDateTimeFormatter = DateFormat.getDateTimeInstance({
        pattern: "dd.MM.yyyy, HH:mm"
    });

    function formatHumanDateTime(vDate) {
        var oDate = vDate instanceof Date ? vDate : new Date(vDate || Date.now());
        if (Number.isNaN(oDate.getTime())) {
            oDate = new Date();
        }
        return oDateTimeFormatter.format(oDate);
    }

    function eventPayload(oEvent) {
        return (oEvent && typeof oEvent.getParameters === "function" && oEvent.getParameters()) || {};
    }

    return {
        formatHumanDateTime: formatHumanDateTime,
        eventPayload: eventPayload
    };
});
