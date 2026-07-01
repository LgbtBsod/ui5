sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DateTimeUtils"
], function (DateTimeUtils) {
    "use strict";

    function formatHumanDateTime(vDate) {
        return DateTimeUtils.formatHumanDateTime(vDate, new Date());
    }

    function eventPayload(oEvent) {
        return (oEvent && typeof oEvent.getParameters === "function" && oEvent.getParameters()) || {};
    }

    return {
        formatHumanDateTime: formatHumanDateTime,
        eventPayload: eventPayload
    };
});
