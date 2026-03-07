sap.ui.define([
    "checklist/app/controller/support/DetailFormatTextSupport",
    "checklist/app/controller/support/DetailFormatValueSupport"
], function (DetailFormatTextSupport, DetailFormatValueSupport) {
    "use strict";

    return Object.assign({}, DetailFormatTextSupport, DetailFormatValueSupport);
});
