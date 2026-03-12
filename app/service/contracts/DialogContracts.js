sap.ui.define([], function () {
    "use strict";

    var IDS = Object.freeze({
        LOCATION_VALUE_HELP: "locationValueHelp",
        CHECKS_EXPANDED: "checksExpanded",
        BARRIERS_EXPANDED: "barriersExpanded",
        ANALYTICS_REPORT: "analyticsReport",
        ANALYTICS_YEAR_PICKER: "analyticsYearPicker"
    });

    var FRAGMENTS = Object.freeze({
        locationValueHelp: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.LocationValueHelpDialog",
        checksExpanded: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.ChecksExpandedDialog",
        barriersExpanded: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.BarriersExpandedDialog",
        analyticsReport: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.AnalyticsReportDialog",
        analyticsYearPicker: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.AnalyticsYearPickerPopover"
    });

    function getFragmentName(sDialogId) {
        return FRAGMENTS[String(sDialogId || "").trim()] || "";
    }

    return Object.freeze({
        IDS: IDS,
        FRAGMENTS: FRAGMENTS,
        getFragmentName: getFragmentName
    });
});
