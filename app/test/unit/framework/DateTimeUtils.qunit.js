sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DateTimeUtils"
], function (DateTimeUtils) {
    "use strict";

    QUnit.module("framework/DateTimeUtils");

    QUnit.test("normalizes OData, ISO and UI dates through UI5 DateFormat", function (assert) {
        assert.strictEqual(DateTimeUtils.formatYmdUtc("/Date(1704067200000)/"), "2024-01-01", "OData date is normalized to canonical UTC yyyy-MM-dd");
        assert.strictEqual(DateTimeUtils.formatHumanDate(new Date(2024, 0, 1, 10, 30, 0)), "01.01.2024", "Date object is formatted with the shared UI5 date formatter");
        assert.strictEqual(DateTimeUtils.formatHumanTime(new Date(1970, 0, 1, 8, 5, 0)), "08:05", "Date object time is formatted with the shared UI5 time formatter");
        assert.strictEqual(DateTimeUtils.formatHumanDate("not-a-date"), "", "invalid dates are not echoed back into the UI");
    });
});
