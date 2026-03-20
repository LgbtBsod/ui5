sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerState",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (FeedbackBannerState, FeedbackConstants, UiSemanticConstants) {
    "use strict";

    QUnit.module("FeedbackBannerState");

    QUnit.test("normalizes severity and maps it to canonical UI semantic types", function (assert) {
        assert.strictEqual(FeedbackBannerState.normalizeSeverity("warning"), FeedbackConstants.SEVERITY.WARNING, "known severity remains canonical");
        assert.strictEqual(FeedbackBannerState.normalizeSeverity("unexpected"), FeedbackConstants.SEVERITY.INFO, "unknown severity falls back to info");
        assert.strictEqual(FeedbackBannerState.toUi5MessageType(FeedbackConstants.SEVERITY.ERROR), UiSemanticConstants.MESSAGE_TYPE.ERROR, "error severity maps to canonical UI semantic");
    });
});
