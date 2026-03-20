sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/RequestResiliencePolicy",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RequestVerbConstants"
], function (RequestResiliencePolicy, RequestVerbConstants) {
    "use strict";

    QUnit.module("framework/RequestResiliencePolicy");

    QUnit.test("treats canonical GET verbs as safe reads", function (assert) {
        assert.strictEqual(RequestResiliencePolicy.isSafeRead(RequestVerbConstants.REQUEST.GET), true, "GET is a safe read");
        assert.strictEqual(RequestResiliencePolicy.isSafeRead(RequestVerbConstants.REQUEST.GET_FUNCTION), true, "GET function import is a safe read");
        assert.strictEqual(RequestResiliencePolicy.isSafeRead(RequestVerbConstants.REQUEST.POST_FUNCTION), false, "POST function import is not a safe read");
        assert.strictEqual(RequestResiliencePolicy.resolveRetryCount(RequestVerbConstants.REQUEST.GET), 1, "GET keeps safe-read retry policy");
        assert.strictEqual(RequestResiliencePolicy.resolveRetryCount(RequestVerbConstants.REQUEST.POST_FUNCTION), 0, "mutating requests do not retry by default");
    });
});
