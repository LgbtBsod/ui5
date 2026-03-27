sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchBindingPolicy"
], function (Result, SearchBindingPolicy) {
    "use strict";

    function ApplyRebindPolicyUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

function execute(mInput) {
        SearchBindingPolicy.applyRebindParams({
            bindingParams: (mInput && mInput.bindingParams) || {},
            state: (mInput && mInput.state) || {},
            onDataReceived: mInput && mInput.onDataReceived
        });
        return Promise.resolve(Result.ok({ source: (mInput && mInput.source) || "beforeRebind" }));
    }

    return ApplyRebindPolicyUseCase;
});