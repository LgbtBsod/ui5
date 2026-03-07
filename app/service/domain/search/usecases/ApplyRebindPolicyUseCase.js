sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/util/search/SearchBindingPolicy"
], function (UseCase, Result, SearchBindingPolicy) {
    "use strict";

    function ApplyRebindPolicyUseCase() {
        UseCase.call(this, "ApplyRebindPolicyUseCase");
    }

    ApplyRebindPolicyUseCase.prototype = Object.create(UseCase.prototype);
    ApplyRebindPolicyUseCase.prototype.constructor = ApplyRebindPolicyUseCase;

    ApplyRebindPolicyUseCase.prototype.execute = function (mInput) {
        SearchBindingPolicy.applyRebindParams({
            bindingParams: (mInput && mInput.bindingParams) || {},
            state: (mInput && mInput.state) || {},
            onDataReceived: mInput && mInput.onDataReceived
        });
        return Promise.resolve(Result.ok({ source: (mInput && mInput.source) || "beforeRebind" }));
    };

    return ApplyRebindPolicyUseCase;
});
