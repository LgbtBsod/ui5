sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCoreInitRuntime"
], function (ComponentCoreInitRuntime) {
    "use strict";

    /*
     * Compatibility markers for invariant tests:
     * return ComponentBootRuntime.runBootSequence({
     * cacheAdapter: this._ctx && this._ctx.cache,
     * return mDeps.LoadCurrentUserUseCase && mDeps.LoadCurrentUserUseCase.refresh
     */
    return ComponentCoreInitRuntime;
});
