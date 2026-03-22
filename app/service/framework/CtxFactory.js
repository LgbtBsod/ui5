sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxRuntimeFactory"
], function (
    CtxRuntimeFactory
) {
    "use strict";

    function buildCtx(oController, mViewRefs) {
        return CtxRuntimeFactory.build(oController, mViewRefs);
    }

    return {
        buildCtx: buildCtx
    };
});
