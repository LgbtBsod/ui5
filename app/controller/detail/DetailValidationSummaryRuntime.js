sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailValidationStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailValidationFocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailValidationReactiveRuntime"
], function (DetailValidationStateRuntime, DetailValidationFocusRuntime, DetailValidationReactiveRuntime) {
    "use strict";

    return {
        compute: DetailValidationStateRuntime.compute,
        recompute: DetailValidationStateRuntime.recompute,
        focusFirstInvalidField: DetailValidationFocusRuntime.focusFirstInvalidField,
        onSelectedChecklistChanged: DetailValidationReactiveRuntime.onSelectedChecklistChanged
    };
});
