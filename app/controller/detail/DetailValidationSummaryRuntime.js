sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailValidationStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailValidationFocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailValidationReactiveRuntime"
], function (DetailValidationStateRuntime, DetailValidationFocusRuntime, DetailValidationReactiveRuntime) {
    "use strict";

    return {
        compute: DetailValidationStateRuntime.compute,
        recompute: DetailValidationStateRuntime.recompute,
        focusFirstInvalidField: DetailValidationFocusRuntime.focusFirstInvalidField,
        onSelectedChecklistChanged: DetailValidationReactiveRuntime.onSelectedChecklistChanged
    };
});
