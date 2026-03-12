sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentFormattingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentDetailStateRuntime"
], function (ComponentSessionRuntime, ComponentFormattingRuntime, ComponentDetailStateRuntime) {
    "use strict";

    return {
        resolveBootDetailId: ComponentDetailStateRuntime.resolveBootDetailId,
        isCreateBootHash: ComponentDetailStateRuntime.isCreateBootHash,
        ensureSessionId: ComponentSessionRuntime.ensureSessionId,
        ensureTabSessionId: ComponentSessionRuntime.ensureTabSessionId,
        formatHumanDateTime: ComponentFormattingRuntime.formatHumanDateTime,
        eventPayload: ComponentFormattingRuntime.eventPayload,
        applyLockProbeState: ComponentDetailStateRuntime.applyLockProbeState,
        syncUiStateMode: ComponentDetailStateRuntime.syncUiStateMode,
        syncDetailCurrentFromSelected: ComponentDetailStateRuntime.syncDetailCurrentFromSelected,
        resolveDetailCurrent: ComponentDetailStateRuntime.resolveDetailCurrent
    };
});
