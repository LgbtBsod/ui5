sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime"
], function (ControllerTextRuntime, ShellStateRuntime) {
    "use strict";

    var getText = ControllerTextRuntime.getText;

    return {
        _ensureShellDefaults: function () {
            ShellStateRuntime.ensureShellDefaults(this);
        },
        _syncShellState: function () {
            ShellStateRuntime.syncShellState(this, {
                getText: getText
            });
        }
    };
});
