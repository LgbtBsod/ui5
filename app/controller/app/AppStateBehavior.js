sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime"
], function (ControllerTextRuntime, ShellStateRuntime) {
    "use strict";

    var getText = ControllerTextRuntime.getText;

    return {
        _ensureAppViewDefaults: function () {
            ShellStateRuntime.ensureAppViewDefaults(this);
        },
        _syncShellState: function () {
            ShellStateRuntime.syncShellState(this, {
                getText: getText
            });
        }
    };
});
