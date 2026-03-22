sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (StatePaths) {
    "use strict";

    return Object.freeze(Object.assign({}, StatePaths, {
        AUTOSAVE_ENABLED: StatePaths.WORKFLOW_AUTOSAVE_ENABLED,
        IS_DIRTY: StatePaths.WORKFLOW_DIRTY
    }));
});
