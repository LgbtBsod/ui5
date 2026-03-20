sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ModelPathContracts, ModelStateRuntime, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function markDirty(oController) {
        ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.IS_DIRTY, true);
        ModelStateRuntime.write(oController, STATE_MODEL, "/persistence/state", "dirty");
        ModelStateRuntime.write(oController, STATE_MODEL, "/persistence/messageKey", "persistenceDirty");
    }

    return {
        markDirty: markDirty
    };
});
