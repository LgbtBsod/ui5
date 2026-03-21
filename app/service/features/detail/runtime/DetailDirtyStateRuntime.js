sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailMessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailPersistenceConstants"
], function (ModelPathContracts, ModelStateRuntime, ModelContracts, DetailMessageKeyConstants, DetailPersistenceConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_MESSAGE_KEYS = DetailMessageKeyConstants;
    var PERSISTENCE_STATES = DetailPersistenceConstants.STATES;

    function markDirty(oController) {
        ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.IS_DIRTY, true);
        ModelStateRuntime.write(oController, STATE_MODEL, "/persistence/state", PERSISTENCE_STATES.DIRTY);
        ModelStateRuntime.write(oController, STATE_MODEL, "/persistence/messageKey", DETAIL_MESSAGE_KEYS.PERSISTENCE_DIRTY);
    }

    return {
        markDirty: markDirty
    };
});
