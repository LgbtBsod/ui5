sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime"
], function (ModelPathContracts, ModelStateRuntime, ModelContracts, DetailContracts, MessageKeyConstants, DetailPersistenceRuntime) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function markDirty(oController) {
        var oPersistencePatch;
        var aEffects = DetailPersistenceRuntime.dirtyEffects(true);

        ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.IS_DIRTY, true);
        oPersistencePatch = (aEffects || []).filter(function (oEffect) {
            return oEffect && oEffect.type === "modelPatch" && oEffect.path === "/persistence";
        })[0];

        if (oPersistencePatch) {
            ModelStateRuntime.write(oController, STATE_MODEL, "/persistence", oPersistencePatch.value);
            return;
        }

        ModelStateRuntime.write(oController, STATE_MODEL, "/persistence/state", DetailContracts.STATES.DIRTY);
        ModelStateRuntime.write(oController, STATE_MODEL, "/persistence/messageKey", MessageKeyConstants.VIEW.PERSISTENCE_DIRTY);
    }

    return {
        markDirty: markDirty
    };
});
