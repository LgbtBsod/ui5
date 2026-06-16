sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/RandomId"
], function (StatePaths, ModelStateRuntime, RandomId) {
    "use strict";

    function ensureId(oStateModel, sPath, sStorageKey, sPrefix) {
        var sCurrent = ModelStateRuntime.readOnModel(oStateModel, sPath, "");
        var sStored = window.sessionStorage.getItem(sStorageKey) || "";
        if (sCurrent) {
            return sCurrent;
        }
        if (sStored) {
            ModelStateRuntime.writeOnModel(oStateModel, sPath, sStored);
            return sStored;
        }
        var sNext = sPrefix + RandomId.randomFragment() + Date.now().toString(36);
        ModelStateRuntime.writeOnModel(oStateModel, sPath, sNext);
        window.sessionStorage.setItem(sStorageKey, sNext);
        return sNext;
    }

    function ensureSessionId(oStateModel) {
        return ensureId(oStateModel, StatePaths.SESSION_ID, "pcct_session_id", "S");
    }

    function ensureTabSessionId(oStateModel) {
        return ensureId(oStateModel, StatePaths.TAB_SESSION_ID, "pcct_tab_session_id", "T");
    }

    return {
        ensureSessionId: ensureSessionId,
        ensureTabSessionId: ensureTabSessionId
    };
});
