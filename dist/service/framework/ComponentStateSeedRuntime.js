sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (ModelStateRuntime) {
    "use strict";

    function seedInitialState(oStateModel, StatePaths, TimeConfigService) {
        var mTimerDefaults = TimeConfigService.buildDefaultTimerMap();
        var mInitState = { "/timers": mTimerDefaults };
        mInitState[StatePaths.SAVE_IN_FLIGHT] = false;
        mInitState[StatePaths.PENDING_NAVIGATION_INTENT] = null;
        mInitState[StatePaths.TAB_CONFLICT_STATE] = { active: false, source: "", at: "" };
        mInitState["/networkOnline"] = true;
        mInitState["/networkGraceMode"] = false;
        mInitState["/networkGraceExpiresAt"] = null;
        ModelStateRuntime.setManyOnModel(oStateModel, mInitState);
        return mTimerDefaults;
    }

    return {
        seedInitialState: seedInitialState
    };
});
