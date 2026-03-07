sap.ui.define([
    "sap_ui5/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    var ACTIONS = {
        DETAIL_RETRY_GUARDED_SAVE: "detail.retryGuardedSave",
        DETAIL_TAKEOVER_LOCK: "detail.takeoverLock",
        DETAIL_CANCEL_ENTER_EDIT: "detail.cancelEnterEdit"
    };

    var RETRY_ACTIONS = {
        SAVE: "save",
        SEARCH: "search",
        SESSION: "session"
    };

    var SHELL_USER_ACTIONS = {
        TEST_USER: "testUser",
        REFRESH_CONTEXT: "refreshContext"
    };
    var RETRY_ACTION_VALUES = [RETRY_ACTIONS.SAVE, RETRY_ACTIONS.SEARCH, RETRY_ACTIONS.SESSION];
    var SHELL_USER_ACTION_VALUES = [SHELL_USER_ACTIONS.TEST_USER, SHELL_USER_ACTIONS.REFRESH_CONTEXT];

    function normalizeRetryAction(vAction) {
        var sAction = RuntimeInput.asString(vAction).trim().toLowerCase();
        return RETRY_ACTION_VALUES.indexOf(sAction) >= 0 ? sAction : "";
    }

    function normalizeShellUserAction(vAction) {
        var sAction = RuntimeInput.asString(vAction).trim();
        return SHELL_USER_ACTION_VALUES.indexOf(sAction) >= 0 ? sAction : "";
    }

    function normalizeObjectPayload(vPayload) {
        return RuntimeInput.asObject(vPayload);
    }

    function normalizeTakeoverPayload(vPayload) {
        var oPayload = RuntimeInput.asObject(vPayload);
        return {
            rootId: RuntimeInput.asString(oPayload.rootId || "").trim(),
            force: RuntimeInput.asBoolean(oPayload.force, false)
        };
    }

    var ACTION_PAYLOAD_NORMALIZERS = {};
    ACTION_PAYLOAD_NORMALIZERS[ACTIONS.DETAIL_TAKEOVER_LOCK] = normalizeTakeoverPayload;

    function normalizeActionPayload(sAction, vPayload) {
        var fn = ACTION_PAYLOAD_NORMALIZERS[sAction] || normalizeObjectPayload;
        return fn(vPayload);
    }

    return {
        ACTIONS: ACTIONS,
        RETRY_ACTIONS: RETRY_ACTIONS,
        SHELL_USER_ACTIONS: SHELL_USER_ACTIONS,
        normalizeRetryAction: normalizeRetryAction,
        normalizeShellUserAction: normalizeShellUserAction,
        normalizeActionPayload: normalizeActionPayload
    };
});
