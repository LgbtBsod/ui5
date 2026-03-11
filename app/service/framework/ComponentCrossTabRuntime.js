sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime"
], function (ModelStateRuntime, FeedbackBannerRuntime) {
    "use strict";

    function buildTabId() {
        var sTabId = "";
        try {
            sTabId = window.sessionStorage.getItem("pcct_tab_id") || "";
            if (!sTabId) {
                sTabId = "tab_" + Date.now().toString(36) + "_" + Math.random().toString(36).slice(2, 10);
                window.sessionStorage.setItem("pcct_tab_id", sTabId);
            }
        } catch (_e) {
            sTabId = "tab_volatile";
        }
        return sTabId;
    }

    function attachCrossTabRuntime(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oStatePaths = mOptions.statePaths || {};
        var fnBundleText = mOptions.bundleText;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnHandleForceReadOnly = mOptions.handleForceReadOnly;
        var sThisTabId = buildTabId();
        var STORAGE_KEY = "pcct_lock_signal";
        var CHANNEL_NAME = "pcct_lock_channel";

        function publishTabSignal(sType, mPayload) {
            var oSignal = Object.assign({}, mPayload || {}, {
                type: sType,
                tabId: sThisTabId,
                at: new Date().toISOString()
            });
            if (oComponent._oCrossTabChannel && typeof oComponent._oCrossTabChannel.postMessage === "function") {
                oComponent._oCrossTabChannel.postMessage(oSignal);
            }
            try {
                window.localStorage.setItem(STORAGE_KEY, JSON.stringify(oSignal));
            } catch (_e) {
            }
        }

        function handleTabSignal(oSignal) {
            var oPayload = oSignal || {};
            var sSignalType = String(oPayload.type || "").toUpperCase();
            var sSignalRootId = String(oPayload.rootId || "").trim();
            var sCurrentRootId = String(ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "") || "").trim();
            var sMode = String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") || "").toUpperCase();
            var sLockState = String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") || "").toUpperCase();
            if (!sSignalType || oPayload.tabId === sThisTabId || !sSignalRootId || !sCurrentRootId || sSignalRootId !== sCurrentRootId) {
                return;
            }
            if (sSignalType !== "LOCK_OWNED" || sMode !== "EDIT" || sLockState !== "EDIT_LOCKED") {
                return;
            }
            ModelStateRuntime.writeOnModel(oStateModel, oStatePaths.TAB_CONFLICT_STATE, {
                active: true,
                source: "cross_tab",
                at: new Date().toISOString()
            });
            fnSetGlobalBanner(FeedbackBannerRuntime.createBannerInput({
                severity: "warning",
                textKey: "tabConflictBanner",
                details: fnBundleText("tabConflictCopyHint")
            }));
            fnHandleForceReadOnly({
                reason: "TAB_CONFLICT",
                messageKey: "tabConflictBanner",
                source: "crossTab"
            });
        }

        if (typeof window !== "undefined" && typeof window.BroadcastChannel === "function") {
            oComponent._oCrossTabChannel = new window.BroadcastChannel(CHANNEL_NAME);
            oComponent._oCrossTabChannel.onmessage = function (oEvent) {
                handleTabSignal((oEvent && oEvent.data) || {});
            };
        }

        oComponent._fnCrossTabStorage = function (oStorageEvent) {
            if (!oStorageEvent || oStorageEvent.key !== STORAGE_KEY || !oStorageEvent.newValue) {
                return;
            }
            try {
                handleTabSignal(JSON.parse(oStorageEvent.newValue));
            } catch (_e) {
            }
        };
        window.addEventListener("storage", oComponent._fnCrossTabStorage);

        return {
            publishTabSignal: publishTabSignal,
            tabId: sThisTabId
        };
    }

    return {
        attachCrossTabRuntime: attachCrossTabRuntime
    };
});
