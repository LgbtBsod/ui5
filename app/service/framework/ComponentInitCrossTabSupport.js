sap.ui.define([
    "checklist/app/service/framework/FeedbackBannerRuntime"
], function (FeedbackBannerRuntime) {
    "use strict";

    var STORAGE_KEY = "pcct_lock_signal";
    var CHANNEL_NAME = "pcct_lock_channel";

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

    function attach(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oStatePaths = mOptions.statePaths || {};
        var fnBundleText = mOptions.bundleText;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnHandleForceReadOnly = mOptions.handleForceReadOnly;
        var sThisTabId = buildTabId();
        var fnPublishTabSignal;
        var fnHandleTabSignal;

        fnPublishTabSignal = function (sType, mPayload) {
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
                // no-op: storage signal is best-effort.
            }
        };

        fnHandleTabSignal = function (oSignal) {
            var oPayload = oSignal || {};
            var sSignalType = String(oPayload.type || "").toUpperCase();
            var sSignalRootId = String(oPayload.rootId || "").trim();
            var sCurrentRootId = String(oStateModel.getProperty("/activeObjectId") || "").trim();
            var sMode = String(oStateModel.getProperty(oStatePaths.WORKFLOW_EDIT_MODE) || "").toUpperCase();
            var sLockState = String(oStateModel.getProperty(oStatePaths.WORKFLOW_LOCK_STATUS) || "").toUpperCase();
            if (!sSignalType || oPayload.tabId === sThisTabId || !sSignalRootId || !sCurrentRootId || sSignalRootId !== sCurrentRootId) {
                return;
            }
            if (sSignalType !== "LOCK_OWNED" || sMode !== "EDIT" || sLockState !== "LOCKED") {
                return;
            }
            oStateModel.setProperty(oStatePaths.TAB_CONFLICT_STATE, {
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
        };

        if (typeof window !== "undefined" && typeof window.BroadcastChannel === "function") {
            oComponent._oCrossTabChannel = new window.BroadcastChannel(CHANNEL_NAME);
            oComponent._oCrossTabChannel.onmessage = function (oEvent) {
                fnHandleTabSignal((oEvent && oEvent.data) || {});
            };
        }

        oComponent._fnCrossTabStorage = function (oStorageEvent) {
            if (!oStorageEvent || oStorageEvent.key !== STORAGE_KEY || !oStorageEvent.newValue) {
                return;
            }
            try {
                fnHandleTabSignal(JSON.parse(oStorageEvent.newValue));
            } catch (_e) {
                // no-op
            }
        };
        window.addEventListener("storage", oComponent._fnCrossTabStorage);

        return {
            publishTabSignal: fnPublishTabSignal,
            tabId: sThisTabId
        };
    }

    return {
        attach: attach
    };
});
