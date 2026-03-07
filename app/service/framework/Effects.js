sap.ui.define([
    "sap_ui5/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    function toast(sTextKey, sLevel) {
        return {
            type: "toast",
            textKey: sTextKey,
            level: sLevel || "info"
        };
    }

    function busy(sScope, bValue) {
        return {
            type: "busy",
            scope: sScope,
            value: !!bValue
        };
    }

    function modelPatch(sModelName, sPath, vValue) {
        return {
            type: "modelPatch",
            modelName: sModelName,
            path: sPath,
            value: vValue
        };
    }

    function modelMerge(sModelName, sPath, oPartialObject) {
        return {
            type: "modelMerge",
            modelName: sModelName,
            path: sPath,
            partialObject: RuntimeInput.asObject(oPartialObject)
        };
    }

    function navigate(sRoute, oParams, bReplace) {
        return {
            type: "navigate",
            route: sRoute,
            params: RuntimeInput.asObject(oParams),
            replace: !!bReplace
        };
    }

    function banner(sId, bVisible, oPayload) {
        return {
            type: "banner",
            id: sId,
            visible: !!bVisible,
            payload: RuntimeInput.asObject(oPayload)
        };
    }

    function dialog(sId, sAction, oPayload) {
        return {
            type: "dialog",
            id: sId,
            action: sAction,
            payload: RuntimeInput.asObject(oPayload)
        };
    }

    function confirm(sId, sTextKey, oPayload) {
        return {
            type: "confirm",
            id: sId,
            textKey: sTextKey,
            payload: RuntimeInput.asObject(oPayload)
        };
    }

    function notify(sTextKey, sVariant) {
        return {
            type: "dialog",
            variant: sVariant || "information",
            textKey: sTextKey
        };
    }

    function warn(sTextKey) {
        return notify(sTextKey, "warning");
    }

    function log(sLevel, sMessage, oMeta) {
        return {
            type: "log",
            level: sLevel || "info",
            message: sMessage,
            meta: oMeta
        };
    }

    return {
        toast: toast,
        busy: busy,
        modelPatch: modelPatch,
        modelMerge: modelMerge,
        navigate: navigate,
        banner: banner,
        dialog: dialog,
        confirm: confirm,
        notify: notify,
        warn: warn,
        log: log
    };
});
