sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (RuntimeInput, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

    function ActionDispatcher(mHandlers) {
        this._handlers = Object.assign({}, mHandlers || {});
    }

    ActionDispatcher.prototype.register = function (sAction, fnHandler) {
        var sName = RuntimeInput.asString(sAction).trim();
        if (sName && typeof fnHandler === TYPE_FUNCTION) {
            this._handlers[sName] = fnHandler;
        }
    };

    ActionDispatcher.prototype.unregister = function (sAction) {
        var sName = RuntimeInput.asString(sAction).trim();
        if (sName && this._handlers[sName]) {
            delete this._handlers[sName];
        }
    };

    ActionDispatcher.prototype.dispatch = function (sAction, mPayload) {
        var sName = RuntimeInput.asString(sAction).trim();
        var fn = this._handlers && this._handlers[sName];
        var oPayload = RuntimeInput.asObject(mPayload);
        if (typeof fn !== TYPE_FUNCTION) {
            return Promise.resolve(false);
        }
        return Promise.resolve(fn(oPayload)).then(function () { return true; });
    };

    return ActionDispatcher;
});
