sap.ui.define([
    "checklist/app/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    function ActionDispatcher(mHandlers, mValidators) {
        this._handlers = Object.assign({}, mHandlers || {});
        this._validators = Object.assign({}, mValidators || {});
    }

    ActionDispatcher.prototype.register = function (sAction, fnHandler) {
        var sName = RuntimeInput.asString(sAction).trim();
        if (sName && typeof fnHandler === "function") {
            this._handlers[sName] = fnHandler;
        }
    };

    ActionDispatcher.prototype.unregister = function (sAction) {
        var sName = RuntimeInput.asString(sAction).trim();
        if (sName && this._handlers[sName]) {
            delete this._handlers[sName];
        }
    };

    ActionDispatcher.prototype.setValidators = function (mValidators) {
        this._validators = Object.assign({}, RuntimeInput.asObject(mValidators));
    };

    ActionDispatcher.prototype.dispatch = function (sAction, mPayload) {
        var sName = RuntimeInput.asString(sAction).trim();
        var fn = this._handlers && this._handlers[sName];
        var fnNormalize = this._validators && this._validators[sName];
        var oPayload = RuntimeInput.asObject(mPayload);
        if (typeof fn !== "function") {
            return Promise.resolve(false);
        }
        if (typeof fnNormalize === "function") {
            oPayload = RuntimeInput.asObject(fnNormalize(oPayload));
        }
        return Promise.resolve(fn(oPayload)).then(function () { return true; });
    };

    return ActionDispatcher;
});
