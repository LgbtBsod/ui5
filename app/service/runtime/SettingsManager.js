sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/DebugLogger"
], function (DebugLogger) {
    "use strict";

    var _loaded = false;
    var _loadingPromise = null;
    var _runtimeCache = {};
    var _subscribers = [];
    var _summaryLogged = false;

    function _safeError(oError) {
        if (!oError) {
            return "unknown";
        }
        if (typeof oError === "string") {
            return oError;
        }
        return String(oError.message || oError.code || "runtime_settings_load_failed");
    }

    function _logSummary(mData) {
        if (!DebugLogger.isEnabled() || _summaryLogged) {
            return;
        }
        _summaryLogged = true;
        DebugLogger.info("SETTINGS_LOAD_SUMMARY", "[SETTINGS_LOAD_SUMMARY]", mData);
    }

    function _notify(oRuntime) {
        _subscribers.forEach(function (fn) {
            try { fn(oRuntime || {}); } catch (e) { /* noop */ }
        });
    }

    return {
        subscribe: function (fnHandler) {
            if (typeof fnHandler === "function" && _subscribers.indexOf(fnHandler) === -1) {
                _subscribers.push(fnHandler);
            }

            return function unsubscribe() {
                var iIndex = _subscribers.indexOf(fnHandler);
                if (iIndex > -1) {
                    _subscribers.splice(iIndex, 1);
                }
            };
        },

        load: function (gatewayClient) {
            if (_loaded) {
                return Promise.resolve(_runtimeCache);
            }
            if (_loadingPromise) {
                return _loadingPromise;
            }

            var iStartedAt = Date.now();
            _loadingPromise = gatewayClient.readEntity("RuntimeSettingsSet", "Key='GLOBAL'", {}).then(function (oData) {
                _runtimeCache = oData || {};
                _loaded = true;
                _notify(_runtimeCache);
                _logSummary({
                    source: "RuntimeSettingsSet(GLOBAL)",
                    ok: true,
                    durationMs: Date.now() - iStartedAt,
                    applied: true,
                    error: "",
                    loadedAtIso: new Date().toISOString()
                });
                return _runtimeCache;
            }).catch(function (oError) {
                _loadingPromise = null;
                _logSummary({
                    source: "RuntimeSettingsSet(GLOBAL)",
                    ok: false,
                    durationMs: Date.now() - iStartedAt,
                    applied: false,
                    error: _safeError(oError),
                    loadedAtIso: new Date().toISOString()
                });
                throw oError;
            });

            return _loadingPromise;
        }
    };
});
