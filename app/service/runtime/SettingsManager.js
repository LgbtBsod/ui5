sap.ui.define([
"PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/FrontendConfigConstants"
], function (DebugLogger, FrontendConfigConstants) {
    "use strict";

    var SETTINGS_CACHE_TTL_MS = Number(FrontendConfigConstants && FrontendConfigConstants.SETTINGS_CACHE_TTL_MS) || (5 * 60 * 1000);
    var _loaded = false;
    var _loadedAt = 0;
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

    function _notify(oRuntime, mMeta) {
        _subscribers.forEach(function (fn) {
            try { fn(oRuntime || {}, mMeta || {}); } catch (e) { /* noop */ }
        });
    }

    function _isTtlExpired() {
        if (!_loaded || !_loadedAt) {
            return true;
        }
        return (Date.now() - _loadedAt) >= SETTINGS_CACHE_TTL_MS;
    }

    function _resolveLoadReason(bForce) {
        if (!_loaded) {
            return "initial";
        }
        if (bForce) {
            return "force";
        }
        return "ttl_expired";
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

        load: function (gatewayClient, mOptions) {
            var bForce = !!(mOptions && mOptions.force);
            var bExpired = _isTtlExpired();

            if (_loaded && !bForce && !bExpired) {
                return Promise.resolve(_runtimeCache);
            }
            if (_loadingPromise) {
                return _loadingPromise;
            }

            var sReason = _resolveLoadReason(bForce);
            var iStartedAt = Date.now();
            _loadingPromise = gatewayClient.readEntity("RuntimeSettingsSet", "Key='GLOBAL'", {}).then(function (oData) {
                _runtimeCache = oData || {};
                _loaded = true;
                _loadedAt = Date.now();
                _notify(_runtimeCache, {
                    loadedAt: _loadedAt,
                    reason: sReason,
                    refreshed: sReason !== "initial"
                });
                _logSummary({
                    source: FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL,
                    ok: true,
                    durationMs: Date.now() - iStartedAt,
                    applied: true,
                    error: "",
                    loadedAtIso: new Date(_loadedAt).toISOString(),
                    cacheTtlMs: SETTINGS_CACHE_TTL_MS,
                    reason: sReason
                });
                _loadingPromise = null;
                return _runtimeCache;
            }).catch(function (oError) {
                _loadingPromise = null;
                _logSummary({
                    source: FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL,
                    ok: false,
                    durationMs: Date.now() - iStartedAt,
                    applied: false,
                    error: _safeError(oError),
                    loadedAtIso: _loadedAt ? new Date(_loadedAt).toISOString() : "",
                    cacheTtlMs: SETTINGS_CACHE_TTL_MS,
                    reason: sReason
                });
                throw oError;
            });

            return _loadingPromise;
        },

        reload: function (gatewayClient) {
            return this.load(gatewayClient, { force: true });
        },

        getLoadedAt: function () {
            return _loadedAt;
        },

        reset: function () {
            _loaded = false;
            _loadedAt = 0;
            _loadingPromise = null;
            _runtimeCache = {};
            _summaryLogged = false;
        }
    };
});
