sap.ui.define([
    "sap_ui5/service/detail/ObjectLoader",
    "sap_ui5/service/cache/CacheService"
], function (ObjectLoader, CacheService) {
    "use strict";

    var _ctx = null;

    function _runtime() {
        return (_ctx.models.masterData && _ctx.models.masterData.getProperty("/runtime")) || {};
    }

    return {
        init: function (view, models, gatewayClient, managers) {
            _ctx = { view: view, models: models || {}, gatewayClient: gatewayClient, managers: managers || {} };
            ObjectLoader.init(gatewayClient);
        },

        openChecklist: function (rootKey) {
            return CacheService.checkCacheOrReload(rootKey, _ctx.gatewayClient, this, _runtime()).then(function (snapshot) {
                if (_ctx.models.view) {
                    CacheService.applySnapshotToViewModel(_ctx.models.view, snapshot);
                }
                return snapshot;
            });
        },

        enterEdit: function () {
            if (_ctx.managers.lockStatus && _ctx.managers.lockStatus.start) {
                _ctx.managers.lockStatus.start();
            }
            if (_ctx.managers.heartbeat && _ctx.managers.heartbeat.start) {
                _ctx.managers.heartbeat.start();
            }
            return Promise.resolve(true);
        },

        exitEdit: function () {
            if (_ctx.managers.lockStatus && _ctx.managers.lockStatus.stop) {
                _ctx.managers.lockStatus.stop();
            }
            if (_ctx.managers.heartbeat && _ctx.managers.heartbeat.stop) {
                _ctx.managers.heartbeat.stop();
            }
            return Promise.resolve(true);
        },

        setStatus: function (newStatus, rootKey) {
            return _ctx.gatewayClient.callFunctionImport("SetChecklistStatus", { RootKey: rootKey, NewStatus: newStatus });
        },

        refreshChildrenPaging: function (type, top, skip, rootKey) {
            if (type === "checks") {
                return ObjectLoader.loadChecks(rootKey, top, skip);
            }
            return ObjectLoader.loadBarriers(rootKey, top, skip);
        }
    };
});
