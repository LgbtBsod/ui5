sap.ui.define([
    "sap_ui5/service/backend/FakeBackendService",
    "sap_ui5/service/backend/RealBackendService"
], function (FakeBackendService, RealBackendService) {
    "use strict";

    var _backendService = FakeBackendService;

    function _readUrlMode() {
        try {
            var oUrl = new URL(window.location.href);
            return oUrl.searchParams.get("backend");
        } catch (e) {
            return null;
        }
    }

    function _selectBackend(mConfig) {
        var sMode = (mConfig && mConfig.mode) || _readUrlMode() || "fake";
        _backendService = (sMode === "real") ? RealBackendService : FakeBackendService;

        if (_backendService.configure) {
            _backendService.configure(mConfig || {});
        }
    }

    return {

        configure: function (mConfig) {
            _selectBackend(mConfig || {});
        },

        login: function (username) {
            return _backendService.login(username);
        },

        init: function () {
            return _backendService.init();
        },

        getCheckLists: function () {
            return _backendService.getCheckLists();
        },

        queryCheckLists: function (mQuery) {
            return _backendService.queryCheckLists(mQuery);
        },

        createCheckList: function (oData) {
            return _backendService.createCheckList(oData);
        },

        updateCheckList: function (sId, oData) {
            return _backendService.updateCheckList(sId, oData);
        },

        deleteCheckList: function (sId) {
            return _backendService.deleteCheckList(sId);
        },

        upsertRows: function (sId, sSection, aRows) {
            return _backendService.upsertRows(sId, sSection, aRows);
        },

        lockHeartbeat: function (sSessionId) {
            return _backendService.lockHeartbeat(sSessionId);
        },

        lockRelease: function (sSessionId) {
            return _backendService.lockRelease(sSessionId);
        },

        getServerState: function () {
            return _backendService.getServerState();
        },

        getPersons: function () {
            if (_backendService.getPersons) {
                return _backendService.getPersons();
            }
            return Promise.resolve([]);
        },

        getDictionary: function (sDomain) {
            if (_backendService.getDictionary) {
                return _backendService.getDictionary(sDomain);
            }
            return Promise.resolve([]);
        },

        getLocations: function () {
            if (_backendService.getLocations) {
                return _backendService.getLocations();
            }
            return Promise.resolve([]);
        },

        createObject: function (data) {
            return _backendService.create(data);
        },

        readObject: function (uuid) {
            return _backendService.read(uuid);
        },

        updateObject: function (uuid, data) {
            return _backendService.update(uuid, data);
        },

        getAllObjects: function () {
            return _backendService.getAll();
        }

    };
});
