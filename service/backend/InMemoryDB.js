sap.ui.define([], function () {
    "use strict";

    var STORAGE_KEY = "sap_ui5_fake_backend_db";

    var _db = {
        checkLists: [],
        objects: {}
    };

    function _clone(vData) {
        return JSON.parse(JSON.stringify(vData));
    }

    function _persist() {
        if (typeof window === "undefined" || !window.localStorage) {
            return;
        }

        window.localStorage.setItem(STORAGE_KEY, JSON.stringify(_db));
    }

    function _tryRestoreFromStorage() {
        if (typeof window === "undefined" || !window.localStorage) {
            return false;
        }

        var sRaw = window.localStorage.getItem(STORAGE_KEY);
        if (!sRaw) {
            return false;
        }

        try {
            var oParsed = JSON.parse(sRaw);
            _db.checkLists = Array.isArray(oParsed.checkLists) ? oParsed.checkLists : [];
            _db.objects = oParsed.objects || {};
            return true;
        } catch (e) {
            return false;
        }
    }

    function _generateUUID() {
        return "uuid-" + Date.now() + "-" + Math.floor(Math.random() * 10000);
    }

    return {

        init: function (aCheckLists) {
            if (_tryRestoreFromStorage()) {
                return;
            }

            _db.checkLists = Array.isArray(aCheckLists) ? _clone(aCheckLists) : [];
            _persist();
        },

        getCheckLists: function () {
            return _clone(_db.checkLists);
        },

        createCheckList: function (oData) {
            _db.checkLists.push(_clone(oData));
            _persist();
            return _clone(oData);
        },

        updateCheckList: function (sId, oData) {
            var iIdx = _db.checkLists.findIndex(function (oItem) {
                return oItem && oItem.root && oItem.root.id === sId;
            });

            if (iIdx < 0) {
                return null;
            }

            _db.checkLists[iIdx] = _clone(oData);
            _persist();
            return _clone(_db.checkLists[iIdx]);
        },

        createObject: function (data) {
            var uuid = _generateUUID();

            _db.objects[uuid] = {
                uuid: uuid,
                version_number: 1,
                changed_on: new Date().toISOString(),
                server_changed_on: new Date().toISOString(),
                locked_by: null,
                lock_expires: null,
                data: _clone(data)
            };

            _persist();
            return _clone(_db.objects[uuid]);
        },

        readObject: function (uuid) {
            return _db.objects[uuid] ? _clone(_db.objects[uuid]) : null;
        },

        updateObject: function (uuid, newData) {
            var obj = _db.objects[uuid];
            if (!obj) {
                return null;
            }

            obj.data = _clone(newData);
            obj.version_number += 1;
            obj.changed_on = new Date().toISOString();
            obj.server_changed_on = new Date().toISOString();

            _persist();
            return _clone(obj);
        },

        getAll: function () {
            return Object.values(_db.objects).map(_clone);
        }

    };
});
