sap.ui.define([
    "sap_ui5/domain/ChecklistEngine"
], function (ChecklistEngine) {
    "use strict";

    var STORAGE_KEY = "sap_ui5_fake_backend_db";

    var _db = {
        checkLists: [],
        objects: {},
        locks: {},
        meta: {
            lastChangeSet: null,
            serverChangedOn: null
        }
    };

    function _clone(vData) {
        return JSON.parse(JSON.stringify(vData));
    }

    function _touchMeta() {
        var sNow = new Date().toISOString();
        _db.meta.lastChangeSet = "cs-" + Date.now();
        _db.meta.serverChangedOn = sNow;
    }

    function _persist() {
        if (typeof window === "undefined" || !window.localStorage) {
            return;
        }

        window.localStorage.setItem(STORAGE_KEY, JSON.stringify(_db));
    }

    function _normalizeChecklist(oChecklist) {
        if (!oChecklist) {
            return oChecklist;
        }

        oChecklist.checks = Array.isArray(oChecklist.checks) ? oChecklist.checks : [];
        oChecklist.barriers = Array.isArray(oChecklist.barriers) ? oChecklist.barriers : [];
        oChecklist.root = oChecklist.root || {};
        oChecklist.basic = oChecklist.basic || {};
        if (typeof oChecklist.root.this_is_integration_data === "undefined") {
            oChecklist.root.this_is_integration_data = !!oChecklist.root.integrationFlag;
        }

        return ChecklistEngine.recalculate(oChecklist);
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
            _db.checkLists = Array.isArray(oParsed.checkLists)
                ? oParsed.checkLists.map(function (oChecklist) { return _normalizeChecklist(oChecklist); })
                : [];
            _db.objects = oParsed.objects || {};
            _db.locks = oParsed.locks || {};
            _db.meta = oParsed.meta || _db.meta;
            return true;
        } catch (e) {
            window.localStorage.removeItem(STORAGE_KEY);
            return false;
        }
    }

    function _generateUUID() {
        return "uuid-" + Date.now() + "-" + Math.floor(Math.random() * 10000);
    }

    function _generateChecklistId() {
        return "CHK-" + Date.now();
    }

    return {

        init: function (aCheckLists) {
            if (_tryRestoreFromStorage()) {
                _persist();
                return;
            }

            _db.checkLists = Array.isArray(aCheckLists)
                ? _clone(aCheckLists).map(function (oChecklist) { return _normalizeChecklist(oChecklist); })
                : [];
            _touchMeta();
            _persist();
        },

        getCheckLists: function () {
            return _clone(_db.checkLists);
        },

        queryCheckLists: function (mQuery) {
            var aData = _db.checkLists;
            var sId = String((mQuery && mQuery.idContains) || "").toLowerCase().trim();
            var sLpc = (mQuery && mQuery.lpcKey) || "";

            var aFiltered = aData.filter(function (oItem) {
                var sItemId = String((((oItem || {}).root || {}).id) || "").toLowerCase();
                var sItemLpc = (((oItem || {}).basic || {}).LPC_KEY || "");
                var bId = !sId || sItemId.indexOf(sId) >= 0;
                var bLpc = !sLpc || sLpc === sItemLpc;
                return bId && bLpc;
            });

            var iMax = Number((mQuery && mQuery.maxResults) || 0);
            if (iMax > 0) {
                aFiltered = aFiltered.slice(0, iMax);
            }

            return _clone(aFiltered);
        },

        createCheckList: function (oData) {
            var oNew = _normalizeChecklist(_clone(oData || {}));
            var sId = (((oNew || {}).root || {}).id || "").trim();

            if (!oNew.root) {
                oNew.root = {};
            }

            if (!sId) {
                sId = _generateChecklistId();
                oNew.root.id = sId;
            }

            var bExists = _db.checkLists.some(function (oItem) {
                return oItem && oItem.root && oItem.root.id === sId;
            });

            if (bExists) {
                throw new Error("Checklist with id '" + sId + "' already exists");
            }

            _db.checkLists.push(oNew);
            _touchMeta();
            _persist();
            return _clone(oNew);
        },

        updateCheckList: function (sId, oData) {
            var iIdx = _db.checkLists.findIndex(function (oItem) {
                return oItem && oItem.root && oItem.root.id === sId;
            });

            if (iIdx < 0) {
                return null;
            }

            _db.checkLists[iIdx] = _normalizeChecklist(_clone(oData));
            _touchMeta();
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

            _touchMeta();
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

            _touchMeta();
            _persist();
            return _clone(obj);
        },

        deleteCheckList: function (sId) {
            var iIdx = _db.checkLists.findIndex(function (oItem) {
                return oItem && oItem.root && oItem.root.id === sId;
            });

            if (iIdx < 0) {
                return false;
            }

            _db.checkLists.splice(iIdx, 1);
            _touchMeta();
            _persist();
            return true;
        },

        upsertRows: function (sId, sSection, aRows) {
            var iIdx = _db.checkLists.findIndex(function (oItem) {
                return oItem && oItem.root && oItem.root.id === sId;
            });

            if (iIdx < 0) {
                return null;
            }

            _db.checkLists[iIdx][sSection] = _clone(aRows || []);
            _db.checkLists[iIdx] = _normalizeChecklist(_db.checkLists[iIdx]);
            _touchMeta();
            _persist();
            return _clone(_db.checkLists[iIdx]);
        },

        lockHeartbeat: function (sSessionId) {
            var sId = sSessionId || "anonymous";
            var sNow = new Date().toISOString();
            _db.locks[sId] = {
                sessionId: sId,
                updatedAt: sNow,
                isKilled: false
            };

            _persist();
            return {
                sessionId: sId,
                is_killed: false,
                server_changed_on: _db.meta.serverChangedOn,
                last_change_set: _db.meta.lastChangeSet
            };
        },

        lockRelease: function (sSessionId) {
            var sId = sSessionId || "anonymous";
            delete _db.locks[sId];
            _persist();
            return { released: true, sessionId: sId };
        },

        getServerState: function () {
            return _clone(_db.meta);
        },

        getAll: function () {
            return Object.values(_db.objects).map(_clone);
        }

    };
});
