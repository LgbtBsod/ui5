sap.ui.define([], function () {
    "use strict";

    var _db = {
        objects: {}
    };

    function _generateUUID() {
        return "uuid-" + Date.now() + "-" + Math.floor(Math.random() * 10000);
    }

    return {

        createObject: function (data) {
            var uuid = _generateUUID();

            _db.objects[uuid] = {
                uuid: uuid,
                version_number: 1,
                changed_on: new Date().toISOString(),
                server_changed_on: new Date().toISOString(),
                locked_by: null,
                lock_expires: null,
                data: data
            };

            return _db.objects[uuid];
        },

        readObject: function (uuid) {
            return _db.objects[uuid] || null;
        },

        updateObject: function (uuid, newData) {
            var obj = _db.objects[uuid];
            if (!obj) {
                return null;
            }

            obj.data = newData;
            obj.version_number += 1;
            obj.changed_on = new Date().toISOString();
            obj.server_changed_on = new Date().toISOString();

            return obj;
        },

        getAll: function () {
            return Object.values(_db.objects);
        }

    };
});