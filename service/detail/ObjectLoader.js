sap.ui.define([], function () {
    "use strict";

    function _read(gw, setName, params) { return gw.readSet(setName, params || {}); }

    var _gw;

    return {
        init: function (gatewayClient) { _gw = gatewayClient; },

        loadRoot: function (rootKey) { return _gw.readEntity("ChecklistRootSet", "Key='" + rootKey + "'", {}); },
        loadBasic: function (rootKey) { return _gw.readEntity("ChecklistBasicInfoSet", "RootKey='" + rootKey + "'", {}); },
        loadChecks: function (rootKey, top, skip) { return _read(_gw, "ChecklistCheckSet", { "$filter": "RootKey eq '" + rootKey + "'", "$top": top, "$skip": skip }); },
        loadBarriers: function (rootKey, top, skip) { return _read(_gw, "ChecklistBarrierSet", { "$filter": "RootKey eq '" + rootKey + "'", "$top": top, "$skip": skip }); },
        loadAttachments: function (rootKey) { return _read(_gw, "AttachmentSet", { "$filter": "RootKey eq '" + rootKey + "'" }); },

        openChecklist: function (rootKey) {
            return Promise.all([
                this.loadRoot(rootKey),
                this.loadBasic(rootKey),
                this.loadChecks(rootKey, 20, 0),
                this.loadBarriers(rootKey, 20, 0),
                this.loadAttachments(rootKey),
                _gw.readEntity("LastChangeSet", "RootKey='" + rootKey + "'", {})
            ]).then(function (aParts) {
                return {
                    root: aParts[0],
                    basic: aParts[1],
                    checks: aParts[2],
                    barriers: aParts[3],
                    attachments: aParts[4],
                    _aggChangedOn: (aParts[5] || {}).AggChangedOn || null
                };
            });
        }
    };
});
