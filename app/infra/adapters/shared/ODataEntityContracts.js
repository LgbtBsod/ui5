sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayContractConstants) {
    "use strict";

    var TYPES = Object.freeze({
        ATTACHMENT_KEY: "Edm.String",
        CURRENT_ALIAS_KEY: "Edm.String",
        DB_KEY: "Edm.Binary",
        PARENT_KEY: "Edm.Binary",
        ROOT_KEY: "Edm.Binary"
    });

    var DETAIL_ENTITY_FILTERS = Object.freeze({
        CHECKLIST_BASIC_INFO: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_BASIC_INFO,
            property: "RootKey",
            type: TYPES.ROOT_KEY
        }),
        CHECKLIST_CHECK: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_CHECK,
            property: "RootId"
        }),
        CHECKLIST_BARRIER: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_BARRIER,
            property: "RootId"
        }),
        ATTACHMENT: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.ATTACHMENT,
            property: "RootKey",
            type: TYPES.ROOT_KEY
        })
    });

    function byEntitySet(sEntitySet) {
        var aKeys = Object.keys(DETAIL_ENTITY_FILTERS);
        var i;
        var oEntry;
        for (i = 0; i < aKeys.length; i += 1) {
            oEntry = DETAIL_ENTITY_FILTERS[aKeys[i]];
            if (oEntry.entitySet === sEntitySet) {
                return oEntry;
            }
        }
        return null;
    }

    return {
        TYPES: TYPES,
        DETAIL_ENTITY_FILTERS: DETAIL_ENTITY_FILTERS,
        byEntitySet: byEntitySet
    };
});
