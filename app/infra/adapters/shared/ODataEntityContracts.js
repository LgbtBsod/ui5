sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayContractConstants) {
    "use strict";

    /* Backend serializes DB_KEY/PARENT_KEY as bin-to-hex string32 (Edm.String), not raw Edm.Binary
     * - so filters/keys must use plain string literals, never the binary'...' OData literal. */
    var TYPES = Object.freeze({
        ATTACHMENT_KEY: "Edm.String",
        CURRENT_ALIAS_KEY: "Edm.String",
        DB_KEY: "Edm.String",
        PARENT_KEY: "Edm.String"
    });

    var IDENTITY = Object.freeze({
        ROOT_FILTER_BINARY: "DB_KEY",
        ROOT_CANONICAL_FIELDS: Object.freeze(["DB_KEY"]),
        ROOT_ALIAS_FIELDS: Object.freeze(["DB_KEY"]),
        ATTACHMENT_KEY_FIELDS: Object.freeze(["AttachmentKey"]),
        PARENT_KEY_FIELDS: Object.freeze(["PARENT_KEY"]),
        ROOT_KEY_FIELDS: Object.freeze(["DB_KEY"]),
        FILE_NAME_FIELDS: Object.freeze(["FileName"]),
        MIME_TYPE_FIELDS: Object.freeze(["MimeType"]),
        FILE_SIZE_FIELDS: Object.freeze(["FileSize", "FileSizeContent"]),
    });

    var DETAIL_ENTITY_FILTERS = Object.freeze({
        CHECKLIST_BASIC_INFO: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_BASIC_INFO,
            property: IDENTITY.ROOT_FILTER_BINARY,
            type: TYPES.DB_KEY
        }),
        CHECKLIST_CHECK: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_CHECK,
            property: "PARENT_KEY",
            type: TYPES.PARENT_KEY
        }),
        CHECKLIST_BARRIER: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_BARRIER,
            property: "PARENT_KEY",
            type: TYPES.PARENT_KEY
        }),
        ATTACHMENT: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.ATTACHMENT,
            property: "PARENT_KEY",
            type: TYPES.PARENT_KEY
        })
    });

    var SELECTS = Object.freeze({
        ATTACHMENT: "AttachmentKey,DB_KEY,PARENT_KEY,FolderKey,CategoryKey,CategoryText,Type,FileName,Name,MimeType,Description,FileSize,FileSizeContent,DownloadUrl,DocumentHandle,ScanStatus,ScannedOn,CreatedOn,ChangedOn",
        ATTACHMENT_CONTENT: "AttachmentKey,FileName,MimeType,DownloadUrl,DocumentHandle",
        CHECKLIST_BASIC_INFO: "DB_KEY,ChecklistId,LocationKey,LocationName,LocationText,Bukrs,ObserverPernr,ObserverName,ObserverFullname,ObserverPosition,ObserverOrgUnit,ObservedPernr,ObservedName,ObservedFullname,ObservedPosition,ObservedOrgUnit,Lpc,LpcText,Profession,ProfessionText,ChecksNumber,ChecksNumberText,BarriersNumber,BarriersNumberText,DateCheck,TimeCheck,TimeZone,EquipName",
        CHECKLIST_BARRIER: "DB_KEY,PARENT_KEY,BarriersNum,Text,Comment,Result,ChangedOn",
        CHECKLIST_CHECK: "DB_KEY,PARENT_KEY,ChecksNum,Text,Comment,Result,ChangedOn",
        CHECKLIST_ROOT: "DB_KEY,RequestId,Id,ChangedOn,CreatedOn,VersionNumber,Status,IntegrationFlag,HasFailedChecks,HasFailedBarriers,SuccessChecksRate,SuccessBarriersRate,ChecksTotal,BarriersTotal",
        CHECKLIST_PERMISSION: "DB_KEY,CanCreate,CanView,CanEdit,CanDelete,ReasonCode,Message"
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
        IDENTITY: IDENTITY,
        TYPES: TYPES,
        DETAIL_ENTITY_FILTERS: DETAIL_ENTITY_FILTERS,
        SELECTS: SELECTS,
        byEntitySet: byEntitySet
    };
});
