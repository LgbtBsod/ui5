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

    var IDENTITY = Object.freeze({
        ROOT_FILTER_BINARY: "RootKey",
        ROOT_FILTER_ALIAS: "RootId",
        ROOT_CANONICAL_FIELDS: Object.freeze(["RootKey", "Key", "id"]),
        ROOT_ALIAS_FIELDS: Object.freeze(["rootKey", "key", "pcct_uuid", "Id"]),
        ATTACHMENT_KEY_FIELDS: Object.freeze(["AttachmentKey", "Key", "key", "attach_uuid"]),
        PARENT_KEY_FIELDS: Object.freeze(["ParentKey", "parentKey", "RootKey", "rootKey"]),
        ROOT_KEY_FIELDS: Object.freeze(["RootKey", "rootKey"]),
        FILE_NAME_FIELDS: Object.freeze(["FileName", "fileName", "Name", "name"]),
        MIME_TYPE_FIELDS: Object.freeze(["MimeType", "mimeType"]),
        FILE_SIZE_FIELDS: Object.freeze(["FileSize", "fileSize", "FileSizeContent", "fileSizeContent"]),
        INLINE_VALUE_FIELDS: Object.freeze(["Value", "value"])
    });

    var DETAIL_ENTITY_FILTERS = Object.freeze({
        CHECKLIST_BASIC_INFO: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_BASIC_INFO,
            property: IDENTITY.ROOT_FILTER_BINARY,
            type: TYPES.ROOT_KEY
        }),
        CHECKLIST_CHECK: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_CHECK,
            property: IDENTITY.ROOT_FILTER_ALIAS
        }),
        CHECKLIST_BARRIER: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.CHECKLIST_BARRIER,
            property: IDENTITY.ROOT_FILTER_ALIAS
        }),
        ATTACHMENT: Object.freeze({
            entitySet: GatewayContractConstants.ENTITY_SETS.ATTACHMENT,
            property: IDENTITY.ROOT_FILTER_BINARY,
            type: TYPES.ROOT_KEY
        })
    });

    var SELECTS = Object.freeze({
        ATTACHMENT: "AttachmentKey,Key,RootKey,FolderKey,CategoryKey,CategoryText,Type,FileName,Name,MimeType,Description,FileSize,FileSizeContent,DownloadUrl,DocumentHandle,Value,ScanStatus,ScannedOn,CreatedOn,ChangedOn",
        ATTACHMENT_CONTENT: "AttachmentKey,FileName,MimeType,DownloadUrl,DocumentHandle,Value",
        CHECKLIST_BASIC_INFO: "RootKey,LocationKey,LocationName,LocationText,Bukrs,ObserverPernr,ObserverFullname,ObservedPernr,ObservedFullname,Lpc,Profession,DateCheck,TimeCheck,TimeZone,EquipName",
        CHECKLIST_BARRIER: "Key,RootKey,BarriersNum,Text,Comment,Result,ChangedOn",
        CHECKLIST_CHECK: "Key,RootKey,ChecksNum,Text,Comment,Result,ChangedOn",
        CHECKLIST_PERMISSION: "RootKey,CanCreate,CanView,CanEdit,CanDelete,ReasonCode,Message"
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
