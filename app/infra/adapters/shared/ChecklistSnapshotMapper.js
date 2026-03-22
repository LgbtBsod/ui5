sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/NullishPick",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts"
], function (NullishPick, ODataEntityContracts) {
    "use strict";

    var firstDefined = NullishPick.firstDefined;
    var IDENTITY = ODataEntityContracts.IDENTITY;

    function mapCheckRow(oItem, iIndex) {
        var o = oItem || {};
        var nNum = Number(firstDefined(o.ChecksNum, o.checksNum, o.position, iIndex + 1)) || (iIndex + 1);
        var sText = String(firstDefined(o.Text, o.text, "")).trim();
        var sComment = String(firstDefined(o.Comment, o.comment, "")).trim();
        var bResult = Object.prototype.hasOwnProperty.call(o, "Result") ? !!o.Result : !!o.result;
        var sKey = String(firstDefined(o.Key, o.key, o.check_uuid, "")).trim();
        return Object.assign({}, o, {
            id: sKey,
            Key: sKey,
            check_uuid: sKey,
            ChecksNum: nNum,
            checksNum: nNum,
            text: sText,
            Text: sText,
            comment: sComment,
            Comment: sComment,
            result: bResult,
            Result: bResult,
            selected: !!o.selected
        });
    }

    function mapBarrierRow(oItem, iIndex) {
        var o = oItem || {};
        var nNum = Number(firstDefined(o.BarriersNum, o.barriersNum, o.position, iIndex + 1)) || (iIndex + 1);
        var sText = String(firstDefined(o.Text, o.text, o.Description, o.description, "")).trim();
        var sComment = String(firstDefined(o.Comment, o.comment, "")).trim();
        var bResult = Object.prototype.hasOwnProperty.call(o, "Result") ? !!o.Result : !!o.result;
        var sKey = String(firstDefined(o.Key, o.key, o.barrier_uuid, "")).trim();
        return Object.assign({}, o, {
            id: sKey,
            Key: sKey,
            barrier_uuid: sKey,
            BarriersNum: nNum,
            barriersNum: nNum,
            text: sText,
            Text: sText,
            description: sText,
            Description: sText,
            comment: sComment,
            Comment: sComment,
            result: bResult,
            Result: bResult,
            selected: !!o.selected
        });
    }

    function mapAttachmentRow(oItem) {
        var o = oItem || {};
        var sKey = String(firstDefined.apply(null, IDENTITY.ATTACHMENT_KEY_FIELDS.map(function (sField) {
            return o[sField];
        }).concat([""]))).trim();
        return Object.assign({}, o, {
            Key: sKey,
            AttachmentKey: sKey,
            RootKey: String(firstDefined(o.RootKey, o.rootKey, "")).trim(),
            ParentKey: String(firstDefined(o.ParentKey, o.parentKey, o.RootKey, o.rootKey, "")).trim(),
            FolderKey: String(firstDefined(o.FolderKey, o.folderKey, "")).trim(),
            fileName: String(firstDefined(o.FileName, o.fileName, o.Name, o.name, "")).trim(),
            FileName: String(firstDefined(o.FileName, o.fileName, o.Name, o.name, "")).trim(),
            mimeType: String(firstDefined(o.MimeType, o.mimeType, "application/octet-stream")).trim(),
            MimeType: String(firstDefined(o.MimeType, o.mimeType, "application/octet-stream")).trim(),
            description: String(firstDefined(o.Description, o.description, "")).trim(),
            Description: String(firstDefined(o.Description, o.description, "")).trim(),
            fileSize: Number(firstDefined(o.FileSize, o.fileSize, 0)) || 0,
            FileSize: Number(firstDefined(o.FileSize, o.fileSize, 0)) || 0,
            fileSizeContent: Number(firstDefined(o.FileSizeContent, o.fileSizeContent, o.FileSize, o.fileSize, 0)) || 0,
            FileSizeContent: Number(firstDefined(o.FileSizeContent, o.fileSizeContent, o.FileSize, o.fileSize, 0)) || 0,
            changedOn: String(firstDefined(o.ChangedOn, o.changedOn, "")).trim(),
            ChangedOn: String(firstDefined(o.ChangedOn, o.changedOn, "")).trim(),
            createdOn: String(firstDefined(o.CreatedOn, o.createdOn, "")).trim(),
            CreatedOn: String(firstDefined(o.CreatedOn, o.createdOn, "")).trim(),
            categoryKey: String(firstDefined(o.CategoryKey, o.categoryKey, "GEN")).trim(),
            CategoryKey: String(firstDefined(o.CategoryKey, o.categoryKey, "GEN")).trim(),
            categoryText: String(firstDefined(o.CategoryText, o.categoryText, "")).trim(),
            CategoryText: String(firstDefined(o.CategoryText, o.categoryText, "")).trim(),
            Value: String(firstDefined(o.Value, o.value, "")).trim()
        });
    }

    return {
        mapCheckRow: mapCheckRow,
        mapBarrierRow: mapBarrierRow,
        mapAttachmentRow: mapAttachmentRow
    };
});
