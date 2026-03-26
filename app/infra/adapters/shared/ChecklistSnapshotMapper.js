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
        var sParentKey = String(firstDefined(o.PARENT_KEY, "")).trim();
        var sDbKey = String(firstDefined(o.DB_KEY, "")).trim();
        return Object.assign({}, o, {
            DB_KEY: sDbKey,
            AttachmentKey: sKey,
            PARENT_KEY: sParentKey,
            FolderKey: String(firstDefined(o.FolderKey, "")).trim(),
            FileName: String(firstDefined(o.FileName, "")).trim(),
            MimeType: String(firstDefined(o.MimeType, "application/octet-stream")).trim(),
            Description: String(firstDefined(o.Description, "")).trim(),
            FileSize: Number(firstDefined(o.FileSize, 0)) || 0,
            FileSizeContent: Number(firstDefined(o.FileSizeContent, o.FileSize, 0)) || 0,
            ChangedOn: String(firstDefined(o.ChangedOn, "")).trim(),
            CreatedOn: String(firstDefined(o.CreatedOn, "")).trim(),
            CategoryKey: String(firstDefined(o.CategoryKey, "GEN")).trim(),
            CategoryText: String(firstDefined(o.CategoryText, "")).trim(),
            DownloadUrl: String(firstDefined(o.DownloadUrl, "")).trim(),
            DocumentHandle: String(firstDefined(o.DocumentHandle, "")).trim()
        });
    }

    return {
        mapCheckRow: mapCheckRow,
        mapBarrierRow: mapBarrierRow,
        mapAttachmentRow: mapAttachmentRow
    };
});
