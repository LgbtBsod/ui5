sap.ui.define([], function () {
    "use strict";

    function mapCheckRow(oItem, iIndex) {
        var o = oItem || {};
        var nNum = Number(o.ChecksNum || o.checksNum || o.position || (iIndex + 1)) || (iIndex + 1);
        var sText = String(o.Text || o.text || "").trim();
        var sComment = String(o.Comment || o.comment || "").trim();
        var bResult = Object.prototype.hasOwnProperty.call(o, "Result") ? !!o.Result : !!o.result;
        var sKey = String(o.Key || o.key || o.check_uuid || "").trim();
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
        var nNum = Number(o.BarriersNum || o.barriersNum || o.position || (iIndex + 1)) || (iIndex + 1);
        var sText = String(o.Text || o.text || o.Description || o.description || "").trim();
        var sComment = String(o.Comment || o.comment || "").trim();
        var bResult = Object.prototype.hasOwnProperty.call(o, "Result") ? !!o.Result : !!o.result;
        var sKey = String(o.Key || o.key || o.barrier_uuid || "").trim();
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
        var sKey = String(o.AttachmentKey || o.Key || o.key || "").trim();
        return Object.assign({}, o, {
            Key: sKey,
            AttachmentKey: sKey,
            RootKey: String(o.RootKey || o.rootKey || "").trim(),
            ParentKey: String(o.ParentKey || o.parentKey || o.RootKey || o.rootKey || "").trim(),
            FolderKey: String(o.FolderKey || o.folderKey || "").trim(),
            fileName: String(o.FileName || o.fileName || "").trim(),
            FileName: String(o.FileName || o.fileName || "").trim(),
            mimeType: String(o.MimeType || o.mimeType || "application/octet-stream").trim(),
            MimeType: String(o.MimeType || o.mimeType || "application/octet-stream").trim(),
            description: String(o.Description || o.description || "").trim(),
            Description: String(o.Description || o.description || "").trim(),
            fileSize: Number(o.FileSize || o.fileSize || 0) || 0,
            FileSize: Number(o.FileSize || o.fileSize || 0) || 0,
            fileSizeContent: Number(o.FileSizeContent || o.fileSizeContent || o.FileSize || o.fileSize || 0) || 0,
            FileSizeContent: Number(o.FileSizeContent || o.fileSizeContent || o.FileSize || o.fileSize || 0) || 0,
            changedOn: String(o.ChangedOn || o.changedOn || "").trim(),
            ChangedOn: String(o.ChangedOn || o.changedOn || "").trim(),
            createdOn: String(o.CreatedOn || o.createdOn || "").trim(),
            CreatedOn: String(o.CreatedOn || o.createdOn || "").trim(),
            categoryKey: String(o.CategoryKey || o.categoryKey || "GEN").trim(),
            CategoryKey: String(o.CategoryKey || o.categoryKey || "GEN").trim(),
            categoryText: String(o.CategoryText || o.categoryText || "").trim(),
            CategoryText: String(o.CategoryText || o.categoryText || "").trim(),
            Value: String(o.Value || o.value || "").trim()
        });
    }

    return {
        mapCheckRow: mapCheckRow,
        mapBarrierRow: mapBarrierRow,
        mapAttachmentRow: mapAttachmentRow
    };
});
