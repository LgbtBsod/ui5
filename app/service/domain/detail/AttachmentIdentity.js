sap.ui.define([], function () {
    "use strict";

    function keyOf(oAttachment) {
        return String((oAttachment && (oAttachment.AttachmentKey || oAttachment.Key || oAttachment.client_row_id)) || "").trim();
    }

    function appendUnique(aList, oAttachment) {
        var aSafe = Array.isArray(aList) ? aList.slice() : [];
        var sKey = keyOf(oAttachment);
        var iIndex;
        if (!sKey) {
            aSafe.push(oAttachment);
            return aSafe;
        }
        iIndex = aSafe.findIndex(function (oItem) {
            return keyOf(oItem) === sKey;
        });
        if (iIndex >= 0) {
            aSafe[iIndex] = Object.assign({}, aSafe[iIndex], oAttachment);
            return aSafe;
        }
        aSafe.push(oAttachment);
        return aSafe;
    }

    function removeById(aList, sAttachmentId) {
        var sId = String(sAttachmentId || "").trim();
        var aSafe = Array.isArray(aList) ? aList : [];
        if (!sId) {
            return aSafe.slice();
        }
        return aSafe.filter(function (oItem) {
            return keyOf(oItem) !== sId;
        });
    }

    function resolveDeletionId(sAttachmentId, oAttachment) {
        var sDirect = String(sAttachmentId || "").trim();
        if (sDirect) {
            return sDirect;
        }
        return keyOf(oAttachment);
    }

    function removeByAttachment(aList, sAttachmentId, oAttachment) {
        var sResolvedId = resolveDeletionId(sAttachmentId, oAttachment);
        var aSafe = Array.isArray(aList) ? aList : [];
        if (sResolvedId) {
            return removeById(aSafe, sResolvedId);
        }
        var sName = String((oAttachment && (oAttachment.FileName || oAttachment.filename || oAttachment.name)) || "").trim();
        var sMime = String((oAttachment && (oAttachment.MimeType || oAttachment.mimeType || oAttachment.type)) || "").trim();
        return aSafe.filter(function (oItem) {
            var bSameName = String((oItem && (oItem.FileName || oItem.filename || oItem.name)) || "").trim() === sName;
            var bSameMime = String((oItem && (oItem.MimeType || oItem.mimeType || oItem.type)) || "").trim() === sMime;
            return !(sName && bSameName && (!sMime || bSameMime));
        });
    }

    return {
        keyOf: keyOf,
        appendUnique: appendUnique,
        resolveDeletionId: resolveDeletionId,
        removeById: removeById,
        removeByAttachment: removeByAttachment
    };
});
