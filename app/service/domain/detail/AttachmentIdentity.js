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
        return aSafe.filter(function (oItem) {
            return keyOf(oItem) !== sId;
        });
    }

    return {
        keyOf: keyOf,
        appendUnique: appendUnique,
        removeById: removeById
    };
});
