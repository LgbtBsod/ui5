sap.ui.define([], function () {
    "use strict";

    function extractChecklistId(oObject) {
        if (!oObject) {
            return "";
        }
        return String(
            oObject.Key ||
            oObject.key ||
            oObject.Uuid ||
            oObject.id ||
            oObject.ID ||
            oObject.Id ||
            oObject.RequestId ||
            oObject.ChecklistId ||
            oObject.checklist_id ||
            oObject.CHECKLIST_ID ||
            (((oObject.root || {}).id) || "")
        ).trim();
    }

    function extractChecklistDisplayId(oObject) {
        return String(
            (oObject && (oObject.Id || oObject.id || oObject.ChecklistId || oObject.checklist_id || oObject.Key || oObject.key)) || ""
        ).trim();
    }

    function normalizeChecklistIds(aIds) {
        var mSeen = {};
        return (aIds || []).reduce(function (aAcc, sId) {
            var sNorm = String(sId || "").trim();
            if (!sNorm || mSeen[sNorm]) {
                return aAcc;
            }
            mSeen[sNorm] = true;
            aAcc.push(sNorm);
            return aAcc;
        }, []);
    }

    return {
        extractChecklistDisplayId: extractChecklistDisplayId,
        extractChecklistId: extractChecklistId,
        normalizeChecklistIds: normalizeChecklistIds
    };
});
