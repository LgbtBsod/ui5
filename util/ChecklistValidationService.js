sap.ui.define([], function () {
    "use strict";

    var DEFAULT_REQUIRED_FIELDS = [
        "/basic/date",
        "/basic/time",
        "/basic/timezone",
        "/basic/OBSERVER_FULLNAME",
        "/basic/OBSERVED_FULLNAME",
        "/basic/LOCATION_KEY",
        "/basic/LPC_KEY",
        "/basic/PROF_KEY"
    ];

    function _readPath(oData, sPath) {
        return String(sPath || "")
            .split("/")
            .filter(Boolean)
            .reduce(function (v, sSeg) {
                if (!v || typeof v !== "object") {
                    return undefined;
                }
                return v[sSeg];
            }, oData);
    }

    function _isFilled(v) {
        if (Array.isArray(v)) {
            return v.length > 0;
        }
        if (typeof v === "boolean") {
            return true;
        }
        return String(v || "").trim().length > 0;
    }

    return {
        validateForStatusChange: function (oChecklist, mRules) {
            var aRequired = (mRules && mRules.requiredFields) || DEFAULT_REQUIRED_FIELDS;
            var aMissing = aRequired.filter(function (sPath) {
                return !_isFilled(_readPath(oChecklist || {}, sPath));
            });

            var aChecks = (oChecklist && oChecklist.checks) || [];
            var bHasAtLeastOneCheck = aChecks.length > 0;

            return {
                valid: aMissing.length === 0 && bHasAtLeastOneCheck,
                missingPaths: aMissing,
                hasAtLeastOneCheck: bHasAtLeastOneCheck
            };
        },

        isBarrierSectionAllowed: function (sLpcKey) {
            var sKey = String(sLpcKey || "").toLowerCase();
            return sKey !== "lpc000" && sKey !== "lpc001";
        },

        buildOverallResult: function (oChecklist) {
            var aChecks = (oChecklist && oChecklist.checks) || [];
            var aBarriers = (oChecklist && oChecklist.barriers) || [];
            var aAll = aChecks.concat(aBarriers);
            if (!aAll.length) {
                return null;
            }
            return aAll.every(function (oRow) { return !!(oRow && oRow.result); });
        }
    };
});
