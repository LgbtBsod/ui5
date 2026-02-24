sap.ui.define([], function () {
    "use strict";

    function _isFilled(vValue) {
        if (vValue === null || vValue === undefined) {
            return false;
        }

        return String(vValue).trim().length > 0;
    }

    function calculateSectionCompletion(aItems) {
        if (!Array.isArray(aItems) || aItems.length === 0) {
            return 0;
        }

        var iCompleted = aItems.filter(function (oItem) {
            return _isFilled((oItem || {}).text);
        }).length;

        return Math.round((iCompleted / aItems.length) * 100);
    }

    function calculateBasicCompletion(oBasic) {
        var aFields = [
            oBasic && oBasic.date,
            oBasic && oBasic.equipment,
            oBasic && oBasic.LPC_TEXT,
            oBasic && oBasic.timezone
        ];
        var iFilled = aFields.filter(_isFilled).length;

        return Math.round((iFilled / aFields.length) * 100);
    }

    function deriveStatus(iChecksRate, iBarriersRate, iBasicRate) {
        var iMinRate = Math.min(iChecksRate, iBarriersRate, iBasicRate);

        if (iMinRate < 50) {
            return "CRITICAL";
        }

        if (iMinRate < 85) {
            return "WARNING";
        }

        return "SUCCESS";
    }

    return {

        recalculate: function (oCheckList) {
            if (!oCheckList) {
                return oCheckList;
            }

            oCheckList.root = oCheckList.root || {};
            oCheckList.basic = oCheckList.basic || {};

            var iChecksRate = calculateSectionCompletion(oCheckList.checks);
            var iBarriersRate = calculateSectionCompletion(oCheckList.barriers);
            var iBasicRate = calculateBasicCompletion(oCheckList.basic);

            oCheckList.root.successRateChecks = iChecksRate;
            oCheckList.root.successRateBarriers = iBarriersRate;
            oCheckList.root.successRateOverall = Math.round((iChecksRate + iBarriersRate + iBasicRate) / 3);
            oCheckList.root.status = deriveStatus(iChecksRate, iBarriersRate, iBasicRate);

            return oCheckList;
        }

    };
});
