sap.ui.define([], function () {
    "use strict";

    function _isFilled(vValue) {
        if (vValue === null || vValue === undefined) {
            return false;
        }

        return String(vValue).trim().length > 0;
    }

    function calculateSectionSuccess(aItems) {
        if (!Array.isArray(aItems) || aItems.length === 0) {
            return {
                rate: 0,
                hasFailed: null
            };
        }

        var iSuccess = aItems.filter(function (oItem) {
            return !!((oItem || {}).result);
        }).length;

        return {
            rate: Math.round((iSuccess / aItems.length) * 100),
            hasFailed: iSuccess < aItems.length
        };
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

            var oChecksStats = calculateSectionSuccess(oCheckList.checks);
            var oBarriersStats = calculateSectionSuccess(oCheckList.barriers);
            var iBasicRate = calculateBasicCompletion(oCheckList.basic);

            oCheckList.root.successRateChecks = oChecksStats.rate;
            oCheckList.root.successRateBarriers = oBarriersStats.rate;
            oCheckList.root.hasFailedChecks = oChecksStats.hasFailed;
            oCheckList.root.hasFailedBarriers = oBarriersStats.hasFailed;
            oCheckList.root.successRateOverall = Math.round((oChecksStats.rate + oBarriersStats.rate + iBasicRate) / 3);
            oCheckList.root.status = deriveStatus(oChecksStats.rate, oBarriersStats.rate, iBasicRate);

            return oCheckList;
        }

    };
});
