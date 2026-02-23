sap.ui.define([], function () {
    "use strict";

    function calculateRate(items) {
        if (!items || items.length === 0) return 0;

        const success = items.filter(i => i.result === true).length;
        return Math.round((success / items.length) * 100);
    }

    function deriveStatus(checkRate, barrierRate) {
        if (barrierRate < 100) return "CRITICAL";
        if (checkRate < 100) return "WARNING";
        return "SUCCESS";
    }

    return {

        recalculate: function (checkList) {
            const checkRate = calculateRate(checkList.checks);
            const barrierRate = calculateRate(checkList.barriers);

            checkList.root.successRateChecks = checkRate;
            checkList.root.successRateBarriers = barrierRate;
            checkList.root.status = deriveStatus(checkRate, barrierRate);

            return checkList;
        }

    };
});