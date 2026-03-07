sap.ui.define([
    "checklist/app/controller/support/BindingContextReadSupport"
], function (BindingContextReadSupport) {
    "use strict";

    function getProgressIndicatorCtor() { return sap.ui.requireSync("sap/m/ProgressIndicator"); }
    function getTextCtor() { return sap.ui.requireSync("sap/m/Text"); }

    function readColumnProperty(oColumn) {
        var vData, oData;
        if (!oColumn || typeof oColumn.data !== "function") { return ""; }
        vData = oColumn.data("p13nData");
        oData = vData;
        if (typeof vData === "string") {
            try { oData = JSON.parse(vData); } catch (e) { oData = null; }
        }
        return String(oData && (oData.leadingProperty || oData.columnKey) || "").toLowerCase();
    }

    function resolveRateColumnIndexes(oTable) {
        var aColumns = (oTable && oTable.getColumns && oTable.getColumns()) || [];
        var mIndexes = { checksIndex: -1, barriersIndex: -1 };
        aColumns.forEach(function (oColumn, iIndex) {
            var sProperty = readColumnProperty(oColumn);
            if (sProperty === "success_checks_rate" || sProperty === "successchecksrate") { mIndexes.checksIndex = iIndex; }
            if (sProperty === "success_barriers_rate" || sProperty === "successbarriersrate" || sProperty === "barriers_rate") { mIndexes.barriersIndex = iIndex; }
        });
        return mIndexes;
    }

    function toRatePercent(vRate) {
        var n = parseFloat(String(vRate == null ? "0" : vRate).replace("%", "").replace(",", "."));
        if (!isFinite(n)) { return 0; }
        return Math.max(0, Math.min(100, n));
    }

    function applyRateColumnVisibility(oTable, mIndexes, bHasChecks, bHasBarriers) {
        var aColumns, oChecksColumn, oBarriersColumn, bChecksVisible, bBarriersVisible, bChecksChanged, bBarriersChanged;
        if (!oTable || !mIndexes) { return; }
        aColumns = (oTable.getColumns && oTable.getColumns()) || [];
        oChecksColumn = mIndexes.checksIndex >= 0 ? aColumns[mIndexes.checksIndex] : null;
        oBarriersColumn = mIndexes.barriersIndex >= 0 ? aColumns[mIndexes.barriersIndex] : null;
        bChecksVisible = !!bHasChecks;
        bBarriersVisible = !!bHasBarriers;
        if (oChecksColumn && oChecksColumn.setVisible) {
            bChecksChanged = typeof oChecksColumn.getVisible === "function" ? oChecksColumn.getVisible() !== bChecksVisible : true;
            if (bChecksChanged) { oChecksColumn.setVisible(bChecksVisible); }
        }
        if (oBarriersColumn && oBarriersColumn.setVisible) {
            bBarriersChanged = typeof oBarriersColumn.getVisible === "function" ? oBarriersColumn.getVisible() !== bBarriersVisible : true;
            if (bBarriersChanged) { oBarriersColumn.setVisible(bBarriersVisible); }
        }
    }

    function applyResponsiveColumnSizing(oTable) {
        var aColumns = (oTable && oTable.getColumns && oTable.getColumns()) || [];
        aColumns.forEach(function (oColumn) {
            var sProperty = readColumnProperty(oColumn);
            if (!oColumn || typeof oColumn.setWidth !== "function") {
                return;
            }
            if (sProperty === "id") { oColumn.setWidth("9rem"); }
            if (sProperty === "lpctext") { oColumn.setWidth("8rem"); }
            if (sProperty === "professiontext") { oColumn.setWidth("10rem"); }
            if (sProperty === "status") { oColumn.setWidth("7.5rem"); }
            if (sProperty === "success_checks_rate" || sProperty === "successchecksrate") { oColumn.setWidth("7.5rem"); }
            if (sProperty === "success_barriers_rate" || sProperty === "successbarriersrate" || sProperty === "barriers_rate") { oColumn.setWidth("7.5rem"); }
            if (sProperty === "datecheck") { oColumn.setWidth("8rem"); }
        });
    }

    function ensureRateCell(oItem, iIndex, nRate, sKind, bVisible) {
        var aCells = oItem && oItem.getCells ? (oItem.getCells() || []) : [];
        var oCell = aCells[iIndex];
        var bFull = Math.round(nRate) >= 100;
        var sDisplayValue = Math.round(nRate) + "%";
        var sState = bFull ? "Success" : "Error";
        var ProgressIndicator = getProgressIndicatorCtor();
        var Text = getTextCtor();
        var oIndicator;
        if (!oItem || typeof oItem.removeCell !== "function" || typeof oItem.insertCell !== "function" || !oCell) { return; }
        if (!bVisible) {
            if (typeof oCell.data === "function" && oCell.data("rateIndicator") === false) {
                return;
            }
            oIndicator = new Text({ text: "" });
            oIndicator.data("rateIndicator", false);
            oItem.removeCell(oCell);
            if (typeof oCell.destroy === "function") { oCell.destroy(); }
            oItem.insertCell(oIndicator, iIndex);
            return;
        }
        if (typeof oCell.data === "function" && oCell.data("rateIndicator") === true) {
            oIndicator = oCell;
            if (oIndicator.setPercentValue) { oIndicator.setPercentValue(nRate); }
            if (oIndicator.setDisplayValue) { oIndicator.setDisplayValue(sDisplayValue); }
            if (oIndicator.setState) { oIndicator.setState(sState); }
            return;
        }
        oIndicator = new ProgressIndicator({ percentValue: nRate, displayValue: sDisplayValue, state: sState, showValue: true });
        oIndicator.data("rateIndicator", true);
        oIndicator.data("rateKind", sKind || "checks");
        oItem.removeCell(oCell);
        if (typeof oCell.destroy === "function") { oCell.destroy(); }
        oItem.insertCell(oIndicator, iIndex);
    }

    function applyToRows(oController) {
        var oTable = oController._rateProgressTable;
        var mIndexes, aItems, bChecksTotalKnown, bBarriersTotalKnown, bHasChecks, bHasBarriers;
        if (!oTable || typeof oTable.getItems !== "function") { return; }
        mIndexes = resolveRateColumnIndexes(oTable);
        aItems = oTable.getItems() || [];
        bChecksTotalKnown = false;
        bBarriersTotalKnown = false;
        bHasChecks = false;
        bHasBarriers = false;
        aItems.forEach(function (oItem) {
            var oCtx = oItem.getBindingContext && oItem.getBindingContext();
            var aCells = oItem.getCells ? (oItem.getCells() || []) : [];
            var nChecks, nBarriers, vChecksTotal, vBarriersTotal, nChecksTotal, nBarriersTotal, bShowChecks, bShowBarriers;
            if (oItem && typeof oItem.setType === "function" && oItem.getType && oItem.getType() !== "Active") { oItem.setType("Active"); }
            if (!oCtx || !aCells.length) { return; }
            nChecks = toRatePercent(BindingContextReadSupport.readAny(oCtx, ["success_checks_rate", "SuccessChecksRate"], 0));
            nBarriers = toRatePercent(BindingContextReadSupport.readAny(oCtx, ["success_barriers_rate", "barriers_rate", "SuccessBarriersRate"], 0));
            vChecksTotal = BindingContextReadSupport.readAny(oCtx, ["checks_total", "ChecksTotal"], null);
            vBarriersTotal = BindingContextReadSupport.readAny(oCtx, ["barriers_total", "BarriersTotal"], null);
            nChecksTotal = vChecksTotal == null ? NaN : Number(vChecksTotal);
            nBarriersTotal = vBarriersTotal == null ? NaN : Number(vBarriersTotal);
            if (isFinite(nChecksTotal)) { bChecksTotalKnown = true; if (nChecksTotal > 0) { bHasChecks = true; } }
            if (isFinite(nBarriersTotal)) { bBarriersTotalKnown = true; if (nBarriersTotal > 0) { bHasBarriers = true; } }
            bShowChecks = !isFinite(nChecksTotal) || nChecksTotal > 0;
            bShowBarriers = !isFinite(nBarriersTotal) || nBarriersTotal > 0;
            [{ index: mIndexes.checksIndex, rate: nChecks, kind: "checks", visible: bShowChecks }, { index: mIndexes.barriersIndex, rate: nBarriers, kind: "barriers", visible: bShowBarriers }]
                .forEach(function (mCell) {
                    if (mCell.index < 0) { return; }
                    ensureRateCell(oItem, mCell.index, mCell.rate, mCell.kind, mCell.visible);
                });
        });
        applyRateColumnVisibility(oTable, mIndexes, bChecksTotalKnown ? bHasChecks : true, bBarriersTotalKnown ? bHasBarriers : true);
    }

    function wireTable(oController, oTable) {
        if (!oTable || oController._rateProgressBound) { return; }
        oController._rateProgressBound = true;
        oController._rateProgressTable = oTable;
        applyResponsiveColumnSizing(oTable);
        if (oTable.attachUpdateFinished) {
            oTable.attachUpdateFinished(function () { applyToRows(oController); });
        }
    }

    return { wireTable: wireTable, applyToRows: applyToRows };
});
