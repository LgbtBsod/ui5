sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/BindingContextReader",
    "sap/m/ProgressIndicator",
    "sap/m/Text",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (BindingContextReader, ProgressIndicator, Text, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function readColumnProperty(oColumn) {
        var vData, oData;
        if (!oColumn || typeof oColumn.data !== TYPE_FUNCTION) { return ""; }
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
        if (oChecksColumn && typeof oChecksColumn.setVisible === TYPE_FUNCTION) {
            bChecksChanged = typeof oChecksColumn.getVisible === TYPE_FUNCTION ? oChecksColumn.getVisible() !== bChecksVisible : true;
            if (bChecksChanged) { oChecksColumn.setVisible(bChecksVisible); }
        }
        if (oBarriersColumn && typeof oBarriersColumn.setVisible === TYPE_FUNCTION) {
            bBarriersChanged = typeof oBarriersColumn.getVisible === TYPE_FUNCTION ? oBarriersColumn.getVisible() !== bBarriersVisible : true;
            if (bBarriersChanged) { oBarriersColumn.setVisible(bBarriersVisible); }
        }
    }

    function applyResponsiveColumnSizing(oTable) {
        var aColumns = (oTable && oTable.getColumns && oTable.getColumns()) || [];
        aColumns.forEach(function (oColumn) {
            var sProperty = readColumnProperty(oColumn);
            if (!oColumn || typeof oColumn.setWidth !== TYPE_FUNCTION) {
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
        var oIndicator;
        if (!oItem || typeof oItem.removeCell !== TYPE_FUNCTION || typeof oItem.insertCell !== TYPE_FUNCTION || !oCell) { return; }
        if (!bVisible) {
            if (typeof oCell.data === TYPE_FUNCTION && oCell.data("rateIndicator") === false) {
                return;
            }
            oIndicator = new Text({ text: "" });
            oIndicator.data("rateIndicator", false);
            oItem.removeCell(oCell);
            if (typeof oCell[METHODS.DESTROY] === TYPE_FUNCTION) { oCell[METHODS.DESTROY](); }
            oItem.insertCell(oIndicator, iIndex);
            return;
        }
        if (typeof oCell.data === TYPE_FUNCTION && oCell.data("rateIndicator") === true) {
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
        if (typeof oCell[METHODS.DESTROY] === TYPE_FUNCTION) { oCell[METHODS.DESTROY](); }
        oItem.insertCell(oIndicator, iIndex);
    }

    function applyToRows(oController) {
        var oTable = oController._rateProgressTable;
        var mIndexes, aItems, bChecksTotalKnown, bBarriersTotalKnown, bHasChecks, bHasBarriers;
        if (!oTable || typeof oTable.getItems !== TYPE_FUNCTION) { return; }
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
            if (oItem && typeof oItem.setType === TYPE_FUNCTION && typeof oItem.getType === TYPE_FUNCTION && oItem.getType() !== "Active") { oItem.setType("Active"); }
            if (!oCtx || !aCells.length) { return; }
        nChecks = toRatePercent(BindingContextReader.readAny(oCtx, ["success_checks_rate", "SuccessChecksRate"], 0));
        nBarriers = toRatePercent(BindingContextReader.readAny(oCtx, ["success_barriers_rate", "barriers_rate", "SuccessBarriersRate"], 0));
        vChecksTotal = BindingContextReader.readAny(oCtx, ["checks_total", "ChecksTotal"], null);
        vBarriersTotal = BindingContextReader.readAny(oCtx, ["barriers_total", "BarriersTotal"], null);
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
        if (typeof oTable.attachUpdateFinished === TYPE_FUNCTION) {
            oTable.attachUpdateFinished(function () { applyToRows(oController); });
        }
    }

    return { wireTable: wireTable, applyToRows: applyToRows };
});
