sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/SpreadsheetExport",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/NullishPick",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchMaxResults",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchRuntimeConstants"
], function (Result, Effects, SpreadsheetExport, ChecklistIdentity, NullishPick, SearchMaxResults, WorkflowTelemetry, SearchRuntimeConstants) {
    "use strict";

    var SEARCH_MODE = SearchRuntimeConstants.SEARCH_MODE;
    var SEARCH_SEGMENTS = SearchRuntimeConstants.SEARCH_SEGMENTS;

    function ExportSearchUseCase() {
        return {
            execute: execute
        };
    }

function pick(v, fallback) {
        return v === undefined || v === null ? fallback : v;
    }

    var pickFirstDefined = NullishPick.firstDefined;

    function pickFilterValue(vValue) {
        if (vValue == null) {
            return "";
        }
        if (typeof vValue === "string" || typeof vValue === "number" || typeof vValue === "boolean") {
            return String(vValue).trim();
        }
        if (Array.isArray(vValue)) {
            return pickFilterValue(vValue[0]);
        }
        if (typeof vValue === "object") {
            if (Object.prototype.hasOwnProperty.call(vValue, "value")) {
                return pickFilterValue(vValue.value);
            }
            if (Object.prototype.hasOwnProperty.call(vValue, "key")) {
                return pickFilterValue(vValue.key);
            }
            if (Object.prototype.hasOwnProperty.call(vValue, "text")) {
                return pickFilterValue(vValue.text);
            }
            if (Array.isArray(vValue.items) && vValue.items.length) {
                return pickFilterValue(vValue.items[0]);
            }
            if (Array.isArray(vValue.ranges) && vValue.ranges.length) {
                return pickFilterValue(vValue.ranges[0]);
            }
            if (Object.prototype.hasOwnProperty.call(vValue, "value1")) {
                return pickFilterValue(vValue.value1);
            }
            if (Object.prototype.hasOwnProperty.call(vValue, "low")) {
                return pickFilterValue(vValue.low);
            }
        }
        return "";
    }

    function pickStateOrFilter(vFilterValue, vStateValue) {
        var sFilterValue = pickFilterValue(vFilterValue);
        return sFilterValue || pickFilterValue(vStateValue);
    }

    function normalizeDateRange(vValue) {
        var oRange = vValue && typeof vValue === "object" && !Array.isArray(vValue)
            ? vValue
            : {};
        var aRanges = Array.isArray(oRange.ranges) ? oRange.ranges : [];
        var oFirstRange = aRanges[0] || oRange;
        var sFrom = pickFilterValue(pickFirstDefined(oFirstRange.value1, oFirstRange.low, vValue));
        var sTo = pickFilterValue(pickFirstDefined(oFirstRange.value2, oFirstRange.high, sFrom));
        return {
            dateFrom: sFrom,
            dateTo: sTo || sFrom
        };
    }

    function hasComplexDateRange(vValue) {
        var oRange = vValue && typeof vValue === "object" && !Array.isArray(vValue) ? vValue : null;
        var aRanges = oRange && Array.isArray(oRange.ranges) ? oRange.ranges : [];
        var sOperation = String((oRange && oRange.operation) || "").trim();
        if (aRanges.length > 1) {
            return true;
        }
        if (aRanges.length === 1) {
            sOperation = String((aRanges[0] && aRanges[0].operation) || sOperation).trim();
        }
        return !!sOperation && sOperation !== "BT";
    }

    function shouldKeepRowForEntity(oRow, sEntity) {
        if (sEntity === "check" || sEntity === "barrier") {
            return oRow.ItemType !== "ROOT";
        }
        if (sEntity === "screen") {
            return true;
        }
        return oRow.ItemType === "ROOT" || !oRow.ItemType;
    }

    function normalizeRows(aRows, sEntity) {
        return (aRows || []).map(function (oRow) {
            var o = oRow || {};
            return {
                RootKey: pick(pickFirstDefined(o.RootKey, o.rootKey, o.Key, o.key), ""),
                Id: pick(pickFirstDefined(o.Id, o.id), ""),
                Lpc: pick(pickFirstDefined(o.LpcText, o.Lpc, o.lpc), ""),
                Profession: pick(pickFirstDefined(o.ProfessionText, o.Profession, o.profession), ""),
                Location: pick(pickFirstDefined(o.LocationKey, o.location_key), ""),
                Status: pick(pickFirstDefined(o.Status, o.status), ""),
                DateCheck: pick(pickFirstDefined(o.DateCheck, o.date_check), ""),
                EquipName: pick(pickFirstDefined(o.EquipName, o.equipment), ""),
                ChangedOn: pick(pickFirstDefined(o.ChangedOn, o.changed_on), ""),
                ItemType: pick(pickFirstDefined(o.ItemType, o.itemType), ""),
                Num: pick(pickFirstDefined(o.Num, o.num), ""),
                Text: pick(pickFirstDefined(o.Text, o.text), ""),
                Comment: pick(pickFirstDefined(o.Comment, o.comment), ""),
                Result: pickFirstDefined(o.Result, o.result, "")
            };
        }).filter(function (oRow) {
            return shouldKeepRowForEntity(oRow, sEntity);
        });
    }

    function buildSearchContract(mInput, mCtx) {
        var oSmart = mCtx && mCtx.smartControls;
        var oStateModel = mCtx && mCtx.stateModel;
        var mFilterData = (mInput && mInput.filterData) || (
            oSmart && typeof oSmart.getSmartFilterData === "function"
                ? oSmart.getSmartFilterData()
                : {}
        ) || {};
        var mState = (oStateModel && oStateModel.getData && oStateModel.getData()) || {};
        var mDateRange = normalizeDateRange(mFilterData.DateCheck);
        return {
            filterId: pickStateOrFilter(mFilterData.Id, mState.filterId),
            filterDateFrom: mDateRange.dateFrom,
            filterDateTo: mDateRange.dateTo,
            filterLocationKey: pickStateOrFilter(mFilterData.LocationKey, mState.filterLocationKey),
            filterLpc: pickStateOrFilter(mFilterData.Lpc, mState.filterLpc),
            filterProfession: pickFilterValue(mFilterData.ProfessionText),
            filterStatus: pickFilterValue(mFilterData.Status),
            searchMode: String(mState.searchMode || SEARCH_MODE.EXACT).toUpperCase(),
            checksSegment: String(((mState.search || {}).checksFailSegment) || SEARCH_SEGMENTS.ALL).toUpperCase(),
            barriersSegment: String(((mState.search || {}).barriersFailSegment) || SEARCH_SEGMENTS.ALL).toUpperCase()
        };
    }

    function resolveBoundRowsForCurrentFilters(mInput, mCtx) {
        var oSmart = mCtx && mCtx.smartControls;
        var oStateModel = mCtx && mCtx.stateModel;
        var mState = (oStateModel && oStateModel.getData && oStateModel.getData()) || {};
        var iExportLimit = SearchMaxResults.resolveExportLimit(mState);
        if (!oSmart || typeof oSmart.getBoundRows !== "function") {
            return Promise.reject(new Error("EXPORT_FILTER_STATE_UNSUPPORTED"));
        }
        return Promise.resolve(oSmart.getBoundRows(iExportLimit)).then(function (aRows) {
            return Array.isArray(aRows) ? aRows : [];
        });
    }

    function resolveExportRows(mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var oStateModel = mCtx && mCtx.stateModel;
        var oSmart = mCtx && mCtx.smartControls;
        var aSelectedIds = ChecklistIdentity.normalizeChecklistIds(mInput && mInput.selectedRowIds);
        var mState = (oStateModel && oStateModel.getData && oStateModel.getData()) || {};
        var iExportLimit = SearchMaxResults.resolveExportLimit(mState);
        var oRequest;
        var mFilterData = (mInput && mInput.filterData) || (
            oSmart && typeof oSmart.getSmartFilterData === "function"
                ? oSmart.getSmartFilterData()
                : {}
        ) || {};

        if (!oRepo || typeof oRepo.exportSearchResults !== "function") {
            return Promise.reject(new Error("EXPORT_HANDLER_MISSING"));
        }
        if (aSelectedIds.length > iExportLimit) {
            return Promise.reject(new Error("EXPORT_LIMIT_EXCEEDED"));
        }

        oRequest = {
            entity: (mInput && mInput.entity) || "screen",
            limit: iExportLimit
        };

        if (aSelectedIds.length) {
            oRequest.rootIds = aSelectedIds;
            oRequest.selectionMode = "selected";
        } else {
            if (hasComplexDateRange(mFilterData.DateCheck)) {
                return resolveBoundRowsForCurrentFilters(mInput, mCtx);
            }
            oRequest.selectionMode = "all";
            oRequest.searchContract = buildSearchContract(mInput, mCtx);
        }

        return Promise.resolve(oRepo.exportSearchResults(oRequest));
    }

    function emitExportTelemetry(mCtx, sEventName, oPayload) {
        WorkflowTelemetry.emit(sEventName, {
            stateModel: mCtx && mCtx.stateModel,
            payload: oPayload || {}
        });
    }

    function execute(mInput, mCtx) {
        var sEntity = (mInput && mInput.entity) || "screen";
        return resolveExportRows(mInput || {}, mCtx || {}).then(function (aRows) {
            var aNormalized = normalizeRows(aRows, sEntity);
            var aSelectedIds = ChecklistIdentity.normalizeChecklistIds(mInput && mInput.selectedRowIds);
            var sMode = aSelectedIds.length ? "selected" : "all";
            if (!aNormalized.length) {
                return Result.fail({ message: "No export data", code: "NO_EXPORT_DATA" }, [Effects.toast("nothingToExport", "warning")]);
            }
            return SpreadsheetExport.download("checklist_" + sEntity, aNormalized).then(function () {
                emitExportTelemetry(mCtx, sMode === "selected" ? "export.selected.completed" : "export.all.completed", {
                    entity: sEntity,
                    rows: aNormalized.length,
                    selectionMode: sMode
                });
                return Result.ok({ entity: sEntity, exported: aNormalized.length }, [
                    Effects.toast("searchExportSuccess", "info"),
                    Effects.log("info", "Export completed", { entity: sEntity, rows: aNormalized.length })
                ]);
            });
        }).catch(function (oError) {
            var sCode = String((oError && (oError.code || oError.message)) || "").trim().toUpperCase();
            var sMessageCode = sCode === "EXPORT_LIMIT_EXCEEDED" ? "EXPORT_LIMIT_EXCEEDED" : "EXPORT_FAILED";
            emitExportTelemetry(mCtx, sCode === "EXPORT_LIMIT_EXCEEDED" ? "export.limit.exceeded" : "export.failed", {
                entity: sEntity,
                code: sMessageCode
            });
            return Result.fail({ message: sMessageCode, code: sMessageCode }, [Effects.toast("exportFailed", "error")]);
        });
    }

    return ExportSearchUseCase;
});
