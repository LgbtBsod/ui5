sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/util/ExcelExport",
    "PRODUCTION_CONTROL_CHECKLIST/util/search/SearchMaxResults",
    "PRODUCTION_CONTROL_CHECKLIST/util/WorkflowTelemetry"
], function (UseCase, Result, Effects, ExcelExport, SearchMaxResults, WorkflowTelemetry) {
    "use strict";

    function ExportSearchUseCase() {
        UseCase.call(this, "ExportSearchUseCase");
    }

    ExportSearchUseCase.prototype = Object.create(UseCase.prototype);
    ExportSearchUseCase.prototype.constructor = ExportSearchUseCase;

    function pick(v, fallback) {
        return v === undefined || v === null ? fallback : v;
    }

    function normalizeChecklistIds(aIds) {
        var mSeen = {};
        return (aIds || []).reduce(function (aAcc, sId) {
            var sNormalized = String(sId || "").trim();
            if (!sNormalized || mSeen[sNormalized]) {
                return aAcc;
            }
            mSeen[sNormalized] = true;
            aAcc.push(sNormalized);
            return aAcc;
        }, []);
    }

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
        var sFrom = pickFilterValue(oFirstRange.value1 || oFirstRange.low || vValue);
        var sTo = pickFilterValue(oFirstRange.value2 || oFirstRange.high || sFrom);
        return {
            dateFrom: sFrom,
            dateTo: sTo || sFrom
        };
    }

    function normalizeRows(aRows, sEntity) {
        return (aRows || []).map(function (oRow) {
            var o = oRow || {};
            return {
                RootKey: pick(o.RootKey || o.rootKey || o.Key || o.key, ""),
                Id: pick(o.Id || o.id, ""),
                Lpc: pick(o.LpcText || o.Lpc || o.lpc, ""),
                Profession: pick(o.ProfessionText || o.Profession || o.profession, ""),
                Location: pick(o.LocationKey || o.location_key, ""),
                Status: pick(o.Status || o.status, ""),
                DateCheck: pick(o.DateCheck || o.date_check, ""),
                EquipName: pick(o.EquipName || o.equipment, ""),
                ChangedOn: pick(o.ChangedOn || o.changed_on, ""),
                ItemType: pick(o.ItemType || o.itemType, ""),
                Num: pick(o.Num || o.num, ""),
                Text: pick(o.Text || o.text, ""),
                Comment: pick(o.Comment || o.comment, ""),
                Result: pick(o.Result || o.result, "")
            };
        }).filter(function (oRow) {
            if (sEntity === "check" || sEntity === "barrier") {
                return oRow.ItemType !== "ROOT";
            }
            return oRow.ItemType === "ROOT" || !oRow.ItemType;
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
            filterLpc: pickStateOrFilter(mFilterData.Lpc, mState.filterLpc),
            filterProfession: pickFilterValue(mFilterData.ProfessionText),
            filterStatus: pickFilterValue(mFilterData.Status),
            searchMode: String(mState.searchMode || "EXACT").toUpperCase(),
            checksSegment: String(((mState.search || {}).checksFailSegment) || mState.filterFailedChecks || "ALL").toUpperCase(),
            barriersSegment: String(((mState.search || {}).barriersFailSegment) || mState.filterFailedBarriers || "ALL").toUpperCase()
        };
    }

    function resolveExportRows(mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var oStateModel = mCtx && mCtx.stateModel;
        var aSelectedIds = normalizeChecklistIds(mInput && mInput.selectedRowIds);
        var mState = (oStateModel && oStateModel.getData && oStateModel.getData()) || {};
        var iExportLimit = SearchMaxResults.resolveExportLimit(mState);
        var oRequest;

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

    ExportSearchUseCase.prototype.execute = function (mInput, mCtx) {
        var sEntity = (mInput && mInput.entity) || "screen";
        return resolveExportRows(mInput || {}, mCtx || {}).then(function (aRows) {
            var aNormalized = normalizeRows(aRows, sEntity);
            var aSelectedIds = normalizeChecklistIds(mInput && mInput.selectedRowIds);
            var sMode = aSelectedIds.length ? "selected" : "all";
            if (!aNormalized.length) {
                return Result.fail({ message: "No export data", code: "NO_EXPORT_DATA" }, [Effects.toast("nothingToExport", "warning")]);
            }
            try {
                ExcelExport.download("checklist_" + sEntity, aNormalized);
            } catch (_e) {
                return Result.fail({ message: "Export failed", code: "EXPORT_FAILED" }, [Effects.toast("exportFailed", "error")]);
            }
            emitExportTelemetry(mCtx, sMode === "selected" ? "export.selected.completed" : "export.all.completed", {
                entity: sEntity,
                rows: aNormalized.length,
                selectionMode: sMode
            });
            return Result.ok({ entity: sEntity, exported: aNormalized.length }, [
                Effects.toast("searchExportSuccess", "info"),
                Effects.log("info", "Export completed", { entity: sEntity, rows: aNormalized.length })
            ]);
        }).catch(function (oError) {
            var sCode = String((oError && (oError.code || oError.message)) || "").trim().toUpperCase();
            var sMessageCode = sCode === "EXPORT_LIMIT_EXCEEDED" ? "EXPORT_LIMIT_EXCEEDED" : "EXPORT_FAILED";
            emitExportTelemetry(mCtx, sCode === "EXPORT_LIMIT_EXCEEDED" ? "export.limit.exceeded" : "export.failed", {
                entity: sEntity,
                code: sMessageCode
            });
            return Result.fail({ message: sMessageCode, code: sMessageCode }, [Effects.toast("exportFailed", "error")]);
        });
    };

    return ExportSearchUseCase;
});
