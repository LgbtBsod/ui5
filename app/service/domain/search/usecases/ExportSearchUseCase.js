sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/util/ExcelExport"
], function (UseCase, Result, Effects, ExcelExport) {
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

    function normalizeRows(aRows, sEntity) {
        return (aRows || []).map(function (oRow) {
            var o = oRow || {};
            return {
                Id: pick(o.Id || o.id, ""),
                Lpc: pick(o.LpcText || o.Lpc || o.lpc, ""),
                Profession: pick(o.ProfessionText || o.Profession || o.profession, ""),
                Location: pick(o.LocationKey || o.location_key, ""),
                Status: pick(o.Status || o.status, ""),
                SuccessChecksRate: pick(o.SuccessChecksRate || o.success_checks_rate, ""),
                SuccessBarriersRate: pick(o.SuccessBarriersRate || o.success_barriers_rate || o.barriers_rate, ""),
                DateCheck: pick(o.DateCheck || o.date_check, ""),
                EquipName: pick(o.EquipName || o.equipment, ""),
                ChangedOn: pick(o.ChangedOn || o.changed_on, "")
            };
        }).filter(function (oRow) {
            if (sEntity === "check") {
                return oRow.SuccessChecksRate !== "";
            }
            if (sEntity === "barrier") {
                return oRow.SuccessBarriersRate !== "";
            }
            return true;
        });
    }

    function extractChecklistId(oRow) {
        return String(
            (oRow && (oRow.Key || oRow.key || oRow.Id || oRow.id || oRow.RequestId || oRow.checklist_id)) || ""
        ).trim();
    }

    function resolveExportRows(mInput, mCtx) {
        var oSmart = mCtx && mCtx.smartControls;
        var aSelectedIds = normalizeChecklistIds(mInput && mInput.selectedRowIds);
        var iBackendTop = Math.max(0, Number(mInput && mInput.backendTop) || 0);
        if (!oSmart) {
            return Promise.resolve([]);
        }
        if (aSelectedIds.length) {
            return Promise.resolve(
                typeof oSmart.getSelectedRows === "function" ? oSmart.getSelectedRows() : []
            ).then(function (aSelectedRows) {
                var aRows = Array.isArray(aSelectedRows) && aSelectedRows.length
                    ? aSelectedRows
                    : (typeof oSmart.getVisibleRows === "function" ? oSmart.getVisibleRows() : []);
                return aRows.filter(function (oRow) {
                    return aSelectedIds.indexOf(extractChecklistId(oRow)) >= 0;
                });
            });
        }
        if (typeof oSmart.getBoundRows === "function") {
            return Promise.resolve(oSmart.getBoundRows(iBackendTop));
        }
        return Promise.resolve(typeof oSmart.getVisibleRows === "function" ? oSmart.getVisibleRows() : []);
    }

    ExportSearchUseCase.prototype.execute = function (mInput, mCtx) {
        var sEntity = (mInput && mInput.entity) || "screen";
        return resolveExportRows(mInput || {}, mCtx || {}).then(function (aRows) {
            var aNormalized = normalizeRows(aRows, sEntity);
            if (!aNormalized.length) {
                return Result.fail({ message: "No export data", code: "NO_EXPORT_DATA" }, [Effects.toast("nothingToExport", "warning")]);
            }
            try {
                ExcelExport.download("checklist_" + sEntity, aNormalized);
            } catch (_e) {
                return Result.fail({ message: "Export failed", code: "EXPORT_FAILED" }, [Effects.toast("exportFailed", "error")]);
            }
            return Result.ok({ entity: sEntity, exported: aNormalized.length }, [
                Effects.toast("searchExportSuccess", "info"),
                Effects.log("info", "Export completed", { entity: sEntity, rows: aNormalized.length })
            ]);
        });
    };

    return ExportSearchUseCase;
});
