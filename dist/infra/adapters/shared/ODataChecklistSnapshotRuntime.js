sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper"
], function (GatewayRequestRuntime, ChecklistSnapshotMapper) {
    "use strict";

    function firstRow(vData) {
        var oUnwrapped = GatewayRequestRuntime.unwrap(vData);
        if (Array.isArray(oUnwrapped)) { return oUnwrapped[0] || {}; }
        if (oUnwrapped && Array.isArray(oUnwrapped.results)) { return oUnwrapped.results[0] || {}; }
        return oUnwrapped || {};
    }

    function pad2(iValue) {
        var s = String(iValue);
        return s.length >= 2 ? s : "0" + s;
    }

    function formatUtcDate(oDate) {
        if (!(oDate instanceof Date) || Number.isNaN(oDate.getTime())) {
            return "";
        }
        return [
            oDate.getUTCFullYear(),
            pad2(oDate.getUTCMonth() + 1),
            pad2(oDate.getUTCDate())
        ].join("-");
    }

    function toYmd(vDateCheck) {
        var s;
        var m;
        var oDate;
        if (vDateCheck instanceof Date) {
            return formatUtcDate(vDateCheck);
        }
        if (typeof vDateCheck === "number" && Number.isFinite(vDateCheck)) {
            return formatUtcDate(new Date(vDateCheck));
        }
        s = String(vDateCheck || "").trim();
        if (!s) {
            return "";
        }
        m = /\/(?:Date\()?([0-9]{10,13})/.exec(s);
        if (m) {
            return formatUtcDate(new Date(Number(m[1])));
        }
        if (/^\d{4}-\d{2}-\d{2}/.test(s)) {
            return s.slice(0, 10);
        }
        oDate = new Date(s);
        return formatUtcDate(oDate);
    }

    function mapBasic(oBasic) {
        var o = oBasic || {};
        var sBrowserTimezone = (Intl && Intl.DateTimeFormat && Intl.DateTimeFormat().resolvedOptions && Intl.DateTimeFormat().resolvedOptions().timeZone) || "UTC";
        return {
            checklist_id: o.ChecklistId || o.ChecklistID || o.CHECKLIST_ID || o.checklist_id || "",
            date: toYmd(o.DateCheck || o.date),
            time: String(o.TimeCheck || o.time || "").slice(0, 5),
            timezone: o.TimeZone || o.timezone || sBrowserTimezone,
            equipment: o.EquipName || o.equipment || "",
            OBSERVER_FULLNAME: o.ObserverFullname || o.OBSERVER_FULLNAME || "",
            OBSERVER_PERNER: o.ObserverPernr || o.OBSERVER_PERNER || "",
            OBSERVER_POSITION: o.ObserverPosition || o.OBSERVER_POSITION || "",
            OBSERVER_ORGUNIT: o.ObserverOrgUnit || o.OBSERVER_ORGUNIT || "",
            OBSERVED_FULLNAME: o.ObservedFullname || o.OBSERVED_FULLNAME || "",
            OBSERVED_PERNER: o.ObservedPernr || o.OBSERVED_PERNER || "",
            OBSERVED_POSITION: o.ObservedPosition || o.OBSERVED_POSITION || "",
            OBSERVED_ORGUNIT: o.ObservedOrgUnit || o.OBSERVED_ORGUNIT || "",
            LOCATION_KEY: o.LocationKey || o.LOCATION_KEY || "",
            LOCATION_NAME: o.LocationName || o.LOCATION_NAME || "",
            LOCATION_TEXT: o.LocationText || o.LOCATION_TEXT || "",
            LPC_KEY: o.Lpc || o.LPC_KEY || "",
            LPC_TEXT: o.LpcText || o.LPC_TEXT || o.Lpc || "",
            PROF_KEY: o.Profession || o.PROF_KEY || "",
            PROF_TEXT: o.ProfessionText || o.PROF_TEXT || o.Profession || "",
            CHECKS_NUMBER: o.ChecksNumber || o.CHECKS_NUMBER || o.checks_number || "",
            CHECKS_NUMBER_TEXT: o.ChecksNumberText || o.CHECKS_NUMBER_TEXT || o.checks_number_text || "",
            BARRIERS_NUMBER: o.BarriersNumber || o.BARRIERS_NUMBER || o.barriers_number || "",
            BARRIERS_NUMBER_TEXT: o.BarriersNumberText || o.BARRIERS_NUMBER_TEXT || o.barriers_number_text || ""
        };
    }

    function mapResult(oRoot, oBasic, oChecks, oBarriers) {
        var oRootRow = firstRow(oRoot);
        var oBasicRow = firstRow(oBasic);
        var oMappedBasic = mapBasic(oBasicRow);
        var sAggChangedOn = oRootRow.ChangedOn || oRootRow.AggChangedOn || "";
        var iVersionNumber = Number(oRootRow.VersionNumber || oRootRow.version_number || 0) || 0;
        var sRootId = String(oRootRow.Key || oRootRow.RootKey || oRootRow.pcct_uuid || "").trim();
        if (sAggChangedOn && !oRootRow.server_changed_on) {
            oRootRow.server_changed_on = sAggChangedOn;
        }
        if (sRootId && !oRootRow.id) {
            oRootRow.id = sRootId;
        }
        if (sRootId && !oRootRow.pcct_uuid) {
            oRootRow.pcct_uuid = sRootId;
        }
        if (!oRootRow.status) {
            oRootRow.status = String(oRootRow.Status || "").trim();
        }
        if (!oRootRow.checklist_id) {
            oRootRow.checklist_id = String(oRootRow.RequestId || oRootRow.Id || "").trim();
        }
        if (!oRootRow.overall_result && typeof oRootRow.HasFailedChecks === "boolean" && typeof oRootRow.HasFailedBarriers === "boolean") {
            oRootRow.overall_result = !(oRootRow.HasFailedChecks || oRootRow.HasFailedBarriers);
        }
        if (oMappedBasic.checklist_id && !oRootRow.checklist_id) {
            oRootRow.checklist_id = oMappedBasic.checklist_id;
        }
        if (iVersionNumber) {
            oRootRow.version_number = iVersionNumber;
            oRootRow.VersionNumber = iVersionNumber;
        }
        return {
            root: oRootRow,
            basic: oMappedBasic,
            checks: GatewayRequestRuntime.asArray(oChecks).map(ChecklistSnapshotMapper.mapCheckRow),
            barriers: GatewayRequestRuntime.asArray(oBarriers).map(ChecklistSnapshotMapper.mapBarrierRow),
            attachments: [],
            meta: { source: "GatewayODataClient", aggChangedOn: sAggChangedOn, versionNumber: iVersionNumber }
        };
    }

    return {
        firstRow: firstRow,
        mapResult: mapResult
    };
});
