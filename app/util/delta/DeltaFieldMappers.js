sap.ui.define(["checklist/app/util/delta/DeltaDateCodec"], function (DeltaDateCodec) {
  "use strict";

  function assignIfPresent(oTarget, sKey, vValue) {
    if (vValue !== undefined) {
      oTarget[sKey] = vValue;
    }
  }

  function pickValue(oSource, aKeys) {
    var i;
    var sKey;
    for (i = 0; i < aKeys.length; i += 1) {
      sKey = aKeys[i];
      if (oSource && Object.prototype.hasOwnProperty.call(oSource, sKey)) {
        return oSource[sKey];
      }
    }
    return undefined;
  }

  function mapRootFields(oRootDelta, oBasicDelta, sRootKey) {
    var oRoot = {};
    var oRootChanges = oRootDelta || {};
    var oBasicChanges = oBasicDelta || {};

    assignIfPresent(oRoot, "pcct_uuid", String(sRootKey || "").trim());
    assignIfPresent(oRoot, "status", pickValue(oRootChanges, ["status", "Status"]));
    assignIfPresent(oRoot, "checklist_id", pickValue(oRootChanges, ["Id", "RequestId", "id", "request_id", "checklist_id"]));
    assignIfPresent(oRoot, "lpc", pickValue(oRootChanges, ["Lpc", "lpc"]));
    if (!Object.prototype.hasOwnProperty.call(oRoot, "lpc")) {
      assignIfPresent(oRoot, "lpc", pickValue(oBasicChanges, ["LPC_KEY"]));
    }
    assignIfPresent(oRoot, "date", pickValue(oBasicChanges, ["date", "DateCheck"]));
    assignIfPresent(oRoot, "time_check", pickValue(oBasicChanges, ["time", "TimeCheck"]));
    assignIfPresent(oRoot, "time_zone", pickValue(oBasicChanges, ["timezone", "TimeZone"]));
    assignIfPresent(oRoot, "equipment", pickValue(oBasicChanges, ["equipment", "EquipName"]));
    assignIfPresent(oRoot, "location_key", pickValue(oBasicChanges, ["LOCATION_KEY", "LocationKey"]));
    assignIfPresent(oRoot, "location_name", pickValue(oBasicChanges, ["LOCATION_NAME", "LocationName"]));
    assignIfPresent(oRoot, "location_text", pickValue(oBasicChanges, ["LOCATION_TEXT", "LocationText", "LOCATION_NAME", "LocationName"]));
    assignIfPresent(oRoot, "observer_fullname", pickValue(oBasicChanges, ["OBSERVER_FULLNAME", "ObserverFullname"]));
    assignIfPresent(oRoot, "observer_perner", pickValue(oBasicChanges, ["OBSERVER_PERNER", "ObserverPernr"]));
    assignIfPresent(oRoot, "observer_position", pickValue(oBasicChanges, ["OBSERVER_POSITION", "ObserverPosition"]));
    assignIfPresent(oRoot, "observer_orgunit", pickValue(oBasicChanges, ["OBSERVER_ORGUNIT", "ObserverOrgUnit"]));
    assignIfPresent(oRoot, "observed_fullname", pickValue(oBasicChanges, ["OBSERVED_FULLNAME", "ObservedFullname"]));
    assignIfPresent(oRoot, "observed_perner", pickValue(oBasicChanges, ["OBSERVED_PERNER", "ObservedPernr"]));
    assignIfPresent(oRoot, "observed_position", pickValue(oBasicChanges, ["PROF_KEY", "OBSERVED_POSITION", "ObservedPosition"]));
    assignIfPresent(oRoot, "observed_orgunit", pickValue(oBasicChanges, ["OBSERVED_ORGUNIT", "ObservedOrgUnit"]));

    return oRoot;
  }

  function toCheckFields(oRow, iIndex, sEditMode) {
    var sKey = pickValue(oRow, ["id", "Key", "check_uuid"]);
    var bCreate = String(sEditMode || "U").toUpperCase() === "C";
    return {
      check_uuid: bCreate ? "" : String(sKey || "").trim(),
      client_row_id: bCreate ? String(pickValue(oRow, ["client_row_id", "id", "Key"]) || "").trim() : "",
      edit_mode: String(sEditMode || "U").toUpperCase(),
      checks_num: Number(oRow.checksNum || oRow.ChecksNum || oRow.position || iIndex + 1),
      text: oRow.text || oRow.Text || "",
      comment: oRow.comment || oRow.Comment || "",
      result: !!(Object.prototype.hasOwnProperty.call(oRow, "result") ? oRow.result : oRow.Result)
    };
  }

  function toBarrierFields(oRow, iIndex, sEditMode) {
    var sKey = pickValue(oRow, ["id", "Key", "barrier_uuid"]);
    var bCreate = String(sEditMode || "U").toUpperCase() === "C";
    return {
      barrier_uuid: bCreate ? "" : String(sKey || "").trim(),
      client_row_id: bCreate ? String(pickValue(oRow, ["client_row_id", "id", "Key"]) || "").trim() : "",
      edit_mode: String(sEditMode || "U").toUpperCase(),
      barriers_num: Number(oRow.barriersNum || oRow.BarriersNum || oRow.position || iIndex + 1),
      text: oRow.text || oRow.Text || oRow.description || oRow.Description || "",
      comment: oRow.comment || oRow.Comment || "",
      result: !!(Object.prototype.hasOwnProperty.call(oRow, "result") ? oRow.result : oRow.Result)
    };
  }

  return { mapRootFields: mapRootFields, toCheckFields: toCheckFields, toBarrierFields: toBarrierFields };
});
