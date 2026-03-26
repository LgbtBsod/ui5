sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts"
], function (DeltaContracts, ODataEntityContracts) {
  "use strict";

  var IDENTITY = ODataEntityContracts.IDENTITY;

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

  function mapRootFields(oRootDelta, oBasicDelta, sRootKey, sEditMode) {
    var oRoot = {};
    var oRootChanges = oRootDelta || {};
    var oBasicChanges = oBasicDelta || {};

    assignIfPresent(oRoot, "pcct_uuid", String(sRootKey || "").trim());
    assignIfPresent(oRoot, "edit_mode", DeltaContracts.normalizeEditMode(sEditMode, DeltaContracts.EDIT_MODE.UPDATE));
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
    assignIfPresent(oRoot, "checks_number", pickValue(oBasicChanges, ["CHECKS_NUMBER", "ChecksNumber", "checks_number"]));
    assignIfPresent(oRoot, "barriers_number", pickValue(oBasicChanges, ["BARRIERS_NUMBER", "BarriersNumber", "barriers_number"]));
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
    var sKey = pickValue(oRow, ["id", IDENTITY.ROOT_CANONICAL_FIELDS[1], "check_uuid"]);
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
    var sKey = pickValue(oRow, ["id", IDENTITY.ROOT_CANONICAL_FIELDS[1], "barrier_uuid"]);
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

  function toParticipantFields(oRow, iIndex, sEditMode) {
    var sKey = pickValue(oRow, ["id", IDENTITY.ROOT_CANONICAL_FIELDS[1], "part_uuid", "participant_uuid"]);
    var sMode = DeltaContracts.normalizeEditMode(sEditMode, DeltaContracts.EDIT_MODE.UPDATE);
    var bCreate = sMode === DeltaContracts.EDIT_MODE.CREATE;
    return {
      part_uuid: bCreate ? "" : String(sKey || "").trim(),
      client_row_id: String(pickValue(oRow, ["client_row_id", "id", IDENTITY.ROOT_CANONICAL_FIELDS[1]]) || "").trim(),
      edit_mode: sMode,
      part_num: Number(oRow.partNum || oRow.PartNum || oRow.position || iIndex + 1),
      role_code: String(pickValue(oRow, ["role_code", "RoleCode", "role", "Role"]) || "").trim(),
      fullname: String(pickValue(oRow, ["fullname", "Fullname", "FULLNAME", "name", "Name"]) || "").trim(),
      pernr: String(pickValue(oRow, ["pernr", "Pernr", "PERNR"]) || "").trim(),
      position: String(pickValue(oRow, ["position", "Position", "POSITION"]) || "").trim(),
      orgunit: String(pickValue(oRow, ["orgunit", "OrgUnit", "ORGUNIT"]) || "").trim()
    };
  }

  function toAttachmentFields(oRow, iIndex, sEditMode, sRootKey) {
    var sMode = DeltaContracts.normalizeEditMode(sEditMode, DeltaContracts.EDIT_MODE.UPDATE);
    var sKey = pickValue(oRow, IDENTITY.ATTACHMENT_KEY_FIELDS);
    var bCreate = sMode === DeltaContracts.EDIT_MODE.CREATE;
    return {
      attach_uuid: bCreate ? "" : String(sKey || "").trim(),
      client_row_id: String(pickValue(oRow, ["client_row_id"].concat(IDENTITY.ATTACHMENT_KEY_FIELDS)) || "").trim(),
      edit_mode: sMode,
      root_key: String(pickValue(oRow, IDENTITY.ROOT_KEY_FIELDS) || sRootKey || "").trim(),
      parent_key: String(pickValue(oRow, IDENTITY.PARENT_KEY_FIELDS) || sRootKey || "").trim(),
      folder_key: String(pickValue(oRow, ["FolderKey", "folderKey"].concat(IDENTITY.PARENT_KEY_FIELDS)) || sRootKey || "").trim(),
      category_key: String(pickValue(oRow, ["CategoryKey", "categoryKey", "Type", "type"]) || "GEN").trim() || "GEN",
      file_name: String(pickValue(oRow, IDENTITY.FILE_NAME_FIELDS) || "").trim(),
      mime_type: String(pickValue(oRow, IDENTITY.MIME_TYPE_FIELDS) || "application/octet-stream").trim() || "application/octet-stream",
      description: String(pickValue(oRow, ["Description", "description"]) || "").trim(),
      file_size: Number(pickValue(oRow, IDENTITY.FILE_SIZE_FIELDS) || 0) || 0
    };
  }

  return {
    mapRootFields: mapRootFields,
    toCheckFields: toCheckFields,
    toBarrierFields: toBarrierFields,
    toParticipantFields: toParticipantFields,
    toAttachmentFields: toAttachmentFields
  };
});
