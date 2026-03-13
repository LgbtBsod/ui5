sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DeltaContracts"
], function (CloneUtil, DeltaContracts) {
  "use strict";

  function eq(a, b) { return JSON.stringify(a) === JSON.stringify(b); }
  function clone(v) { return CloneUtil.clone(v, null); }

  function isTechField(sKey) {
    return ["changed_on", "server_changed_on", "version_number", "_cacheTimestamp", "edit_mode"].indexOf(sKey) >= 0;
  }

  function diffFields(oCurrent, oBase) {
    var oDelta = {};
    Object.keys(oCurrent || {}).forEach(function (sKey) {
      if (isTechField(sKey)) { return; }
      if (!eq(oCurrent[sKey], (oBase || {})[sKey])) { oDelta[sKey] = oCurrent[sKey]; }
    });
    return oDelta;
  }

  function rowKey(oRow) {
    return String((oRow && (
      oRow.id ||
      oRow.Key ||
      oRow.check_uuid ||
      oRow.barrier_uuid ||
      oRow.part_uuid ||
      oRow.participant_uuid ||
      oRow.attach_uuid ||
      oRow.AttachmentKey ||
      oRow.client_row_id
    )) || "");
  }

  function buildDeleteStub(oRow, sKey) {
    var oSource = oRow || {};
    return Object.assign({}, oSource, {
      id: oSource.id || sKey,
      Key: oSource.Key || sKey,
      client_row_id: oSource.client_row_id || sKey,
      edit_mode: DeltaContracts.EDIT_MODE.DELETE
    });
  }

  function indexRows(aRows) {
    return (aRows || []).reduce(function (mAcc, oRow) {
      var sId = rowKey(oRow);
      if (sId) { mAcc[sId] = oRow; }
      return mAcc;
    }, {});
  }

  return {
    eq: eq,
    clone: clone,
    diffFields: diffFields,
    rowKey: rowKey,
    indexRows: indexRows,
    buildDeleteStub: buildDeleteStub
  };
});
