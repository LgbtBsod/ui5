sap.ui.define([], function () {
  "use strict";

  function eq(a, b) { return JSON.stringify(a) === JSON.stringify(b); }
  function clone(v) { return JSON.parse(JSON.stringify(v)); }

  function isTechField(sKey) {
    return ["changed_on", "server_changed_on", "version_number", "_cacheTimestamp"].indexOf(sKey) >= 0;
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
    return String((oRow && (oRow.id || oRow.Key || oRow.check_uuid || oRow.barrier_uuid)) || "");
  }

  function indexRows(aRows) {
    return (aRows || []).reduce(function (mAcc, oRow) {
      var sId = rowKey(oRow);
      if (sId) { mAcc[sId] = oRow; }
      return mAcc;
    }, {});
  }

  return { eq: eq, clone: clone, diffFields: diffFields, rowKey: rowKey, indexRows: indexRows };
});
