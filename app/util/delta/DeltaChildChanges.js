sap.ui.define(["PRODUCTION_CONTROL_CHECKLIST/util/delta/DeltaCore"], function (DeltaCore) {
  "use strict";

  function appendChildChanges(aOut, sEntity, sRootKey, aCurrent, aBase, fnFieldsBuilder) {
    var mBase = DeltaCore.indexRows(aBase);
    var mCurrent = DeltaCore.indexRows(aCurrent);

    (aCurrent || []).forEach(function (oRow, iIndex) {
      var sKey = DeltaCore.rowKey(oRow);
      var oBaseRow = sKey ? mBase[sKey] : null;
      if (!sKey || !oBaseRow) {
        aOut.push(fnFieldsBuilder(oRow || {}, iIndex, "C", sRootKey));
        return;
      }
      if (!DeltaCore.eq(oRow, oBaseRow)) {
        aOut.push(fnFieldsBuilder(oRow || {}, iIndex, "U", sRootKey));
      }
    });

    Object.keys(mBase).forEach(function (sKey) {
      if (!mCurrent[sKey]) { aOut.push(fnFieldsBuilder(mBase[sKey] || { id: sKey, Key: sKey }, 0, "D", sRootKey)); }
    });
  }

  return { appendChildChanges: appendChildChanges };
});
