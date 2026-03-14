sap.ui.define([
  "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaCore",
  "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DeltaContracts"
], function (DeltaCore, DeltaContracts) {
  "use strict";

  function appendChildChanges(aOut, sEntity, sRootKey, aCurrent, aBase, fnFieldsBuilder) {
    var mBase = DeltaCore.indexRows(aBase);
    var mCurrent = DeltaCore.indexRows(aCurrent);

    (aCurrent || []).forEach(function (oRow, iIndex) {
      var sKey = DeltaCore.rowKey(oRow);
      var oBaseRow = sKey ? mBase[sKey] : null;
      if (!sKey || !oBaseRow) {
        aOut.push(fnFieldsBuilder(oRow || {}, iIndex, DeltaContracts.EDIT_MODE.CREATE, sRootKey));
        return;
      }
      if (!DeltaCore.eq(oRow, oBaseRow)) {
        aOut.push(fnFieldsBuilder(oRow || {}, iIndex, DeltaContracts.EDIT_MODE.UPDATE, sRootKey));
      }
    });

    Object.keys(mBase).forEach(function (sKey) {
      if (!mCurrent[sKey]) { aOut.push(fnFieldsBuilder(DeltaCore.buildDeleteStub(mBase[sKey], sKey), 0, DeltaContracts.EDIT_MODE.DELETE, sRootKey)); }
    });
  }

  return { appendChildChanges: appendChildChanges };
});
