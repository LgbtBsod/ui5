sap.ui.define([], function () {
  "use strict";

  function _clone(v) {
    return JSON.parse(JSON.stringify(v));
  }

  function _eq(a, b) {
    return JSON.stringify(a) === JSON.stringify(b);
  }

  function _isTechField(sKey) {
    return ["changed_on", "server_changed_on", "version_number", "_cacheTimestamp"].indexOf(sKey) >= 0;
  }

  function _diffObject(oCurrent, oBase) {
    var oDelta = {};
    Object.keys(oCurrent || {}).forEach(function (sKey) {
      if (_isTechField(sKey)) {
        return;
      }
      if (!_eq(oCurrent[sKey], (oBase || {})[sKey])) {
        oDelta[sKey] = oCurrent[sKey];
      }
    });
    return oDelta;
  }

  function _indexRows(aRows) {
    return (aRows || []).reduce(function (mAcc, oRow) {
      var sId = String((oRow && (oRow.id || oRow.check_uuid || oRow.barrier_uuid)) || "");
      if (sId) {
        mAcc[sId] = oRow;
      }
      return mAcc;
    }, {});
  }

  function _diffRows(aCurrent, aBase) {
    var mBase = _indexRows(aBase);
    var mCurrent = _indexRows(aCurrent);
    var aDelta = [];

    (aCurrent || []).forEach(function (oRow) {
      var sId = String((oRow && (oRow.id || oRow.check_uuid || oRow.barrier_uuid)) || "");
      var oBaseRow = sId ? mBase[sId] : null;
      var oOut = _clone(oRow || {});

      if (!sId || !oBaseRow) {
        oOut.edit_mode = "C";
        aDelta.push(oOut);
        return;
      }

      if (!_eq(oRow, oBaseRow)) {
        oOut.edit_mode = "U";
        aDelta.push(oOut);
      }
    });

    Object.keys(mBase).forEach(function (sId) {
      if (!mCurrent[sId]) {
        aDelta.push({ id: sId, edit_mode: "D" });
      }
    });

    return aDelta;
  }

  function buildDeltaPayload(oCurrent, oBase) {
    var oCur = oCurrent || {};
    var oBas = oBase || {};

    var oRootDelta = _diffObject(oCur.root || {}, oBas.root || {});
    var oBasicDelta = _diffObject(oCur.basic || {}, oBas.basic || {});
    var aChecksDelta = _diffRows(oCur.checks || [], oBas.checks || []);
    var aBarriersDelta = _diffRows(oCur.barriers || [], oBas.barriers || []);

    var bHas = Object.keys(oRootDelta).length || Object.keys(oBasicDelta).length || aChecksDelta.length || aBarriersDelta.length;
    if (!bHas) {
      return null;
    }

    return {
      root: oRootDelta,
      basic: oBasicDelta,
      checks: aChecksDelta,
      barriers: aBarriersDelta,
      client_changed_on: (oCur.root || {}).changed_on || null,
      client_version: (oCur.root || {}).version_number || null
    };
  }

  return {
    buildDeltaPayload: buildDeltaPayload
  };
});
