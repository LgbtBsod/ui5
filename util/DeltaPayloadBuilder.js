sap.ui.define([], function () {
  "use strict";

  function _eq(a, b) {
    return JSON.stringify(a) === JSON.stringify(b);
  }

  function _clone(v) {
    return JSON.parse(JSON.stringify(v));
  }

  function _parseODataDateMillis(vDate) {
    if (!vDate) {
      return null;
    }
    if (typeof vDate === "string") {
      var m = vDate.match(/^\/Date\((-?\d+)(?:[+-]\d+)?\)\/$/);
      if (m) {
        return Number(m[1]);
      }
    }
    var n = new Date(vDate).getTime();
    return Number.isNaN(n) ? null : n;
  }

  function _formatODataDate(vDate) {
    var nMillis = _parseODataDateMillis(vDate);
    if (nMillis === null) {
      nMillis = Date.now();
    }
    return "/Date(" + nMillis + ")/";
  }

  function _isTechField(sKey) {
    return ["changed_on", "server_changed_on", "version_number", "_cacheTimestamp"].indexOf(sKey) >= 0;
  }

  function _diffFields(oCurrent, oBase) {
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

  function _rowKey(oRow) {
    return String((oRow && (oRow.id || oRow.Key || oRow.check_uuid || oRow.barrier_uuid)) || "");
  }

  function _indexRows(aRows) {
    return (aRows || []).reduce(function (mAcc, oRow) {
      var sId = _rowKey(oRow);
      if (sId) {
        mAcc[sId] = oRow;
      }
      return mAcc;
    }, {});
  }

  function _mapBasicFields(oBasicDelta) {
    var o = {};
    if (Object.prototype.hasOwnProperty.call(oBasicDelta, "LOCATION_KEY")) {
      o.LocationKey = oBasicDelta.LOCATION_KEY;
    }
    if (Object.prototype.hasOwnProperty.call(oBasicDelta, "LOCATION_NAME")) {
      o.LocationName = oBasicDelta.LOCATION_NAME;
    }
    if (Object.prototype.hasOwnProperty.call(oBasicDelta, "equipment")) {
      o.EquipName = oBasicDelta.equipment;
    }
    if (Object.prototype.hasOwnProperty.call(oBasicDelta, "LPC_KEY")) {
      o.Lpc = oBasicDelta.LPC_KEY;
    }
    if (Object.prototype.hasOwnProperty.call(oBasicDelta, "PROF_KEY")) {
      o.Profession = oBasicDelta.PROF_KEY;
    }
    if (Object.prototype.hasOwnProperty.call(oBasicDelta, "date")) {
      o.DateCheck = _formatODataDate(oBasicDelta.date);
    }
    if (Object.prototype.hasOwnProperty.call(oBasicDelta, "time")) {
      o.TimeCheck = oBasicDelta.time;
    }
    if (Object.prototype.hasOwnProperty.call(oBasicDelta, "timezone")) {
      o.TimeZone = oBasicDelta.timezone;
    }
    return o;
  }

  function _toCheckFields(oRow, iIndex) {
    return {
      ChecksNum: Number(oRow.checksNum || oRow.ChecksNum || oRow.position || iIndex + 1),
      Comment: oRow.comment || oRow.Comment || "",
      Result: !!(Object.prototype.hasOwnProperty.call(oRow, "result") ? oRow.result : oRow.Result)
    };
  }

  function _toBarrierFields(oRow, iIndex) {
    return {
      BarriersNum: Number(oRow.barriersNum || oRow.BarriersNum || oRow.position || iIndex + 1),
      Comment: oRow.comment || oRow.Comment || "",
      Result: !!(Object.prototype.hasOwnProperty.call(oRow, "result") ? oRow.result : oRow.Result)
    };
  }

  function _appendChildChanges(aOut, sEntity, sRootKey, aCurrent, aBase, fnFieldsBuilder) {
    var mBase = _indexRows(aBase);
    var mCurrent = _indexRows(aCurrent);

    (aCurrent || []).forEach(function (oRow, iIndex) {
      var sKey = _rowKey(oRow);
      var oBaseRow = sKey ? mBase[sKey] : null;

      if (!sKey || !oBaseRow) {
        aOut.push({
          Entity: sEntity,
          Key: sKey || ("tmp-" + sEntity.toLowerCase() + "-" + Date.now() + "-" + iIndex),
          ParentKey: sRootKey,
          EditMode: "C",
          Fields: fnFieldsBuilder(oRow || {}, iIndex)
        });
        return;
      }

      if (!_eq(oRow, oBaseRow)) {
        aOut.push({
          Entity: sEntity,
          Key: sKey,
          ParentKey: sRootKey,
          EditMode: "U",
          Fields: fnFieldsBuilder(oRow || {}, iIndex)
        });
      }
    });

    Object.keys(mBase).forEach(function (sKey) {
      if (!mCurrent[sKey]) {
        aOut.push({
          Entity: sEntity,
          Key: sKey,
          ParentKey: sRootKey,
          EditMode: "D",
          Fields: {}
        });
      }
    });
  }

  function buildDeltaPayload(oCurrent, oBase) {
    var oCur = oCurrent || {};
    var oBas = oBase || {};
    var sRootKey = String((((oCur || {}).root || {}).id) || (((oBas || {}).root || {}).id) || "");

    var aChanges = [];
    var oRootDelta = _diffFields(oCur.root || {}, oBas.root || {});
    var oBasicDelta = _diffFields(oCur.basic || {}, oBas.basic || {});

    if (Object.keys(oRootDelta).length) {
      aChanges.push({
        Entity: "ROOT",
        Key: sRootKey,
        ParentKey: sRootKey,
        EditMode: "U",
        Fields: _clone(oRootDelta)
      });
    }

    var oCanonicalBasicFields = _mapBasicFields(oBasicDelta);
    if (Object.keys(oCanonicalBasicFields).length) {
      aChanges.push({
        Entity: "BASIC",
        Key: sRootKey,
        ParentKey: sRootKey,
        EditMode: "U",
        Fields: oCanonicalBasicFields
      });
    }

    _appendChildChanges(aChanges, "CHECK", sRootKey, oCur.checks || [], oBas.checks || [], _toCheckFields);
    _appendChildChanges(aChanges, "BARRIER", sRootKey, oCur.barriers || [], oBas.barriers || [], _toBarrierFields);

    if (!aChanges.length) {
      return null;
    }

    return {
      RootKey: sRootKey,
      ClientAggChangedOn: _formatODataDate((((oCur || {}).meta || {}).aggChangedOn) || (((oCur || {}).root || {}).server_changed_on) || (((oBas || {}).meta || {}).aggChangedOn) || (((oBas || {}).root || {}).server_changed_on)),
      Changes: aChanges
    };
  }

  return {
    buildDeltaPayload: buildDeltaPayload
  };
});
