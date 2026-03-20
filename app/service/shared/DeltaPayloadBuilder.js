sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaFieldMappers",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (DeltaCore, DeltaFieldMappers, DeltaContracts, CreateSentinel) {
  "use strict";

  function parseODataDateMillis(vDate) {
    if (!vDate) { return null; }
    if (typeof vDate === "string") {
      var m = vDate.match(/^\/Date\((-?\d+)(?:[+-]\d+)?\)\/$/);
      if (m) { return Number(m[1]); }
    }
    var n = new Date(vDate).getTime();
    return Number.isNaN(n) ? null : n;
  }

  function formatODataDate(vDate) {
    var nMillis = parseODataDateMillis(vDate);
    if (nMillis === null) { return null; }
    return "/Date(" + nMillis + ")/";
  }

  function appendChildChanges(aOut, sRootKey, aCurrent, aBase, fnFieldsBuilder) {
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
      if (!mCurrent[sKey]) {
        aOut.push(fnFieldsBuilder(DeltaCore.buildDeleteStub(mBase[sKey], sKey), 0, DeltaContracts.EDIT_MODE.DELETE, sRootKey));
      }
    });
  }

  function resolveRootKey(oCurrent, oBase) {
    return String(
      (((oCurrent || {}).root || {}).id)
      || (((oCurrent || {}).root || {}).Key)
      || (((oBase || {}).root || {}).id)
      || (((oBase || {}).root || {}).Key)
      || ""
    ).trim();
  }

  function resolveClientVersion(oCurrent, oBase) {
    var vVersion = (((oCurrent || {}).root || {}).version_number);
    if (vVersion == null || vVersion === "") {
      vVersion = (((oCurrent || {}).root || {}).VersionNumber);
    }
    if (vVersion == null || vVersion === "") {
      vVersion = (((oBase || {}).root || {}).version_number);
    }
    if (vVersion == null || vVersion === "") {
      vVersion = (((oBase || {}).root || {}).VersionNumber);
    }
    vVersion = Number(vVersion);
    return Number.isFinite(vVersion) ? vVersion : 0;
  }

  function buildDeltaPayload(oCurrent, oBase, mOptions) {
    var oCur = oCurrent || {};
    var oBas = oBase || {};
    var oOptions = mOptions || {};
    var sRootKey = resolveRootKey(oCur, oBas);
    var aChecks = [];
    var aBarriers = [];
    var aParticipants = [];
    var aAttachments = [];
    var oRootDelta = DeltaCore.diffFields(oCur.root || {}, oBas.root || {});
    var oBasicDelta = DeltaCore.diffFields(oCur.basic || {}, oBas.basic || {});
    var sRootEditMode = DeltaContracts.normalizeEditMode(
      oOptions.rootEditMode || ((oCur.root || {}).edit_mode) || (CreateSentinel.isCreateId(sRootKey) ? DeltaContracts.EDIT_MODE.CREATE : DeltaContracts.EDIT_MODE.UPDATE),
      DeltaContracts.EDIT_MODE.UPDATE
    );
    var oRootPayload = DeltaFieldMappers.mapRootFields(oRootDelta, oBasicDelta, sRootKey, sRootEditMode);

    appendChildChanges(aChecks, sRootKey, oCur.checks || [], oBas.checks || [], DeltaFieldMappers.toCheckFields);
    appendChildChanges(aBarriers, sRootKey, oCur.barriers || [], oBas.barriers || [], DeltaFieldMappers.toBarrierFields);
    appendChildChanges(aParticipants, sRootKey, oCur.participants || [], oBas.participants || [], DeltaFieldMappers.toParticipantFields);
    appendChildChanges(aAttachments, sRootKey, oCur.attachments || [], oBas.attachments || [], DeltaFieldMappers.toAttachmentFields);
    if (Object.keys(oRootPayload).length === 2 && !aChecks.length && !aBarriers.length && !aParticipants.length && !aAttachments.length) { return null; }

    return {
      root: oRootPayload,
      checks: aChecks,
      barriers: aBarriers,
      participants: aParticipants,
      attachments: aAttachments,
      client_version: resolveClientVersion(oCur, oBas),
      client_changed_on: formatODataDate(
        (((oCur || {}).meta || {}).aggChangedOn)
        || (((oCur || {}).root || {}).server_changed_on)
        || (((oBas || {}).meta || {}).aggChangedOn)
        || (((oBas || {}).root || {}).server_changed_on)
        || (((oCur || {}).root || {}).ChangedOn)
        || (((oBas || {}).root || {}).ChangedOn)
      )
    };
  }

  function buildCreatePayload(oCurrent) {
    var oCur = oCurrent || {};
    var sRootKey = resolveRootKey(oCur, {});
    return {
      root: DeltaFieldMappers.mapRootFields(oCur.root || {}, oCur.basic || {}, sRootKey, DeltaContracts.EDIT_MODE.CREATE),
      basic: Object.assign({}, oCur.basic || {}),
      checks: (oCur.checks || []).map(function (oRow, iIndex) {
        return DeltaFieldMappers.toCheckFields(oRow || {}, iIndex, DeltaContracts.EDIT_MODE.CREATE, sRootKey);
      }),
      barriers: (oCur.barriers || []).map(function (oRow, iIndex) {
        return DeltaFieldMappers.toBarrierFields(oRow || {}, iIndex, DeltaContracts.EDIT_MODE.CREATE, sRootKey);
      }),
      participants: (oCur.participants || []).map(function (oRow, iIndex) {
        return DeltaFieldMappers.toParticipantFields(oRow || {}, iIndex, DeltaContracts.EDIT_MODE.CREATE, sRootKey);
      }),
      attachments: (oCur.attachments || []).map(function (oRow, iIndex) {
        return DeltaFieldMappers.toAttachmentFields(oRow || {}, iIndex, DeltaContracts.EDIT_MODE.CREATE, sRootKey);
      }),
      client_version: 0,
      client_changed_on: null
    };
  }

  return { buildDeltaPayload: buildDeltaPayload, buildCreatePayload: buildCreatePayload };
});
