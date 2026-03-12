sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaDateCodec",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaFieldMappers",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaChildChanges"
], function (DeltaCore, DeltaDateCodec, DeltaFieldMappers, DeltaChildChanges) {
  "use strict";

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

  function buildDeltaPayload(oCurrent, oBase) {
    var oCur = oCurrent || {};
    var oBas = oBase || {};
    var sRootKey = resolveRootKey(oCur, oBas);
    var aChecks = [];
    var aBarriers = [];
    var oRootDelta = DeltaCore.diffFields(oCur.root || {}, oBas.root || {});
    var oBasicDelta = DeltaCore.diffFields(oCur.basic || {}, oBas.basic || {});
    var oRootPayload = DeltaFieldMappers.mapRootFields(oRootDelta, oBasicDelta, sRootKey);

    DeltaChildChanges.appendChildChanges(aChecks, "CHECK", sRootKey, oCur.checks || [], oBas.checks || [], DeltaFieldMappers.toCheckFields);
    DeltaChildChanges.appendChildChanges(aBarriers, "BARRIER", sRootKey, oCur.barriers || [], oBas.barriers || [], DeltaFieldMappers.toBarrierFields);
    if (Object.keys(oRootPayload).length === 1 && !aChecks.length && !aBarriers.length) { return null; }

    return {
      root: oRootPayload,
      checks: aChecks,
      barriers: aBarriers,
      client_version: resolveClientVersion(oCur, oBas),
      client_changed_on: DeltaDateCodec.formatODataDate(
        (((oCur || {}).meta || {}).aggChangedOn)
        || (((oCur || {}).root || {}).server_changed_on)
        || (((oBas || {}).meta || {}).aggChangedOn)
        || (((oBas || {}).root || {}).server_changed_on)
        || (((oCur || {}).root || {}).ChangedOn)
        || (((oBas || {}).root || {}).ChangedOn)
      )
    };
  }

  return { buildDeltaPayload: buildDeltaPayload };
});
