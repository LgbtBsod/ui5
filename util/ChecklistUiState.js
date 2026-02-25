sap.ui.define([], function () {
  "use strict";

  function normalizeStatus(sStatus) {
    return String(sStatus || "").trim().toUpperCase();
  }

  function pathToValidationKey(sPath) {
    return String(sPath || "").replace(/^\/+/, "").replace(/\//g, ".");
  }

  function buildValidationMap(aMissingPaths) {
    return (aMissingPaths || []).reduce(function (mAcc, sPath) {
      mAcc[pathToValidationKey(sPath)] = true;
      return mAcc;
    }, {});
  }

  return {
    normalizeStatus: normalizeStatus,
    pathToValidationKey: pathToValidationKey,
    buildValidationMap: buildValidationMap,
    isSameStatus: function (sLeft, sRight) {
      return normalizeStatus(sLeft) === normalizeStatus(sRight);
    }
  };
});
