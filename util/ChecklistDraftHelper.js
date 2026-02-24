sap.ui.define([], function () {
  "use strict";

  /**
   * Utility helper for draft/change calculations in checklist flows.
   * Kept framework-agnostic to reuse across multiple controllers.
   */
  function _clone(vData) {
    return JSON.parse(JSON.stringify(vData || {}));
  }

  function _stableStringify(vValue) {
    return JSON.stringify(vValue || {});
  }

  function hasChanges(oCurrent, oOriginal) {
    return _stableStringify(oCurrent) !== _stableStringify(oOriginal);
  }

  function buildDefaultChecklist(sId) {
    return {
      root: { id: sId || "" },
      basic: {
        date: "",
        timezone: "Europe/Amsterdam"
      },
      checks: [],
      barriers: []
    };
  }

  return {
    clone: _clone,
    hasChanges: hasChanges,
    buildDefaultChecklist: buildDefaultChecklist
  };
});
