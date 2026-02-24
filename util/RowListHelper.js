sap.ui.define([], function () {
  "use strict";

  function _normalizeRows(aRows) {
    return (aRows || []).map(function (oItem, iIndex) {
      oItem.no = iIndex + 1;
      oItem.selected = false;
      return oItem;
    });
  }

  return {
    addRow: function (aRows) {
      var aNext = (aRows || []).slice();
      aNext.push({
        no: aNext.length + 1,
        text: "",
        result: false,
        selected: false
      });
      return aNext;
    },

    removeSelectedRows: function (aRows) {
      var aFiltered = (aRows || []).filter(function (oItem) {
        return !(oItem && oItem.selected);
      });
      return _normalizeRows(aFiltered);
    },

    removeRowByIndex: function (aRows, iIndex) {
      var aNext = (aRows || []).slice();
      if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aNext.length) {
        return aNext;
      }

      aNext.splice(iIndex, 1);
      return _normalizeRows(aNext);
    }
  };
});
