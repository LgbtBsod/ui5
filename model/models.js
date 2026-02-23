sap.ui.define([
  "sap/ui/model/json/JSONModel"
], function (JSONModel) {
  "use strict";

  return {

    createDataModel: function () {
      return new JSONModel({
        searchResults: [],
        object: {}
      });
    },

    createStateModel: function () {
      return new JSONModel({
        mode: "READ",
        isBusy: false,
        filterMode: "ALL"
      });
    },

    createLayoutModel: function () {
      return new JSONModel({
        tableBusy: false
      });
    }

  };
});