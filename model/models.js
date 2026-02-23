sap.ui.define([
  "sap/ui/model/json/JSONModel"
], function (JSONModel) {
  "use strict";

  return {

    createDataModel: function () {
      return new JSONModel({
        searchResults: [
          { id: "1001", name: "Safety Inspection A", status: "Ready" },
          { id: "1002", name: "Quality Audit B", status: "In Progress" }
        ],
        object: {
          id: "1001",
          name: "Safety Inspection A",
          description: "Enterprise checklist object with design-grade wrapper experience.",
          checks: [
            { title: "Visual Check", status: "OK" },
            { title: "Barrier Check", status: "Warning" }
          ]
        }
      });
    },

    createStateModel: function () {
      return new JSONModel({
        mode: "READ",
        isBusy: false,
        selectedId: "1001"
      });
    },

    createLayoutModel: function () {
      return new JSONModel({
        tableBusy: false
      });
    }

  };
});
