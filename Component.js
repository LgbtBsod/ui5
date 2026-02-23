sap.ui.define([
  "sap/ui/core/UIComponent",
  "sap_ui5/model/models",
  "sap_ui5/model/mockData",
  "sap_ui5/model/referenceData"
], function (UIComponent, models, mockData, referenceData) {

  return UIComponent.extend("sap_ui5.Component", {

    metadata: { manifest: "json" },

    init: function () {
      UIComponent.prototype.init.apply(this, arguments);

      this.setModel(models.createDataModel(mockData), "dataModel");
      this.setModel(models.createStateModel(), "stateModel");
      this.setModel(models.createReferenceModel(referenceData), "referenceModel");

      this.getRouter().initialize();
    }

  });
});