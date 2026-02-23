sap.ui.define([
  "sap/ui/core/UIComponent",
  "sap_ui5/model/models"
], function (UIComponent, models) {
  "use strict";

  return UIComponent.extend("sap_ui5.Component", {

    metadata: {
      manifest: "json"
    },

    init: function () {
      UIComponent.prototype.init.apply(this, arguments);

      this.setModel(models.createDataModel(), "dataModel");
      this.setModel(models.createStateModel(), "stateModel");
      this.setModel(models.createLayoutModel(), "layoutModel");

      this.getRouter().initialize();
    }
  });
});