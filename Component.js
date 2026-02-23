sap.ui.define([
  "sap/ui/core/UIComponent",
  "theme7/model/models"
], function (UIComponent, models) {
  "use strict";

  return UIComponent.extend("theme7.Component", {

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