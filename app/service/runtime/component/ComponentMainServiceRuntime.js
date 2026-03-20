sap.ui.define([], function () {
    "use strict";

    function createMainServiceModel(oComponent, mDeps, sMainServiceUri) {
        var GatewayClient = mDeps.GatewayClient;
        var oMainServiceModel = oComponent && oComponent.getModel ? oComponent.getModel("mainService") : null;
        var sResolvedServiceUrl;

        if (!oMainServiceModel) {
            throw new Error("Manifest-owned mainService model is missing on the component");
        }

        sResolvedServiceUrl = String(
            (oMainServiceModel && oMainServiceModel.sServiceUrl)
            || sMainServiceUri
            || ""
        ).replace(/\/+$/, "");

        GatewayClient.setModel(oMainServiceModel, { serviceUrl: sResolvedServiceUrl || sMainServiceUri });

        return oMainServiceModel;
    }

    return {
        createMainServiceModel: createMainServiceModel
    };
});
