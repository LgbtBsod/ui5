sap.ui.define([], function () {
    "use strict";

    function bootstrap(oComponent, mDeps) {
        var sMainServiceUri = oComponent.getManifestEntry("/sap.app/dataSources/mainService/uri");
        var mModels = mDeps.ComponentModelInitRuntime.initializeModels(oComponent, mDeps);

        if (!sMainServiceUri) {
            throw new Error("Manifest-driven mainService dataSource is missing. Check sap.app/dataSources/mainService/uri in manifest.json.");
        }

        return {
            models: mModels,
            mainServiceModel: mDeps.ComponentMainServiceRuntime.createMainServiceModel(oComponent, mDeps, sMainServiceUri)
        };
    }

    return {
        bootstrap: bootstrap
    };
});
