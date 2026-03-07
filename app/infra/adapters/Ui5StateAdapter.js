sap.ui.define([], function () {
    "use strict";

    function resolveModel(mModels, sModelName) {
        if (!mModels) {
            return null;
        }
        if (String(sModelName || "").trim() === "") {
            return mModels.default || null;
        }
        return mModels[sModelName] || null;
    }

    function withModel(mModels, sModelName, fnWork, vFallback) {
        var oModel = resolveModel(mModels, sModelName);
        if (!oModel || typeof fnWork !== "function") {
            return vFallback;
        }
        return fnWork(oModel);
    }

    function create(mModels) {
        var oModels = mModels || {};

        return {
            get: function (sModelName, sPath) {
                return withModel(oModels, sModelName, function (oModel) {
                    if (!oModel.getProperty) {
                        return undefined;
                    }
                    return oModel.getProperty(sPath);
                }, undefined);
            },

            set: function (sModelName, sPath, vValue) {
                withModel(oModels, sModelName, function (oModel) {
                    if (oModel.setProperty) {
                        oModel.setProperty(sPath, vValue);
                    }
                });
            },

            merge: function (sModelName, sPath, oPartial) {
                withModel(oModels, sModelName, function (oModel) {
                    var oCurrent;
                    if (!oModel.getProperty || !oModel.setProperty) {
                        return;
                    }
                    oCurrent = oModel.getProperty(sPath) || {};
                    oModel.setProperty(sPath, Object.assign({}, oCurrent, oPartial || {}));
                });
            }
        };
    }

    return {
        create: create
    };
});
