sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailValidationHelperRuntime"
], function (ModelStateRuntime, ModelContracts, DetailValidationHelperRuntime) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_MODEL = MODELS.DETAIL;

    function compute(oController) {
        var oDetailModel = ModelStateRuntime.model(oController, DETAIL_MODEL);
        var aRequired = ModelStateRuntime.read(oController, STATE_MODEL, "/requiredFields", []) || [];
        var mMissing = {};
        var aMissingPaths = [];
        var aMissingKeys = [];

        (Array.isArray(aRequired) ? aRequired : []).forEach(function (sRequiredPath) {
            var sPath = DetailValidationHelperRuntime.normalizeRequiredPath(sRequiredPath);
            var sKey = DetailValidationHelperRuntime.toValidationKey(sPath);
            var vCurrent = oDetailModel ? ModelStateRuntime.read(oController, DETAIL_MODEL, DetailValidationHelperRuntime.toDetailModelPath(sPath), undefined) : undefined;
            var bMissing = !DetailValidationHelperRuntime.isFilledValidationValue(vCurrent);
            mMissing[sKey] = bMissing;
            if (bMissing) {
                aMissingPaths.push(sPath);
                aMissingKeys.push(sKey);
            }
        });

        return {
            hasErrors: aMissingKeys.length > 0,
            missingPaths: aMissingPaths,
            missingKeys: aMissingKeys,
            missingCount: aMissingKeys.length,
            firstMissingPath: aMissingPaths[0] || "",
            firstMissingKey: aMissingKeys[0] || "",
            missingMap: mMissing
        };
    }

    function recompute(oController, sSource, bShowValidation, mStatePaths) {
        var oSummary;
        if (!ModelStateRuntime.model(oController, STATE_MODEL) || !ModelStateRuntime.model(oController, VIEW_MODEL)) {
            return { hasErrors: false, missingPaths: [], missingKeys: [], firstMissingPath: "", firstMissingKey: "" };
        }
        oSummary = compute(oController);
        DetailValidationHelperRuntime.setValidationMissing(oController, oSummary.missingMap);
        DetailValidationHelperRuntime.showValidation(oController, bShowValidation);
        ModelStateRuntime.write(oController, STATE_MODEL, DetailValidationHelperRuntime.validationSummaryPath(mStatePaths), {
            hasErrors: !!oSummary.hasErrors,
            missingPaths: oSummary.missingPaths || [],
            missingKeys: oSummary.missingKeys || [],
            missingCount: Number(oSummary.missingCount || 0) || 0,
            source: String(sSource || "sync"),
            firstMissingPath: oSummary.firstMissingPath || "",
            firstMissingKey: oSummary.firstMissingKey || ""
        });
        return oSummary;
    }

    return {
        compute: compute,
        recompute: recompute
    };
});
