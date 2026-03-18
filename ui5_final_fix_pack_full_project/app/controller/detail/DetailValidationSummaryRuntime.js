sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPersonInputRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/ValidationPathMap",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
], function (DetailPersonInputRuntime, StatePaths, FocusRuntime, ControllerViewStateRuntime, LayoutStateRuntime, ModelStateRuntime, SchedulingRuntime, ValidationPathMap, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var SELECTED_MODEL = MODELS.SELECTED;

    function validationSummaryPath(mStatePaths) {
        return (mStatePaths && mStatePaths.VALIDATION_SUMMARY) || "/validationSummary";
    }

    function isFilledValidationValue(vValue) {
        if (Array.isArray(vValue)) {
            return vValue.length > 0;
        }
        if (typeof vValue === "boolean") {
            return true;
        }
        return String(vValue == null ? "" : vValue).trim().length > 0;
    }

    function shouldTrackSelectedDirtyPath(sModelPath) {
        var sPath = "/" + String(sModelPath || "").replace(/^\//, "");
        if (sPath === "/") {
            return false;
        }
        if (/^\/attachments(?:\/|$)/.test(sPath)) {
            return false;
        }
        if (/^\/(?:root|meta)(?:\/|$)/.test(sPath)) {
            return false;
        }
        return /^\/(?:basic|checks|barriers)(?:\/|$)/.test(sPath);
    }

    function resolveFocusDomRef(oControl) {
        if (!oControl) {
            return null;
        }
        if (typeof oControl.getFocusDomRef === "function") {
            return oControl.getFocusDomRef() || null;
        }
        if (typeof oControl.getDomRef === "function") {
            return oControl.getDomRef() || null;
        }
        return null;
    }

    function scrollInvalidIntoView(oControl) {
        var oDomRef = resolveFocusDomRef(oControl);
        if (!oDomRef || typeof oDomRef.scrollIntoView !== "function") {
            return false;
        }
        try {
            oDomRef.scrollIntoView({ behavior: "smooth", block: "center", inline: "nearest" });
            return true;
        } catch (_e) {
            try {
                oDomRef.scrollIntoView(true);
                return true;
            } catch (_e2) {
                return false;
            }
        }
    }

    function compute(oController) {
        var oSelectedModel = ModelStateRuntime.model(oController, SELECTED_MODEL);
        var aRequired = ModelStateRuntime.read(oController, STATE_MODEL, "/requiredFields", []) || [];
        var mMissing = {};
        var aMissingPaths = [];
        var aMissingKeys = [];

        (Array.isArray(aRequired) ? aRequired : []).forEach(function (sRequiredPath) {
            var sPath = "/" + String(sRequiredPath || "").replace(/^\//, "");
            var sKey = ValidationPathMap.toValidationKey(sPath);
            var vCurrent = oSelectedModel ? ModelStateRuntime.read(oController, SELECTED_MODEL, sPath, undefined) : undefined;
            var bMissing = !isFilledValidationValue(vCurrent);
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
        ControllerViewStateRuntime.set(oController, "/validationMissing", oSummary.missingMap || {});
        if (bShowValidation || ControllerViewStateRuntime.get(oController, "/validationShown")) {
            ControllerViewStateRuntime.set(oController, "/validationShown", true);
        }
        ModelStateRuntime.write(oController, STATE_MODEL, validationSummaryPath(mStatePaths), {
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

    function focusFirstInvalidField(oController, mStatePaths) {
        var sSummaryPath = validationSummaryPath(mStatePaths);
        var oSummary = ModelStateRuntime.read(oController, STATE_MODEL, sSummaryPath, {}) || {};
        var aMissingKeys = (oSummary && oSummary.missingKeys) || [];
        var oView = oController.getView && oController.getView();
        var aControls;
        var oTarget;
        if (!oView || !Array.isArray(aMissingKeys) || !aMissingKeys.length) {
            return false;
        }
        aControls = oView.findAggregatedObjects(true, function (oControl) {
            return !!(oControl && oControl.data && oControl.data("validationKey"));
        });
        oTarget = aMissingKeys.reduce(function (oFound, sKey) {
            if (oFound) {
                return oFound;
            }
            return aControls.find(function (oControl) {
                return oControl && oControl.data && oControl.data("validationKey") === sKey;
            }) || null;
        }, null);
        if (!oTarget) {
            return false;
        }
        scrollInvalidIntoView(oTarget);
        if (typeof oTarget.focus === "function" && FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        var oDomRef = resolveFocusDomRef(oTarget);
        if (!oDomRef || typeof oDomRef.focus !== "function") {
            return false;
        }
        SchedulingRuntime.restartTimer(0, function () {
            oDomRef.focus();
        }, 0);
        return true;
    }

    function onSelectedChecklistChanged(oController, oEvent, mStatePaths) {
        var oSelectedModel = ModelStateRuntime.model(oController, SELECTED_MODEL);
        var sPath = oEvent && oEvent.getParameter && oEvent.getParameter("path");
        var aRequired = ModelStateRuntime.read(oController, STATE_MODEL, "/requiredFields", []) || [];
        var sValidationKey;
        var sRequiredPath;
        var sModelPath;
        var sMode;
        var vCurrent;

        if (!ModelStateRuntime.model(oController, VIEW_MODEL) || !oSelectedModel || !sPath) {
            return;
        }

        sModelPath = "/" + String(sPath || "").replace(/^\//, "");
        if (sPath === "/") {
            ControllerViewStateRuntime.set(oController, "/deleteChecklistConfirmArmed", false);
        }
        DetailPersonInputRuntime.syncDrafts(oController, oSelectedModel, sModelPath);

        sMode = LayoutStateRuntime.normalizeMode(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, ""), "");
        if (shouldTrackSelectedDirtyPath(sModelPath) && (sMode === "EDIT" || sMode === "CREATE")) {
            ModelStateRuntime.write(oController, STATE_MODEL, "/isDirty", true);
        }

        sRequiredPath = sModelPath;
        if (aRequired.indexOf(sRequiredPath) < 0) {
            recompute(oController, "selectedSync", false, mStatePaths);
            return;
        }

        sValidationKey = ValidationPathMap.toValidationKey(sRequiredPath);
        vCurrent = ModelStateRuntime.read(oController, SELECTED_MODEL, sRequiredPath, undefined);
        ControllerViewStateRuntime.set(oController, "/validationMissing/" + sValidationKey, !isFilledValidationValue(vCurrent));
        recompute(oController, "fieldChange", false, mStatePaths);
    }

    return {
        compute: compute,
        recompute: recompute,
        focusFirstInvalidField: focusFirstInvalidField,
        onSelectedChecklistChanged: onSelectedChecklistChanged
    };
});
