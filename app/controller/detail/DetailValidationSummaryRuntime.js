sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPersonInputRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailValidationHelperRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (
    FocusRuntime,
    ControllerViewStateRuntime,
    LayoutStateRuntime,
    ModelStateRuntime,
    SchedulingRuntime,
    ModelContracts,
    StatePaths,
    DetailPersonInputRuntime,
    DetailValidationHelperRuntime,
    JsRuntime
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_MODEL = MODELS.DETAIL;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

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

    function resolveFocusDomRef(oControl) {
        if (!oControl) {
            return null;
        }
        if (typeof oControl[METHODS.GET_FOCUS_DOM_REF] === TYPE_FUNCTION) {
            return oControl[METHODS.GET_FOCUS_DOM_REF]() || null;
        }
        if (typeof oControl[METHODS.GET_DOM_REF] === TYPE_FUNCTION) {
            return oControl[METHODS.GET_DOM_REF]() || null;
        }
        return null;
    }

    function scrollInvalidIntoView(oControl) {
        var oDomRef = resolveFocusDomRef(oControl);
        if (!oDomRef || typeof oDomRef.scrollIntoView !== TYPE_FUNCTION) {
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

    function focusFirstInvalidField(oController, mStatePaths) {
        var sSummaryPath = DetailValidationHelperRuntime.validationSummaryPath(mStatePaths);
        var oSummary = ModelStateRuntime.read(oController, STATE_MODEL, sSummaryPath, {}) || {};
        var aMissingKeys = (oSummary && oSummary.missingKeys) || [];
        var oView = oController.getView && oController.getView();
        var aControls;
        var oTarget;
        var oDomRef;
        if (!oView || !Array.isArray(aMissingKeys) || !aMissingKeys.length) {
            return false;
        }
        aControls = oView.findAggregatedObjects(true, function (oControl) {
            return !!(oControl && typeof oControl.data === TYPE_FUNCTION && oControl.data("validationKey"));
        });
        oTarget = aMissingKeys.reduce(function (oFound, sKey) {
            if (oFound) {
                return oFound;
            }
            return aControls.find(function (oControl) {
                return oControl && typeof oControl.data === TYPE_FUNCTION && oControl.data("validationKey") === sKey;
            }) || null;
        }, null);
        if (!oTarget) {
            return false;
        }
        scrollInvalidIntoView(oTarget);
        if (typeof oTarget[METHODS.FOCUS] === TYPE_FUNCTION && FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        oDomRef = resolveFocusDomRef(oTarget);
        if (!oDomRef || typeof oDomRef[METHODS.FOCUS] !== TYPE_FUNCTION) {
            return false;
        }
        SchedulingRuntime.restartTimer(0, function () {
            oDomRef[METHODS.FOCUS]();
        }, 0);
        return true;
    }

    function onDetailModelChanged(oController, oEvent, mStatePaths) {
        var oDetailModel = ModelStateRuntime.model(oController, DETAIL_MODEL);
        var sPath = oEvent && oEvent.getParameter && oEvent.getParameter("path");
        var aRequired = ModelStateRuntime.read(oController, STATE_MODEL, "/requiredFields", []) || [];
        var sValidationKey;
        var sRequiredPath;
        var sModelPath;
        var sMode;
        var vCurrent;

        if (!ModelStateRuntime.model(oController, VIEW_MODEL) || !oDetailModel || !sPath) {
            return;
        }

        sModelPath = "/" + String(sPath || "").replace(/^\//, "");
        if (sPath === "/") {
            ControllerViewStateRuntime.set(oController, "/deleteChecklistConfirmArmed", false);
        }
        DetailPersonInputRuntime.syncDrafts(oController, oDetailModel, sModelPath);

        sMode = LayoutStateRuntime.normalizeMode(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, ""), "");
        sRequiredPath = DetailValidationHelperRuntime.normalizeRequiredPath(sModelPath);
        if (DetailValidationHelperRuntime.shouldTrackSelectedDirtyPath(sModelPath) && (sMode === "EDIT" || sMode === "CREATE") && aRequired.indexOf(sRequiredPath) < 0) {
            recompute(oController, "selectedSync", false, mStatePaths);
            return;
        }

        if (aRequired.indexOf(sRequiredPath) < 0) {
            return;
        }

        sValidationKey = DetailValidationHelperRuntime.toValidationKey(sRequiredPath);
        vCurrent = ModelStateRuntime.read(oController, DETAIL_MODEL, DetailValidationHelperRuntime.toDetailModelPath(sRequiredPath), undefined);
        DetailValidationHelperRuntime.setValidationMissingKey(oController, sValidationKey, !DetailValidationHelperRuntime.isFilledValidationValue(vCurrent));
        recompute(oController, "fieldChange", false, mStatePaths);
    }

    return {
        compute: compute,
        recompute: recompute,
        focusFirstInvalidField: focusFirstInvalidField,
        onDetailModelChanged: onDetailModelChanged
    };
});
