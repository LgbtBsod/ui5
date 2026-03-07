sap.ui.define([
    "checklist/app/controller/support/DetailSelectionSync",
    "checklist/app/controller/support/DetailPersonInputSupport",
    "checklist/app/service/framework/FocusRuntime",
    "checklist/app/controller/support/ControllerModelWriteSupport"
], function (DetailSelectionSync, DetailPersonInputSupport, FocusRuntime, ControllerModelWriteSupport) {
    "use strict";

    function validationSummaryPath(mStatePaths) {
        return (mStatePaths && mStatePaths.VALIDATION_SUMMARY) || "/validationSummary";
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
        var oSelectedModel = oController.getModel("selected");
        var aRequired = ControllerModelWriteSupport.get(oController, "state", "/requiredFields", []) || [];
        var mMissing = {};
        var aMissingPaths = [];
        var aMissingKeys = [];

        (Array.isArray(aRequired) ? aRequired : []).forEach(function (sRequiredPath) {
            var sPath = "/" + String(sRequiredPath || "").replace(/^\//, "");
            var sKey = DetailSelectionSync.toValidationKey(sPath);
            var vCurrent = oSelectedModel && oSelectedModel.getProperty ? oSelectedModel.getProperty(sPath) : undefined;
            var bMissing = !DetailSelectionSync.isFilledValidationValue(vCurrent);
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
        if (!oController.getModel("state") || !oController.getModel("view")) {
            return { hasErrors: false, missingPaths: [], missingKeys: [], firstMissingPath: "", firstMissingKey: "" };
        }
        oSummary = compute(oController);
        ControllerModelWriteSupport.set(oController, "view", "/validationMissing", oSummary.missingMap || {});
        if (bShowValidation || ControllerModelWriteSupport.get(oController, "view", "/validationShown")) {
            ControllerModelWriteSupport.set(oController, "view", "/validationShown", true);
        }
        ControllerModelWriteSupport.set(oController, "state", validationSummaryPath(mStatePaths), {
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
        var oSummary = oController.getModel("state") && oController.getModel("state").getProperty
            ? oController.getModel("state").getProperty(sSummaryPath) || {}
            : {};
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
        setTimeout(function () {
            oDomRef.focus();
        }, 0);
        return true;
    }

    function onSelectedChecklistChanged(oController, oEvent, mStatePaths) {
        var oSelectedModel = oController.getModel("selected");
        var oUiStateModel = oController.getModel("uiState");
        var sPath = oEvent && oEvent.getParameter && oEvent.getParameter("path");
        var aRequired = ControllerModelWriteSupport.get(oController, "state", "/requiredFields", []) || [];
        var sValidationKey;
        var sRequiredPath;
        var sModelPath;
        var sMode;
        var vCurrent;

        if (!oController.getModel("view") || !oSelectedModel || !sPath) {
            return;
        }

        sModelPath = "/" + String(sPath || "").replace(/^\//, "");
        if (sPath === "/") {
            ControllerModelWriteSupport.set(oController, "view", "/deleteChecklistConfirmArmed", false);
        }
        DetailPersonInputSupport.syncDrafts(oController, oSelectedModel, sModelPath);

        if (oUiStateModel) {
            if (sPath === "/") {
                ControllerModelWriteSupport.set(
                    oController,
                    "uiState",
                    "/_detailCurrent",
                    JSON.parse(JSON.stringify(oSelectedModel.getProperty("/") || {}))
                );
            } else {
                ControllerModelWriteSupport.set(
                    oController,
                    "uiState",
                    "/_detailCurrent" + sModelPath,
                    oSelectedModel.getProperty(sModelPath)
                );
            }
        }

        sMode = String(ControllerModelWriteSupport.get(oController, "state", "/mode", "") || "").toUpperCase();
        if (DetailSelectionSync.shouldTrackSelectedDirtyPath(sModelPath) && (sMode === "EDIT" || sMode === "CREATE")) {
            ControllerModelWriteSupport.set(oController, "state", "/isDirty", true);
        }

        sRequiredPath = sModelPath;
        if (aRequired.indexOf(sRequiredPath) < 0) {
            recompute(oController, "selectedSync", false, mStatePaths);
            return;
        }

        sValidationKey = DetailSelectionSync.toValidationKey(sRequiredPath);
        vCurrent = oSelectedModel.getProperty(sRequiredPath);
        ControllerModelWriteSupport.set(oController, "view", "/validationMissing/" + sValidationKey, !DetailSelectionSync.isFilledValidationValue(vCurrent));
        recompute(oController, "fieldChange", false, mStatePaths);
    }

    return {
        compute: compute,
        recompute: recompute,
        focusFirstInvalidField: focusFirstInvalidField,
        onSelectedChecklistChanged: onSelectedChecklistChanged
    };
});
