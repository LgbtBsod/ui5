sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (ModelPathContracts, ModelStateRuntime, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var SELECTED_MODEL = MODELS.SELECTED;

    function resolveSelectedBindingPath(oSource, sProperty) {
        var oBinding = oSource && oSource.getBinding && oSource.getBinding(sProperty);
        var oContext = oSource && oSource.getBindingContext && (oSource.getBindingContext(SELECTED_MODEL) || oSource.getBindingContext());
        var sContextPath = String((oContext && oContext.getPath && oContext.getPath()) || "").trim();
        var sBindingPath = String((oBinding && oBinding.getPath && oBinding.getPath()) || "").trim();
        var sModelName = String((oBinding && oBinding.getModel && oBinding.getModel() && oBinding.getModel().sName) || "").trim();
        if (!sBindingPath || (sModelName && sModelName !== SELECTED_MODEL)) {
            return "";
        }
        if (sBindingPath.charAt(0) === "/") {
            return sBindingPath;
        }
        return (sContextPath ? sContextPath + "/" : "/") + sBindingPath;
    }

    function normalizeEventValue(oEvent, sParameterName, sPropertyName, oSource) {
        var vValue = oEvent && oEvent.getParameter && oEvent.getParameter(sParameterName);
        if (typeof vValue !== "undefined") {
            return vValue;
        }
        if (oSource && typeof oSource.getProperty === "function") {
            return oSource.getProperty(sPropertyName);
        }
        return undefined;
    }

    function applySelectedFieldChange(oController, oEvent, mOptions, mHooks) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        var sProperty = String((mOptions && mOptions.property) || "value").trim() || "value";
        var sParameter = String((mOptions && mOptions.parameter) || sProperty).trim() || sProperty;
        var sPath = resolveSelectedBindingPath(oSource, sProperty);
        var vValue;
        if (!oSource || !sPath || !mHooks.isDirtyTrackMode()) {
            return false;
        }
        vValue = normalizeEventValue(oEvent, sParameter, sProperty, oSource);
        if (typeof vValue === "undefined") {
            return false;
        }
        ModelStateRuntime.write(oController, SELECTED_MODEL, sPath, vValue);
        ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.IS_DIRTY, true);
        return true;
    }

    function resolveRowInput(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        var oCursor = oSource;
        var oCtx;
        var oRow;
        var sPath = "";
        while (oCursor) {
            if (oCursor.getBindingContext) {
                oCtx = oCursor.getBindingContext(SELECTED_MODEL) || oCursor.getBindingContext();
                if (oCtx && oCtx.getPath) {
                    sPath = String(oCtx.getPath() || "");
                    oRow = oCtx.getObject && oCtx.getObject();
                    if (sPath) {
                        break;
                    }
                }
            }
            oCursor = oCursor.getParent && oCursor.getParent();
        }
        return {
            rowPath: sPath,
            rowId: String((oRow && (oRow.client_row_id || oRow.Key || oRow.id)) || "").trim()
        };
    }

    return {
        applySelectedFieldChange: applySelectedFieldChange,
        resolveRowInput: resolveRowInput
    };
});
