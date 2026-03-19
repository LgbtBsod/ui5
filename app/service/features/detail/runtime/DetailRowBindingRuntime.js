sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowEntityConfig",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (DetailRowEntityConfig, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SELECTED_MODEL = MODELS.SELECTED;

    function resolveConfig(vEntityOrSpec) {
        if (vEntityOrSpec && typeof vEntityOrSpec === "object" && vEntityOrSpec.rowsPath) {
            return vEntityOrSpec;
        }
        return DetailRowEntityConfig.get(vEntityOrSpec);
    }

    function resolveBindingContext(oControl, sModelName) {
        var oCursor = oControl;
        var oContext;
        while (oCursor) {
            if (typeof oCursor.getBindingContext === "function") {
                oContext = oCursor.getBindingContext(sModelName) || oCursor.getBindingContext();
                if (oContext && typeof oContext.getPath === "function" && oContext.getPath()) {
                    return oContext;
                }
            }
            oCursor = oCursor.getParent && oCursor.getParent();
        }
        return null;
    }

    function resolveSelectedBindingPath(oSource, sProperty) {
        var oBinding = oSource && oSource.getBinding && oSource.getBinding(sProperty);
        var oContext = resolveBindingContext(oSource, SELECTED_MODEL);
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

    function resolveSelectedRowContext(oSource) {
        var oContext = resolveBindingContext(oSource, SELECTED_MODEL);
        var sPath = String((oContext && oContext.getPath && oContext.getPath()) || "").trim();
        var oRow = oContext && oContext.getObject && oContext.getObject();
        return {
            rowPath: sPath,
            rowId: String((oRow && (oRow.client_row_id || oRow.Key || oRow.id)) || "").trim()
        };
    }

    function bindSelectedCollectionContext(oControl, vEntityOrSpec) {
        var oConfig = resolveConfig(vEntityOrSpec);
        var sRowsPath = String((oConfig && oConfig.rowsPath) || "").trim();
        var oCurrentContext = oControl && oControl.getBindingContext && oControl.getBindingContext(SELECTED_MODEL);
        var sCurrentPath = String((oCurrentContext && oCurrentContext.getPath && oCurrentContext.getPath()) || "").trim();
        if (!oControl || typeof oControl.bindElement !== "function" || !sRowsPath || sCurrentPath === sRowsPath) {
            return false;
        }
        oControl.bindElement({
            path: sRowsPath,
            model: SELECTED_MODEL
        });
        return true;
    }

    function formatRowNumber(oRow, vEntityOrSpec) {
        var oConfig = resolveConfig(vEntityOrSpec);
        var sField = String((oConfig && oConfig.numberField) || "").trim();
        var oData = oRow || {};
        return sField ? oData[sField] : "";
    }

    return {
        bindSelectedCollectionContext: bindSelectedCollectionContext,
        formatRowNumber: formatRowNumber,
        getConfig: resolveConfig,
        resolveRowsPath: function (vEntityOrSpec) {
            return resolveConfig(vEntityOrSpec).rowsPath;
        },
        resolveSelectedBindingPath: resolveSelectedBindingPath,
        resolveSelectedBindingContext: resolveBindingContext,
        resolveSelectedRowContext: resolveSelectedRowContext
    };
});
