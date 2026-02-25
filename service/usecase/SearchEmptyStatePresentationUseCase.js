sap.ui.define([], function () {
    "use strict";

    function resolveEmptyStateKind(mArgs) {
        var bHasRows = !!(mArgs && mArgs.hasRows);
        var bLoadError = !!(mArgs && mArgs.loadError);
        var bUseSmartControls = !!(mArgs && mArgs.useSmartControls);

        if (bLoadError) {
            return "load_error";
        }
        if (bHasRows) {
            return "has_data";
        }
        if (!bUseSmartControls) {
            return "smart_degraded_empty";
        }
        return "default_empty";
    }

    function resolveNoDataText(mArgs) {
        var sKind = (mArgs && mArgs.kind) || "default_empty";
        var oBundle = mArgs && mArgs.bundle;
        var mKeyByKind = {
            default_empty: "noDataDefault",
            load_error: "noDataLoadError",
            smart_degraded_empty: "noDataDefault"
        };
        var sKey = mKeyByKind[sKind] || ((mArgs && mArgs.unknownFallbackKey) || "noDataDefault");

        if (oBundle && typeof oBundle.getText === "function") {
            try {
                return oBundle.getText(sKey);
            } catch (_e) {
                return "No data";
            }
        }
        return "No data";
    }

    function applyEmptyStatePresentation(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        var oDataModel = mArgs && mArgs.dataModel;

        if (!oViewModel || typeof oViewModel.setProperty !== "function") {
            return { ok: false, reason: "missing_view_model" };
        }
        if (!oDataModel || typeof oDataModel.getProperty !== "function") {
            return { ok: false, reason: "missing_data_model" };
        }

        var aRows = oDataModel.getProperty("/visibleCheckLists");
        var bHasRows = Array.isArray(aRows) && aRows.length > 0;
        var sKind = resolveEmptyStateKind({
            hasRows: bHasRows,
            loadError: oViewModel.getProperty("/loadError") || false,
            useSmartControls: oViewModel.getProperty("/useSmartControls")
        });
        var sText = resolveNoDataText({
            kind: sKind,
            bundle: mArgs && mArgs.bundle,
            unknownFallbackKey: mArgs && mArgs.unknownFallbackKey
        });

        oViewModel.setProperty("/emptyStateKind", sKind);
        oViewModel.setProperty("/noDataText", sText);

        var oTable = mArgs && mArgs.table;
        if (oTable) {
            if (typeof oTable.setNoDataText === "function") {
                oTable.setNoDataText(sText);
            } else if (typeof oTable.setNoData === "function") {
                oTable.setNoData(sText);
            }
        }

        return { ok: true, reason: "applied", kind: sKind, text: sText };
    }

    return {
        resolveEmptyStateKind: resolveEmptyStateKind,
        resolveNoDataText: resolveNoDataText,
        applyEmptyStatePresentation: applyEmptyStatePresentation
    };
});
