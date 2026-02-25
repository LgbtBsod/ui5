sap.ui.define([
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator"
], function (Filter, FilterOperator) {
    "use strict";

    function buildReason(sReason, sFallback) {
        return sReason || sFallback || "";
    }

    function isEnabled(oViewModel) {
        return !!(oViewModel && oViewModel.getProperty("/useSmartControls"));
    }

    function setEnabled(oViewModel, bEnabled, sError) {
        oViewModel.setProperty("/useSmartControls", !!bEnabled);
        oViewModel.setProperty("/smartControlError", sError || "");
    }

    function syncAvailability(mArgs) {
        var oStateModel = mArgs.stateModel;
        var oViewModel = mArgs.viewModel;
        var bMetadataOk = oStateModel.getProperty("/mainServiceMetadataOk");
        var sMetaError = oStateModel.getProperty("/mainServiceMetadataError") || "";
        var sUnavailableText = mArgs.unavailableText || "";
        var fnBootstrap = mArgs.bootstrap;

        if (bMetadataOk === false) {
            var sReason = buildReason(sMetaError, sUnavailableText);
            setEnabled(oViewModel, false, sReason);
            oViewModel.setProperty("/smartControlsReason", sReason);
            return;
        }

        oViewModel.setProperty("/smartControlsReason", "");

        if (!isEnabled(oViewModel)) {
            setEnabled(oViewModel, true, "");
            if (typeof fnBootstrap === "function") {
                fnBootstrap();
            }
        }
    }

    async function bootstrap(mArgs) {
        var oSmartTable = mArgs.smartTable;
        var fnOnReady = mArgs.onReady;
        var fnSetEnabled = mArgs.setEnabled;

        if (!oSmartTable) {
            fnSetEnabled(false, "SmartTable control not found");
            return;
        }

        try {
            if (oSmartTable.isInitialised && oSmartTable.isInitialised()) {
                fnOnReady();
                return;
            }

            await new Promise(function (resolve) {
                oSmartTable.attachInitialise(resolve);
            });
            fnOnReady();
        } catch (oError) {
            fnSetEnabled(false, (oError && oError.message) || "Smart controls init failed");
        }
    }

    function wireInnerTable(mArgs) {
        var oSmartTable = mArgs.smartTable;
        var fnSetEnabled = mArgs.setEnabled;
        var fnSelectionChange = mArgs.onSelectionChange;
        var fnItemPress = mArgs.onItemPress || fnSelectionChange;

        if (!oSmartTable || !oSmartTable.getTable) {
            fnSetEnabled(false, "SmartTable inner table unavailable");
            return null;
        }

        var oInnerTable = oSmartTable.getTable();
        if (!oInnerTable) {
            fnSetEnabled(false, "SmartTable inner table missing");
            return null;
        }

        if (oInnerTable.setMode) {
            oInnerTable.setMode("SingleSelectLeft");
        }
        if (oInnerTable.setGrowing) {
            oInnerTable.setGrowing(true);
            oInnerTable.setGrowingThreshold(100);
            oInnerTable.setGrowingScrollToLoad(false);
        }
        if (oInnerTable.attachItemPress) {
            oInnerTable.attachItemPress(fnItemPress);
        }
        if (oInnerTable.attachSelectionChange) {
            oInnerTable.attachSelectionChange(fnSelectionChange);
        }

        return oInnerTable;
    }

    function extractChecklistId(oObject) {
        if (!oObject) {
            return "";
        }
        return String(
            oObject.id
            || oObject.ID
            || oObject.ChecklistId
            || oObject.CHECKLIST_ID
            || (((oObject.root || {}).id) || "")
        ).trim();
    }

    function extractChecklistIdFromSelectionEvent(oEvent) {
        var oListItem = oEvent.getParameter("listItem") || oEvent.getParameter("item");
        var oCtx = oListItem ? oListItem.getBindingContext() : null;
        var oObj = oCtx ? oCtx.getObject() : null;
        return extractChecklistId(oObj);
    }

    function applyRebindParams(mArgs) {
        var oBindingParams = mArgs.bindingParams || {};
        var mState = mArgs.state || {};
        var fnDataReceived = mArgs.onDataReceived;
        var aFilters = oBindingParams.filters || [];

        var sFilterId = String(mState.filterId || "").trim();
        var sFilterLpc = mState.filterLpc || "";

        if (sFilterId) {
            aFilters.push(new Filter({
                filters: [
                    new Filter("id", FilterOperator.Contains, sFilterId),
                    new Filter("checklist_id", FilterOperator.Contains, sFilterId)
                ],
                and: false
            }));
        }
        if (sFilterLpc) {
            aFilters.push(new Filter("LPC_KEY", FilterOperator.EQ, sFilterLpc));
        }

        var sChecks = mState.filterFailedChecks || "ALL";
        var sBarriers = mState.filterFailedBarriers || "ALL";
        if (sChecks !== "ALL") {
            aFilters.push(new Filter("has_failed_checks", FilterOperator.EQ, sChecks === "TRUE"));
        }
        if (sBarriers !== "ALL") {
            aFilters.push(new Filter("has_failed_barriers", FilterOperator.EQ, sBarriers === "TRUE"));
        }

        var sMax = String(mState.searchMaxResults || "").trim();
        var iMax = sMax ? Math.max(1, Math.min(9999, Number(sMax) || 0)) : 0;
        oBindingParams.parameters = oBindingParams.parameters || {};
        if (iMax > 0) {
            oBindingParams.parameters.top = iMax;
        } else if (Object.prototype.hasOwnProperty.call(oBindingParams.parameters, "top")) {
            delete oBindingParams.parameters.top;
        }

        oBindingParams.filters = aFilters;

        var fnPrevDataReceived = (oBindingParams.events || {}).dataReceived;
        oBindingParams.events = oBindingParams.events || {};
        oBindingParams.events.dataReceived = function (oDataEvent) {
            if (typeof fnPrevDataReceived === "function") {
                fnPrevDataReceived(oDataEvent);
            }
            if (typeof fnDataReceived === "function") {
                fnDataReceived(oDataEvent);
            }
        };

        return oBindingParams;
    }

    function rebindOrFallback(mArgs) {
        if (!mArgs.enabled) {
            if (typeof mArgs.fallbackSearch === "function") {
                mArgs.fallbackSearch();
            }
            return;
        }

        var oSmartTable = mArgs.smartTable;
        if (oSmartTable && oSmartTable.rebindTable) {
            oSmartTable.rebindTable();
        }
    }

    return {
        isEnabled: isEnabled,
        setEnabled: setEnabled,
        syncAvailability: syncAvailability,
        bootstrap: bootstrap,
        wireInnerTable: wireInnerTable,
        extractChecklistId: extractChecklistId,
        extractChecklistIdFromSelectionEvent: extractChecklistIdFromSelectionEvent,
        applyRebindParams: applyRebindParams,
        rebindOrFallback: rebindOrFallback
    };
});
