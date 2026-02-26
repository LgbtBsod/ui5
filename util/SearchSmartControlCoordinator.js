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


    function resolveMetadataAvailability(mArgs) {
        var bMetadataOk = mArgs && mArgs.metadataOk;
        var sMetaError = String((mArgs && mArgs.metadataError) || "").trim();
        var sUnavailableText = String((mArgs && mArgs.unavailableText) || "").trim();

        if (bMetadataOk === false) {
            return {
                enabled: false,
                reasonCode: sMetaError ? "metadata_error" : "metadata_unavailable",
                reasonText: buildReason(sMetaError, sUnavailableText)
            };
        }

        if (bMetadataOk === null || typeof bMetadataOk === "undefined") {
            return {
                enabled: true,
                reasonCode: "metadata_pending",
                reasonText: ""
            };
        }

        return {
            enabled: true,
            reasonCode: "metadata_ready",
            reasonText: ""
        };
    }

    function syncAvailability(mArgs) {
        var oStateModel = mArgs.stateModel;
        var oViewModel = mArgs.viewModel;
        var fnBootstrap = mArgs.bootstrap;

        var oAvailability = resolveMetadataAvailability({
            metadataOk: oStateModel.getProperty("/mainServiceMetadataOk"),
            metadataError: oStateModel.getProperty("/mainServiceMetadataError") || "",
            unavailableText: mArgs.unavailableText || ""
        });

        oViewModel.setProperty("/smartControlsReasonCode", oAvailability.reasonCode);

        if (!oAvailability.enabled) {
            setEnabled(oViewModel, false, oAvailability.reasonText);
            oViewModel.setProperty("/smartControlsReason", oAvailability.reasonText);
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


    
    function pickFilterValue(vValue) {
        if (typeof vValue === "string") {
            return vValue;
        }
        if (Array.isArray(vValue) && vValue.length) {
            return pickFilterValue(vValue[0]);
        }
        if (vValue && typeof vValue === "object") {
            if (typeof vValue.value !== "undefined") {
                return String(vValue.value || "");
            }
            if (typeof vValue.key !== "undefined") {
                return String(vValue.key || "");
            }
            if (Array.isArray(vValue.items) && vValue.items.length) {
                return pickFilterValue(vValue.items[0]);
            }
            if (Array.isArray(vValue.ranges) && vValue.ranges.length) {
                return String((vValue.ranges[0] || {}).value1 || "");
            }
        }
        return "";
    }

    function sanitizeFilter(oFilter) {
        if (!oFilter) {
            return null;
        }

        if (Array.isArray(oFilter.aFilters)) {
            var aChildren = oFilter.aFilters.map(sanitizeFilter).filter(Boolean);
            if (!aChildren.length) {
                return null;
            }
            if (aChildren.length === 1) {
                return aChildren[0];
            }
            return new Filter({
                filters: aChildren,
                and: oFilter.bAnd !== false
            });
        }

        if ((oFilter.oValue1 === "" || oFilter.oValue1 === null || typeof oFilter.oValue1 === "undefined")
            && oFilter.sOperator !== FilterOperator.BT) {
            return null;
        }

        return oFilter;
    }

    function sanitizeFilters(aFilters) {
        return (aFilters || []).map(sanitizeFilter).filter(Boolean);
    }

    function applyRebindParams(mArgs) {
        var oBindingParams = mArgs.bindingParams || {};
        var mState = mArgs.state || {};
        var fnDataReceived = mArgs.onDataReceived;
        var aFilters = sanitizeFilters(oBindingParams.filters || []);

        var sFilterId = String(mState.filterId || "").trim();
        var sFilterLpc = mState.filterLpc || "";
        var sSearchMode = mState.searchMode || "EXACT";

        var oIdFilter = null;
        if (sFilterId) {
            oIdFilter = new Filter({
                filters: [
                    new Filter("id", FilterOperator.Contains, sFilterId),
                    new Filter("checklist_id", FilterOperator.Contains, sFilterId)
                ],
                and: false
            });
        }

        var oLpcFilter = null;
        if (sFilterLpc) {
            oLpcFilter = new Filter({
                filters: [
                    new Filter("lpc", FilterOperator.EQ, sFilterLpc),
                    new Filter("LPC_KEY", FilterOperator.EQ, sFilterLpc)
                ],
                and: false
            });
        }

        var mSmartFilterData = mArgs.smartFilterData || {};
        var sStatus = pickFilterValue(mSmartFilterData.status || mSmartFilterData.STATUS || "").trim();
        var sEquipment = pickFilterValue(mSmartFilterData.equipment || mSmartFilterData.EQUIPMENT || "").trim();
        var sObserver = pickFilterValue(mSmartFilterData.observer_fullname || mSmartFilterData.OBSERVER_FULLNAME || "").trim();
        var sDate = pickFilterValue(mSmartFilterData.date || mSmartFilterData.DATE || "").trim();

        var sChecks = mState.filterFailedChecks || "ALL";
        var sBarriers = mState.filterFailedBarriers || "ALL";
        var aStatusFilters = [];
        if (sChecks !== "ALL") {
            aStatusFilters.push(new Filter("has_failed_checks", FilterOperator.EQ, sChecks === "TRUE"));
        }
        if (sBarriers !== "ALL") {
            aStatusFilters.push(new Filter("has_failed_barriers", FilterOperator.EQ, sBarriers === "TRUE"));
        }

        if (sStatus) {
            aFilters.push(new Filter("status", FilterOperator.Contains, sStatus));
        }
        if (sEquipment) {
            aFilters.push(new Filter("equipment", FilterOperator.Contains, sEquipment));
        }
        if (sObserver) {
            aFilters.push(new Filter("observer_fullname", FilterOperator.Contains, sObserver));
        }
        if (sDate) {
            aFilters.push(new Filter("date", FilterOperator.Contains, sDate));
        }

        if (sSearchMode === "LOOSE") {
            var aLooseParts = [];
            if (oIdFilter) {
                aLooseParts.push(oIdFilter);
            }
            if (oLpcFilter) {
                aLooseParts.push(oLpcFilter);
            }
            Array.prototype.push.apply(aLooseParts, aStatusFilters);
            if (aLooseParts.length) {
                aFilters.push(new Filter({
                    filters: aLooseParts,
                    and: false
                }));
            }
        } else {
            if (oIdFilter) {
                aFilters.push(oIdFilter);
            }
            if (oLpcFilter) {
                aFilters.push(oLpcFilter);
            }
            Array.prototype.push.apply(aFilters, aStatusFilters);
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
        buildReason: buildReason,
        resolveMetadataAvailability: resolveMetadataAvailability,
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
