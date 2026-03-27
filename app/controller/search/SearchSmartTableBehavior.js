sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchActionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadingFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchMaxResults",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts"
], function (
    SearchActionBehavior,
    ControllerViewStateRuntime,
    ModelStateRuntime,
    SchedulingRuntime,
    StatePaths,
    SearchSelectionRuntime,
    SearchViewportRuntime,
    SearchLoadRuntime,
    SearchLoadingFeedbackRuntime,
    SearchReturnRediscoveryRuntime,
    SearchViewStateRuntime,
    SearchRateProgress,
    SearchToolbarContracts,
    SearchMaxResults,
    ModelContracts,
    OperationSourceContracts,
    SearchRuntimeContracts,
    JsRuntime,
    UiControlIds,
    ProgressiveReadinessContracts
) {
    "use strict";

    var SEARCH_MODE = SearchRuntimeContracts.SEARCH_MODE;
    var SEARCH_SEGMENTS = SearchRuntimeContracts.SEARCH_SEGMENTS;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;
    var SEARCH_CHECKS_FAIL_SEGMENT = StatePaths.SEARCH_CHECKS_FAIL_SEGMENT;
    var SEARCH_BARRIERS_FAIL_SEGMENT = StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT;
    var CLEAR_RESET_DELAY_MS = 0;
    var SEARCH_READINESS = ProgressiveReadinessContracts.SEARCH;
    var SEARCH_WORKING_HINT_MS = 2000;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function executeSearchCommand(oController, sMethod, mInput) {
        return SearchActionBehavior.runSearchCommand(oController, sMethod, mInput || {});
    }

    function normalizeRequestValue(sNormalizedValue, sFallbackValue) {
        var sSafeFallback = String(sFallbackValue || "").trim();
        return String(sNormalizedValue || "").trim() || sSafeFallback || SearchRuntimeContracts.DEFAULTS.SEARCH_BACKEND_TOP;
    }

    function normalizeOptionalRequestValue(sNormalizedValue) {
        return String(sNormalizedValue || "").trim();
    }

    function syncToolbarRequestInputs(oController) {
        var oBackendTopInput = oController.byId("backendTopInput");
        var oMaxRowsInput = oController.byId("maxRowsInput");
        var sBackendTop = SearchMaxResults.normalizeSearchBackendTopValue(oBackendTopInput && typeof oBackendTopInput[METHODS.GET_VALUE] === TYPE_FUNCTION && oBackendTopInput[METHODS.GET_VALUE]());
        var sMaxRows = SearchMaxResults.normalizeSearchMaxResultsValue(oMaxRowsInput && typeof oMaxRowsInput[METHODS.GET_VALUE] === TYPE_FUNCTION && oMaxRowsInput[METHODS.GET_VALUE]());
        sBackendTop = normalizeOptionalRequestValue(sBackendTop);
        sMaxRows = normalizeRequestValue(sMaxRows, ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_MAX_RESULTS, SearchRuntimeContracts.DEFAULTS.SEARCH_VISIBLE_ROWS));
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_BACKEND_TOP, sBackendTop);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_FETCH_LIMIT, sBackendTop);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_MAX_RESULTS, sMaxRows);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.GROWING_PAGE_SIZE, sMaxRows);
        if (oBackendTopInput && typeof oBackendTopInput[METHODS.SET_VALUE] === TYPE_FUNCTION) {
            oBackendTopInput[METHODS.SET_VALUE](sBackendTop);
        }
        if (oMaxRowsInput && typeof oMaxRowsInput[METHODS.SET_VALUE] === TYPE_FUNCTION) {
            oMaxRowsInput[METHODS.SET_VALUE](sMaxRows);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
    }

    function applyMaxRowsChange(oController, oEvent) {
        var sValue = SearchMaxResults.normalizeSearchMaxResultsValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        sValue = normalizeRequestValue(sValue, ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_MAX_RESULTS, SearchRuntimeContracts.DEFAULTS.SEARCH_VISIBLE_ROWS));
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_MAX_RESULTS, sValue);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.GROWING_PAGE_SIZE, sValue);
        if (oSource && typeof oSource[METHODS.SET_VALUE] === TYPE_FUNCTION) {
            oSource[METHODS.SET_VALUE](sValue);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
    }

    function applyBackendTopChange(oController, oEvent) {
        var sValue = SearchMaxResults.normalizeSearchBackendTopValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        var sCurrentValue;
        sValue = normalizeOptionalRequestValue(sValue);
        sCurrentValue = String(ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_BACKEND_TOP, SearchRuntimeContracts.DEFAULTS.SEARCH_BACKEND_TOP) || "").trim();
        if (sCurrentValue === sValue) {
            if (oSource && typeof oSource[METHODS.SET_VALUE] === TYPE_FUNCTION) {
                oSource[METHODS.SET_VALUE](sValue);
            }
            return false;
        }
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_BACKEND_TOP, sValue);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_FETCH_LIMIT, sValue);
        if (oSource && typeof oSource[METHODS.SET_VALUE] === TYPE_FUNCTION) {
            oSource[METHODS.SET_VALUE](sValue);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
        return true;
    }

    function deferCustomSegmentReset(oController) {
        if (typeof setTimeout === "function") {
            setTimeout(function () {
                resetCustomSegments(oController);
            }, CLEAR_RESET_DELAY_MS);
            return;
        }
        resetCustomSegments(oController);
    }

    function resetCustomSegments(oController) {
        var oChecksSegment;
        var oBarriersSegment;
        ModelStateRuntime.setMany(oController, STATE_MODEL, {});
        ModelStateRuntime.setMany(oController, STATE_MODEL, {
            [SEARCH_CHECKS_FAIL_SEGMENT]: SEARCH_SEGMENTS.ALL,
            [SEARCH_BARRIERS_FAIL_SEGMENT]: SEARCH_SEGMENTS.ALL
        });
        oChecksSegment = oController.byId("FailChecksSegmentControl");
        oBarriersSegment = oController.byId("FailBarriersSegmentControl");
        if (oChecksSegment && typeof oChecksSegment.setSelectedKey === "function") {
            oChecksSegment.setSelectedKey(SEARCH_SEGMENTS.ALL);
        }
        if (oBarriersSegment && typeof oBarriersSegment.setSelectedKey === "function") {
            oBarriersSegment.setSelectedKey(SEARCH_SEGMENTS.ALL);
        }
    }

    function bindClearHandler(oController) {
        var oSmartFilterBar;
        if (!oController || oController._bSearchClearHandlerBound) {
            return;
        }
        oSmartFilterBar = oController.byId(UiControlIds.SEARCH.SMART_FILTER_BAR);
        if (!oSmartFilterBar || typeof oSmartFilterBar.attachClear !== "function") {
            return;
        }
        oSmartFilterBar.attachClear(function () {
            deferCustomSegmentReset(oController);
        });
        oController._bSearchClearHandlerBound = true;
    }

    function clearSearchWorkingHintTimer(oController) {
        oController._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(oController._iSearchWorkingHintTimer);
    }

    function clearPendingSearchLoad(oController) {
        oController._iPendingSearchLoadTimer = SchedulingRuntime.clearTimer(oController._iPendingSearchLoadTimer);
        oController._oPendingSearchLoad = null;
    }

    function hideSearchWorkingHint(oController) {
        clearSearchWorkingHintTimer(oController);
        ControllerViewStateRuntime.set(oController, "/filterHintVisible", false);
        ControllerViewStateRuntime.set(oController, "/filterHintText", "");
    }

    function isSearchLoading(oController) {
        return !!(
            ControllerViewStateRuntime.get(oController, "/tableBusy", false)
            || ControllerViewStateRuntime.get(oController, "/searchActionBusy", false)
            || ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.UI_BUSY_SEARCH_TABLE, false)
        );
    }

    function scheduleSearchWorkingHint(oController) {
        clearSearchWorkingHintTimer(oController);
        oController._iSearchWorkingHintTimer = SchedulingRuntime.restartTimer(0, function () {
            var fnResolveWorkingText = oController && oController._resolveSearchWorkingText;
            if (!isSearchLoading(oController)) {
                return;
            }
            ControllerViewStateRuntime.set(oController, "/filterHintVisible", true);
            ControllerViewStateRuntime.set(oController, "/filterHintType", "Information");
            ControllerViewStateRuntime.set(
                oController,
                "/filterHintText",
                typeof fnResolveWorkingText === TYPE_FUNCTION
                    ? fnResolveWorkingText()
                    : "Working..."
            );
        }, SEARCH_WORKING_HINT_MS);
    }

    function beginSearchLoadingFeedback(oController) {
        scheduleSearchWorkingHint(oController);
    }

    function afterSearchLoadSuccess(oController, oInnerTable) {
        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
        SearchReturnRediscoveryRuntime.applyAfterSearchSuccess(oController, oInnerTable);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
    }

    function isSearchBindingSettled(oInnerTable) {
        var oBinding = oInnerTable && oInnerTable.getBinding && oInnerTable.getBinding("items");
        if (!oInnerTable || !oBinding) {
            return false;
        }
        if (typeof oInnerTable.getBusy === TYPE_FUNCTION && oInnerTable.getBusy()) {
            return false;
        }
        if (typeof oBinding.isPending === TYPE_FUNCTION && oBinding.isPending()) {
            return false;
        }
        if (oBinding.bPendingRequest || oBinding.bPendingRefresh) {
            return false;
        }
        if (typeof oBinding.isLengthFinal === TYPE_FUNCTION) {
            return !!oBinding.isLengthFinal();
        }
        return true;
    }

    function settlePendingSearchLoad(oController, oOptions, mHooks) {
        var oPending = oController._oPendingSearchLoad;
        var oInnerTable = oOptions && oOptions.innerTable;
        var oError = oOptions && oOptions.error;
        var sErrorMessage;
        var fnReadRows = mHooks && mHooks.readRows;
        var fnAfterSuccess = mHooks && mHooks.afterSuccess;
        var fnApplyLoadError = mHooks && mHooks.applyLoadError;
        var fnApplyLoadSuccess = mHooks && mHooks.applyLoadSuccess;
        if (!oPending || oPending.settled) {
            return;
        }
        oPending.settled = true;
        clearPendingSearchLoad(oController);
        hideSearchWorkingHint(oController);
        if (oError) {
            sErrorMessage = String((oError && (oError.message || oError.statusText)) || SEARCH_READINESS.LOAD_ERROR_MESSAGE).trim();
            if (typeof fnApplyLoadError === TYPE_FUNCTION) {
                fnApplyLoadError(sErrorMessage);
            }
            return;
        }
        if (typeof fnApplyLoadSuccess === TYPE_FUNCTION) {
            fnApplyLoadSuccess(typeof fnReadRows === TYPE_FUNCTION ? fnReadRows(oInnerTable) : []);
        }
        if (typeof fnAfterSuccess === TYPE_FUNCTION) {
            fnAfterSuccess(oInnerTable);
        }
    }

    function bindPendingSearchLoad(oController, oInnerTable, mHooks) {
        var iStartedAt = Date.now();
        var fnPoll;
        clearPendingSearchLoad(oController);
        oController._oPendingSearchLoad = { settled: false };
        if (!oInnerTable || typeof oInnerTable.attachEventOnce !== TYPE_FUNCTION) {
            return;
        }
        oInnerTable.attachEventOnce("updateFinished", function () {
            settlePendingSearchLoad(oController, { innerTable: oInnerTable }, mHooks);
        });
        fnPoll = function () {
            if (!oController._oPendingSearchLoad || oController._oPendingSearchLoad.settled) {
                return;
            }
            if (isSearchBindingSettled(oInnerTable) || (Date.now() - iStartedAt) >= 8000) {
                settlePendingSearchLoad(oController, { innerTable: oInnerTable }, mHooks);
                return;
            }
            oController._iPendingSearchLoadTimer = SchedulingRuntime.restartTimer(
                oController._iPendingSearchLoadTimer,
                fnPoll,
                250
            );
        };
        oController._iPendingSearchLoadTimer = SchedulingRuntime.restartTimer(0, fnPoll, 250);
    }

    function createSmartTableLoadHooks(oController, fnReadRows) {
        return {
            applyLoadError: function (sErrorMessage) {
                SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
            },
            applyLoadSuccess: function (aRows) {
                SearchLoadRuntime.applyLoadSuccess(oController, aRows);
            },
            afterSuccess: function (oInnerTable) {
                afterSearchLoadSuccess(oController, oInnerTable);
            },
            beginSearchLoadingFeedback: function () {
                beginSearchLoadingFeedback(oController);
            },
            bindPendingSearchLoad: function (oInnerTable) {
                bindPendingSearchLoad(oController, oInnerTable, {
                    applyLoadError: function (sErrorMessage) {
                        SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
                    },
                    applyLoadSuccess: function (aRows) {
                        SearchLoadRuntime.applyLoadSuccess(oController, aRows);
                    },
                    readRows: function () {
                        return fnReadRows(oInnerTable);
                    },
                    afterSuccess: function () {
                        afterSearchLoadSuccess(oController, oInnerTable);
                    }
                });
            },
            settlePendingSearchLoad: function (oInnerTable, oError) {
                settlePendingSearchLoad(oController, { innerTable: oInnerTable, error: oError }, {
                    applyLoadError: function (sErrorMessage) {
                        SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
                    },
                    applyLoadSuccess: function (aRows) {
                        SearchLoadRuntime.applyLoadSuccess(oController, aRows);
                    },
                    readRows: function () {
                        return fnReadRows(oInnerTable);
                    },
                    afterSuccess: function () {
                        afterSearchLoadSuccess(oController, oInnerTable);
                    }
                });
            }
        };
    }

    function onSmartTableInitialise(oController, fnReadRows) {
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);

        SearchViewStateRuntime.setSmartTableReady(oController, true);
        if (!oInnerTable) {
            return;
        }
        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, true);
        SearchSelectionRuntime.bindSearchTableRuntime(oController, oInnerTable, function () {
            SearchViewportRuntime.bindSearchViewportRuntime(oController);
            SearchViewportRuntime.scheduleSearchViewportSync(oController, false);
        });
        if (oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        if (oInnerTable.attachSelectionChange && typeof oController.onSearchTableSelectionChange === "function") {
            oInnerTable.attachSelectionChange(oController.onSearchTableSelectionChange, oController);
        }
        if (oInnerTable.attachItemPress && typeof oController.onSearchTableItemPress === "function") {
            oInnerTable.attachItemPress(oController.onSearchTableItemPress, oController);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
        SearchRateProgress.wireTable(oController, oInnerTable);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
        return executeSearchCommand(oController, SearchRuntimeContracts.COMMANDS.REBIND, { source: "smartTableInitialise" });
    }

    function onBeforeSmartTableRebind(oController, oEvent, fnReadRows) {
        syncToolbarRequestInputs(oController);
        var mLoadHooks = createSmartTableLoadHooks(oController, fnReadRows);
        var oBindingParams = oEvent && oEvent.getParameter && oEvent.getParameter("bindingParams");
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);

        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, true);
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
        SearchViewStateRuntime.setTableBusy(oController, true);
        mLoadHooks.beginSearchLoadingFeedback();
        SearchViewportRuntime.scheduleSearchViewportSync(oController, false);
        mLoadHooks.bindPendingSearchLoad(oInnerTable);

        return Promise.resolve(executeSearchCommand(oController, SearchRuntimeContracts.COMMANDS.APPLY_REBIND_POLICY, {
            source: "beforeRebind",
            bindingParams: oBindingParams || {},
            state: SearchViewStateRuntime.readStateData(oController),
            onDataReceived: function (oDataEvent) {
                var oError = oDataEvent && oDataEvent.getParameter
                    && (oDataEvent.getParameter("error") || oDataEvent.getParameter("data") && oDataEvent.getParameter("data").error);
                mLoadHooks.settlePendingSearchLoad(oInnerTable, oError);
            }
        })).catch(function (oError) {
            mLoadHooks.settlePendingSearchLoad(oInnerTable, oError);
            return Promise.reject(oError);
        });
    }

    return {
        onSmartFilterInitialise: function (oController, fnApplyAnalyticsDrilldownIntent) {
            ControllerViewStateRuntime.set(oController, "/smartFilterReady", true);
            if (oController && typeof oController._bindLocationSuggest === "function") {
                oController._bindLocationSuggest();
            }
            bindClearHandler(oController);
            deferCustomSegmentReset(oController);
            executeSearchCommand(oController, SearchRuntimeContracts.COMMANDS.BUILD_FILTER, { source: SEARCH_SOURCES.SMART_FILTER_INIT });
            fnApplyAnalyticsDrilldownIntent();
        },
        onSmartFilterChanged: function (oController) {
            var oSmartFilterBar = oController.byId(UiControlIds.SEARCH.SMART_FILTER_BAR);
            if (!oSmartFilterBar || (typeof oSmartFilterBar.isInitialised === "function" && !oSmartFilterBar.isInitialised())) {
                return;
            }
            if (oController && typeof oController._bindLocationSuggest === "function") {
                oController._bindLocationSuggest();
            }
            executeSearchCommand(oController, SearchRuntimeContracts.COMMANDS.BUILD_FILTER, { source: SEARCH_SOURCES.SMART_FILTER_CHANGED });
        },
        onSmartFilterClear: function (oController) {
            deferCustomSegmentReset(oController);
        },
        onMaxRowsChange: function (oController, oEvent) {
            applyMaxRowsChange(oController, oEvent);
        },
        onBackendTopChange: function (oController, oEvent) {
            if (applyBackendTopChange(oController, oEvent) &&
                ControllerViewStateRuntime.get(oController, "/hasSearched") &&
                ControllerViewStateRuntime.get(oController, "/smartTableReady")) {
                executeSearchCommand(oController, SearchRuntimeContracts.COMMANDS.REBIND, { source: SEARCH_SOURCES.BACKEND_TOP_CHANGE });
            }
        },
        onSearchModeToggle: function (oController, oEvent) {
            var bLoose = !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_MODE, bLoose ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT);
            executeSearchCommand(oController, SearchRuntimeContracts.COMMANDS.EXECUTE_SEARCH, {
                intent: SEARCH_SOURCES.SEARCH_MODE_TOGGLE,
                state: bLoose
            });
        },
        onBeforeSmartTableRebind: onBeforeSmartTableRebind,
        onSmartTableInitialise: onSmartTableInitialise
    };
});
