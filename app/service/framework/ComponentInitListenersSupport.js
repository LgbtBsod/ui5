sap.ui.define([
    "checklist/app/service/framework/ActionContract",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/LayoutStateRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/TelemetryRuntime"
], function (ActionContract, FeedbackBannerRuntime, LayoutStateRuntime, ModelStateRuntime, TelemetryRuntime) {
    "use strict";
    function attach(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var oSelectedModel = mOptions.selectedModel;
        var oLayoutModel = mOptions.layoutModel;
        var oCacheModel = mOptions.cacheModel;
        var oMasterDataModel = mOptions.masterDataModel;
        var oEnvModel = mOptions.envModel;
        var StatePaths = mOptions.statePaths || {};
        var SmartSearchAdapter = mOptions.smartSearchAdapter;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var TimeConfigService = mOptions.timeConfigService;
        var FlowCoordinator = mOptions.flowCoordinator;
        var fnBundleText = mOptions.bundleText;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnClearGlobalBanner = mOptions.clearGlobalBanner;
        var fnHandleForceReadOnly = mOptions.handleForceReadOnly;
        var fnRunGuardedSave = mOptions.runGuardedSave;
        var fnQueuePendingNavigationIntent = mOptions.queuePendingNavigationIntent;
        var fnClearPendingNavigationIntent = mOptions.clearPendingNavigationIntent;
        var fnResumePendingNavigationIntent = mOptions.resumePendingNavigationIntent;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var fnPublishTabSignal = mOptions.publishTabSignal;

        function resetDetailAccessGuard() {
            ModelStateRuntime.writeOnModel(oStateModel, "/detailAccessGuard", {
                rootId: "",
                userId: "",
                canView: true,
                canEdit: false,
                canDelete: false,
                reasonCode: "AUTHORIZED",
                message: "",
                checkedAt: ""
            });
        }

        oComponent._oStateLifecycleModel = oStateModel;
        oComponent._oSelectedLifecycleModel = oSelectedModel;
        oComponent._fnStateModelPropertyChange = function (oEvent) {
            var sPath = oEvent.getParameter("path") || "";
            if (["/mode", "/isBusy", "/isLoading", "/activeObjectId", StatePaths.SESSION_ID].indexOf(sPath) >= 0) {
                ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
            }
            if (sPath === "/mode") {
                fnEmitTelemetry("workflow.mode.changed", TelemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if (sPath === "/lockOperationState") {
                fnEmitTelemetry("lock.state.changed", TelemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if ([StatePaths.SAVE_IN_FLIGHT, StatePaths.WORKFLOW_DIRTY].indexOf(sPath) >= 0
                && !oStateModel.getProperty(StatePaths.SAVE_IN_FLIGHT)
                && !oStateModel.getProperty(StatePaths.WORKFLOW_DIRTY)
                && oStateModel.getProperty(StatePaths.PENDING_NAVIGATION_INTENT)) {
                fnResumePendingNavigationIntent();
            }
            if (["/mode", "/lockOperationState", "/activeObjectId"].indexOf(sPath) >= 0) {
                var sCurrentRootId = String(oStateModel.getProperty("/activeObjectId") || "").trim();
                var sCurrentMode = LayoutStateRuntime.readMode(oStateModel, "");
                var sCurrentLockState = LayoutStateRuntime.readLockState(oStateModel, "");
                if (sCurrentRootId && sCurrentMode === "EDIT" && sCurrentLockState === "LOCKED") {
                    oStateModel.setProperty(StatePaths.TAB_CONFLICT_STATE, { active: false, source: "", at: "" });
                    fnPublishTabSignal("LOCK_OWNED", { rootId: sCurrentRootId });
                } else if (sCurrentRootId && sPath === "/mode" && sCurrentMode !== "EDIT") {
                    fnPublishTabSignal("LOCK_RELEASED", { rootId: sCurrentRootId });
                }
            }
        };
        oComponent._fnSelectedModelPropertyChange = function () {
            ComponentRuntimeSupport.syncDetailCurrentFromSelected(oSelectedModel, oUiStateModel);
        };
        oStateModel.attachPropertyChange(oComponent._fnStateModelPropertyChange, oComponent);
        oSelectedModel.attachPropertyChange(oComponent._fnSelectedModelPropertyChange, oComponent);
        oComponent._detachInitRuntimeListeners = function () {
            if (oComponent._oStateLifecycleModel && oComponent._fnStateModelPropertyChange) {
                oComponent._oStateLifecycleModel.detachPropertyChange(oComponent._fnStateModelPropertyChange, oComponent);
            }
            if (oComponent._oSelectedLifecycleModel && oComponent._fnSelectedModelPropertyChange) {
                oComponent._oSelectedLifecycleModel.detachPropertyChange(oComponent._fnSelectedModelPropertyChange, oComponent);
            }
            if (oComponent._fnBeforeUnload) {
                window.removeEventListener("beforeunload", oComponent._fnBeforeUnload);
            }
        };
        ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
        ComponentRuntimeSupport.syncDetailCurrentFromSelected(oSelectedModel, oUiStateModel);
        oComponent._fnOnFullSave = function () {
            oComponent._oGcd.resetOnFullSave();
        };
        window.addEventListener("pcct:fullSave", oComponent._fnOnFullSave);
        oComponent.setModel(oLayoutModel, "layout");
        oComponent.setModel(oCacheModel, "cache");
        oComponent.setModel(oMasterDataModel, "masterData");
        oComponent.setModel(oEnvModel, "env");
        oComponent._fnBeforeUnload = function (oEvent) {
            var bHasUnsaved = oStateModel.getProperty("/mode") === "EDIT" && oStateModel.getProperty("/isDirty");
            if (!bHasUnsaved) {
                return;
            }
            var sMsg = "You have unsaved changes";
            oEvent.preventDefault();
            oEvent.returnValue = sMsg;
            return sMsg;
        };
        window.addEventListener("beforeunload", oComponent._fnBeforeUnload);
        ModelStateRuntime.setManyOnModel(oLayoutModel, {
            "/smartFilter/fields": SmartSearchAdapter.getSmartFilterConfig().fields,
            "/smartTable/columns": SmartSearchAdapter.getSmartTableConfig().columns,
            "/smartTable/selectionMode": SmartSearchAdapter.getSmartTableConfig().selectionMode
        });
        oComponent._oDirtyStateBinding = oStateModel.bindProperty("/isDirty");
        oComponent._fnDirtyStateBindingChange = function () {
            oComponent._oAutoSave.touch();
        };
        oComponent._oDirtyStateBinding.attachChange(oComponent._fnDirtyStateBindingChange);
        oComponent._aLockScopedStateBindings = ["/lockOperationState", "/mode"].map(function (sPath) {
            var oBinding = oStateModel.bindProperty(sPath);
            var fnBindingChange = function () {
                oComponent._syncLockScopedManagers(oStateModel);
            };
            oBinding.attachChange(fnBindingChange);
            return {
                binding: oBinding,
                handler: fnBindingChange
            };
        });

        oComponent._oConnectivity.attachEvent("state", function (oEvent) {
            var m = oEvent.getParameters() || {};
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/networkOnline": !!m.online,
                "/networkGraceMode": !!m.isGrace,
                "/networkGraceExpiresAt": m.graceExpiresAt || null
            });
            if (!m.online) {
                fnSetGlobalBanner(FeedbackBannerRuntime.createNetworkRetryBannerInput(
                    ActionContract.RETRY_ACTIONS.SEARCH,
                    "searchRetryAction",
                    fnBundleText("retryLaterHint")
                ));
                return;
            }
            var oBanner = FeedbackBannerRuntime.getBanner(oStateModel, "global");
            if (oBanner.retryAction === ActionContract.RETRY_ACTIONS.SEARCH) {
                fnClearGlobalBanner();
            }
        });
        oComponent._oConnectivity.attachEvent("graceExpired", function () {
            fnHandleForceReadOnly({
                reason: "NETWORK_GRACE_EXPIRED",
                messageKey: "networkGraceExpired",
                source: "connectivity"
            });
        });
        var oRouter = oComponent.getRouter();
        oComponent._oLifecycleRouter = oRouter;
        oComponent._fnBeforeRouteMatched = function (oEvent) {
            var sRouteName = String(oEvent.getParameter("name") || "").trim();
            if (oStateModel.getProperty("/navGuardBypass")) {
                oStateModel.setProperty("/navGuardBypass", false);
                return;
            }
            if (oStateModel.getProperty(StatePaths.SAVE_IN_FLIGHT)) {
                oEvent.preventDefault();
                fnQueuePendingNavigationIntent(oEvent);
                return;
            }
            if (oStateModel.getProperty("/isDirty")) {
                oEvent.preventDefault();
                fnQueuePendingNavigationIntent(oEvent);
                FlowCoordinator.confirmUnsavedAndHandle({
                    getModel: oComponent.getModel.bind(oComponent),
                    getResourceBundle: function () { return oComponent.getModel("i18n").getResourceBundle(); }
                }, function () {
                    return fnRunGuardedSave();
                }).then(function (sDecision) {
                    if (sDecision === "DISCARD") {
                        var oPending = oStateModel.getProperty(StatePaths.PENDING_NAVIGATION_INTENT) || {};
                        fnClearPendingNavigationIntent();
                        oStateModel.setProperty("/navGuardBypass", true);
                        oComponent.getRouter().navTo(oPending.routeName || oEvent.getParameter("name"), oPending.routeArgs || oEvent.getParameter("arguments") || {}, false);
                        return;
                    }
                    if (sDecision === "SAVE" || sDecision === "NO_CHANGES") {
                        fnResumePendingNavigationIntent();
                        return;
                    }
                    if (sDecision === "CANCEL") {
                        fnClearPendingNavigationIntent();
                    }
                });
                return;
            }
            if (sRouteName !== "accessDenied") {
                resetDetailAccessGuard();
            }
        };
        oRouter.attachBeforeRouteMatched(oComponent._fnBeforeRouteMatched, oComponent);
        oRouter.initialize();
    }
    return {
        attach: attach
    };
});
