sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (ModelStateRuntime) {
    "use strict";

    function normalizeRouteName(vRouteName) {
        return String(vRouteName || "").trim();
    }

    function normalizeId(vId) {
        return String(vId || "").trim();
    }

    function isDetailRoute(sRouteName) {
        return sRouteName === "detail" || sRouteName === "detailLayout";
    }

    function resolveCurrentRootId(oStateModel) {
        return normalizeId(
            ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "") ||
            ModelStateRuntime.readOnModel(oStateModel, "/selectedId", "")
        );
    }

    function resolveNextRouteIntent(oRouteEvent) {
        var mArgs = oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("arguments") || {};
        return {
            routeName: normalizeRouteName(oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("name")),
            routeArgs: mArgs,
            rootId: normalizeId(mArgs && mArgs.id)
        };
    }

    function isSameDetailTarget(oStateModel, oRouteEvent) {
        var oNextIntent = resolveNextRouteIntent(oRouteEvent);
        var sCurrentRootId = resolveCurrentRootId(oStateModel);
        return !!(sCurrentRootId && isDetailRoute(oNextIntent.routeName) && oNextIntent.rootId === sCurrentRootId);
    }

    function shouldGuardDetailNavigation(oStateModel, oRouteEvent) {
        var sCurrentRootId = resolveCurrentRootId(oStateModel);
        var oNextIntent = resolveNextRouteIntent(oRouteEvent);

        if (!sCurrentRootId) {
            return false;
        }
        if (isSameDetailTarget(oStateModel, oRouteEvent)) {
            return false;
        }
        if (!oNextIntent.routeName) {
            return false;
        }
        return true;
    }

    function shouldReleaseDetailLock(oStateModel, oRouteEvent) {
        var sMode = normalizeRouteName(ModelStateRuntime.readOnModel(oStateModel, "/mode", "READ")).toUpperCase();
        var sLockState = normalizeRouteName(ModelStateRuntime.readOnModel(oStateModel, "/lockOperationState", "IDLE")).toUpperCase();

        if (!shouldGuardDetailNavigation(oStateModel, oRouteEvent)) {
            return false;
        }
        return sMode === "EDIT" || sLockState === "LOCKED";
    }

    function syncDetailMeta(oStateModel, StatePaths) {
        var oReadiness = ModelStateRuntime.readOnModel(oStateModel, StatePaths.READINESS_DETAIL, {}) || {};
        var sMode = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ") || "READ").toUpperCase();
        var sLockState = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE") || "IDLE").toUpperCase();
        var sAutosaveState = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE") || "IDLE").toUpperCase();
        var sValidationSource = String((ModelStateRuntime.readOnModel(oStateModel, StatePaths.VALIDATION_SUMMARY, {}) || {}).source || "idle");
        var bDirty = !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
        var bPermissionKnown = !!oReadiness.permissionKnown;
        var sReadinessStatus = String(oReadiness.status || "idle").trim() || "idle";
        var bAllowed = bPermissionKnown && sReadinessStatus !== "denied" && sReadinessStatus !== "error";

        ModelStateRuntime.writeOnModel(oStateModel, StatePaths.DETAIL_META, {
            rootId: String(oReadiness.rootId || ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "") || "").trim(),
            readiness: {
                status: sReadinessStatus,
                ready: !!oReadiness.ready,
                readyAt: String(oReadiness.readyAt || ""),
                error: String(oReadiness.error || "")
            },
            mode: sMode,
            lock: {
                state: sLockState,
                known: !!oReadiness.lockKnown
            },
            dirty: bDirty,
            permission: {
                known: bPermissionKnown,
                allowed: bAllowed
            },
            save: {
                state: sAutosaveState,
                lastSavedAt: ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null)
            },
            validation: {
                state: sValidationSource || "idle"
            }
        });
    }

    function attachInitListeners(mOptions) {
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
        var FlowCoordinator = mOptions.flowCoordinator;
        var fnRunGuardedSave = mOptions.runGuardedSave;
        var fnQueuePendingNavigationIntent = mOptions.queuePendingNavigationIntent;
        var fnClearPendingNavigationIntent = mOptions.clearPendingNavigationIntent;
        var fnRevertPendingNavigationIntent = mOptions.revertPendingNavigationIntent;
        var fnResumePendingNavigationIntent = mOptions.resumePendingNavigationIntent;
        var fnRestorePendingNavigationIntent = mOptions.restorePendingNavigationIntent;
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

        function resetDetailNavigationState() {
            ModelStateRuntime.resetDetailWorkflowState(oComponent, {
                "/selectedId": "",
                "/activeObjectId": "",
                "/layout": "OneColumn"
            });
            ModelStateRuntime.resetDetailRuntimeData(oComponent);
        }

        oComponent._oStateLifecycleModel = oStateModel;
        oComponent._oSelectedLifecycleModel = oSelectedModel;
        oComponent._fnStateModelPropertyChange = function (oEvent) {
            var sPath = oEvent.getParameter("path") || "";
            var sModeValue;
            if (["/mode", "/isLoading", "/activeObjectId", StatePaths.SESSION_ID, StatePaths.UI_BUSY_DETAIL].indexOf(sPath) >= 0) {
                ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
            }
            if ([
                "/activeObjectId",
                StatePaths.READINESS_DETAIL,
                StatePaths.WORKFLOW_DETAIL_EDIT_MODE,
                StatePaths.WORKFLOW_DETAIL_LOCK_STATE,
                StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE,
                StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT,
                StatePaths.WORKFLOW_DIRTY,
                StatePaths.VALIDATION_SUMMARY
            ].indexOf(sPath) >= 0) {
                syncDetailMeta(oStateModel, StatePaths);
            }
            if (sPath === StatePaths.WORKFLOW_EDIT_MODE) {
                sModeValue = String(oEvent.getParameter("value") || "READ").toUpperCase();
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ") !== sModeValue) {
                    ModelStateRuntime.writeOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, sModeValue);
                }
            }
            if (sPath === "/mode") {
                fnEmitTelemetry("workflow.mode.changed", mOptions.telemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if (sPath === "/lockOperationState") {
                fnEmitTelemetry("lock.state.changed", mOptions.telemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if ([StatePaths.SAVE_IN_FLIGHT, StatePaths.WORKFLOW_DIRTY].indexOf(sPath) >= 0 &&
                !ModelStateRuntime.readOnModel(oStateModel, StatePaths.SAVE_IN_FLIGHT, false) &&
                !ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false) &&
                ModelStateRuntime.readOnModel(oStateModel, StatePaths.PENDING_NAVIGATION_INTENT, null)) {
                fnResumePendingNavigationIntent();
            }
            if (["/mode", "/lockOperationState", "/activeObjectId"].indexOf(sPath) >= 0) {
                var sCurrentRootId = String(ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "") || "").trim();
                var sCurrentMode = mOptions.layoutStateRuntime.readMode(oStateModel, "");
                var sCurrentLockState = mOptions.layoutStateRuntime.readLockState(oStateModel, "");
                if (sCurrentRootId && sCurrentMode === "EDIT" && sCurrentLockState === "LOCKED") {
                    ModelStateRuntime.writeOnModel(oStateModel, StatePaths.TAB_CONFLICT_STATE, { active: false, source: "", at: "" });
                    fnPublishTabSignal("LOCK_OWNED", { rootId: sCurrentRootId });
                } else if (sCurrentRootId && sPath === "/mode" && sCurrentMode !== "EDIT") {
                    fnPublishTabSignal("LOCK_RELEASED", { rootId: sCurrentRootId });
                }
            }
        };
        oComponent._fnSelectedModelPropertyChange = function () { return; };
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
        syncDetailMeta(oStateModel, StatePaths);
        oComponent._fnOnFullSave = function () {
            oComponent._oGcd.resetOnFullSave();
        };
        window.addEventListener("pcct:fullSave", oComponent._fnOnFullSave);
        oComponent.setModel(oLayoutModel, "layout");
        oComponent.setModel(oCacheModel, "cache");
        oComponent.setModel(oMasterDataModel, "masterData");
        oComponent.setModel(oEnvModel, "env");
        oComponent._fnBeforeUnload = function (oEvent) {
            var bHasUnsaved = ModelStateRuntime.readOnModel(oStateModel, "/mode", "") === "EDIT" &&
                ModelStateRuntime.readOnModel(oStateModel, "/isDirty", false);
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

        var oRouter = oComponent.getRouter();
        oComponent._oLifecycleRouter = oRouter;
        oComponent._fnBeforeRouteMatched = function (oEvent) {
            var sRouteName = String(oEvent.getParameter("name") || "").trim();
            if (ModelStateRuntime.readOnModel(oStateModel, "/navGuardBypass", false)) {
                ModelStateRuntime.writeOnModel(oStateModel, "/navGuardBypass", false);
                return;
            }
            if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.SAVE_IN_FLIGHT, false)) {
                oEvent.preventDefault();
                fnQueuePendingNavigationIntent(oEvent);
                if (typeof fnRevertPendingNavigationIntent === "function") {
                    fnRevertPendingNavigationIntent();
                }
                return;
            }
            if (ModelStateRuntime.readOnModel(oStateModel, "/isDirty", false) && shouldGuardDetailNavigation(oStateModel, oEvent)) {
                oEvent.preventDefault();
                fnQueuePendingNavigationIntent(oEvent);
                if (typeof fnRevertPendingNavigationIntent === "function") {
                    fnRevertPendingNavigationIntent();
                }
                FlowCoordinator.confirmUnsavedAndHandle({
                    getModel: oComponent.getModel.bind(oComponent),
                    getResourceBundle: function () {
                        return oComponent.getModel("i18n").getResourceBundle();
                    }
                }, function () {
                    return fnRunGuardedSave();
                }, {
                    onCancel: function () {
                        if (typeof fnRestorePendingNavigationIntent === "function") {
                            return fnRestorePendingNavigationIntent();
                        }
                        if (typeof fnClearPendingNavigationIntent === "function") {
                            return fnClearPendingNavigationIntent();
                        }
                        return null;
                    }
                }).then(function (sDecision) {
                    if (sDecision === "DISCARD") {
                        var oPending = ModelStateRuntime.readOnModel(oStateModel, StatePaths.PENDING_NAVIGATION_INTENT, {}) || {};
                        fnClearPendingNavigationIntent();
                        resetDetailNavigationState();
                        ModelStateRuntime.writeOnModel(oStateModel, "/navGuardBypass", true);
                        oComponent.getRouter().navTo(oPending.routeName || oEvent.getParameter("name"), oPending.routeArgs || oEvent.getParameter("arguments") || {}, false);
                        return;
                    }
                    if (sDecision === "SAVE" || sDecision === "NO_CHANGES") {
                        fnResumePendingNavigationIntent();
                        return;
                    }
                });
                return;
            }
            if (shouldReleaseDetailLock(oStateModel, oEvent)) {
                oEvent.preventDefault();
                fnQueuePendingNavigationIntent(oEvent);
                FlowCoordinator.releaseWithTrySave({
                    getModel: oComponent.getModel.bind(oComponent)
                }).finally(function () {
                    resetDetailNavigationState();
                    fnResumePendingNavigationIntent();
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
        attachInitListeners: attachInitListeners
    };
});
