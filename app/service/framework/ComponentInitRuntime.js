sap.ui.define([
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/EffectTextResolver",
    "checklist/app/util/CloneUtil"
], function (ModelStateRuntime, FeedbackBannerRuntime, NavigationIntentService, EffectTextResolver, CloneUtil) {
    "use strict";

    function reuseJsonModel(oExistingModel, fnCreateModel) {
        var oModel = oExistingModel || fnCreateModel();
        var oSeedModel;

        if (oExistingModel && typeof oExistingModel.setData === "function") {
            oSeedModel = fnCreateModel();
            oExistingModel.setData(oSeedModel && oSeedModel.getData ? oSeedModel.getData() : {}, false);
        }

        return oModel;
    }

    function buildActionValidators(ActionContract) {
        var mValidators = {};
        var mActions = (ActionContract && ActionContract.ACTIONS) || {};
        var fnNormalize = ActionContract && ActionContract.normalizeActionPayload;

        if (typeof fnNormalize !== "function") {
            return mValidators;
        }

        [
            mActions.DETAIL_RETRY_GUARDED_SAVE,
            mActions.DETAIL_TAKEOVER_LOCK,
            mActions.DETAIL_CANCEL_ENTER_EDIT
        ].forEach(function (sAction) {
            if (!sAction) {
                return;
            }
            mValidators[sAction] = function (mPayload) {
                return fnNormalize(sAction, mPayload);
            };
        });

        return mValidators;
    }

    function registerDefaultHandlers(mOptions) {
        var oActionDispatcher = mOptions.actionDispatcher;
        var oActionContract = mOptions.actionContract || {};
        var oDetailFacade = mOptions.detailFacade;
        var fnRunGuardedSave = mOptions.runGuardedSave;
        var fnBuildLatestCtx = mOptions.buildLatestCtx;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnGetCtx = mOptions.getCtx;
        var mActions = oActionContract.ACTIONS || {};

        if (!oActionDispatcher || typeof oActionDispatcher.register !== "function") {
            return;
        }

        if (mActions.DETAIL_RETRY_GUARDED_SAVE) {
            oActionDispatcher.register(mActions.DETAIL_RETRY_GUARDED_SAVE, function () {
                return fnRunGuardedSave();
            });
        }
        if (mActions.DETAIL_TAKEOVER_LOCK) {
            oActionDispatcher.register(mActions.DETAIL_TAKEOVER_LOCK, function (mPayload) {
                return oDetailFacade.confirmTakeover(mPayload || {}, fnBuildLatestCtx()).then(fnApplyFacadeResult);
            });
        }
        if (mActions.DETAIL_CANCEL_ENTER_EDIT) {
            oActionDispatcher.register(mActions.DETAIL_CANCEL_ENTER_EDIT, function (mPayload) {
                return oDetailFacade.cancelEnterEdit(mPayload || {}, fnGetCtx()).then(fnApplyFacadeResult);
            });
        }
    }

    function resolveCorrelationId(oError, FeedbackPolicy) {
        var oNormalizedError = FeedbackPolicy && FeedbackPolicy.normalize ? FeedbackPolicy.normalize(oError || {}) : null;
        var oParams = oNormalizedError && oNormalizedError.params;
        return String(
            (oParams && (oParams.correlationId || oParams.correlation_id || oParams.requestId || oParams.request_id)) ||
            (oError && (oError.correlationId || oError.correlation_id || oError.requestId || oError.request_id)) ||
            ""
        ).trim();
    }

    function isSessionExpiredError(oError) {
        var iStatus = Number((oError && (oError.statusCode || oError.status)) || 0);
        var sCode = String((oError && oError.code) || "").toUpperCase();
        var sMessage = String((oError && oError.message) || "").toUpperCase();
        if (iStatus === 401 || iStatus === 403) {
            return true;
        }
        return sCode === "SESSION_UNAVAILABLE" || sCode === "AUTH_REQUIRED" || /SESSION|AUTH|CSRF/.test(sMessage);
    }

    function createFeedbackRuntime(oOptions) {
        var oStateModel = oOptions.stateModel;
        var FeedbackPolicy = oOptions.feedbackPolicy;
        var fnBundleText = oOptions.bundleText || function (sKey) {
            return sKey;
        };

        function setGlobalBanner(mBannerInput) {
            var mInput = mBannerInput || {};
            FeedbackBannerRuntime.setBanner(oStateModel, "global", mInput, {
                resolveText: fnBundleText
            });
        }

        function clearGlobalBanner() {
            FeedbackBannerRuntime.clearBanner(oStateModel, "global");
        }

        return {
            resolveCorrelationId: function (oError) {
                return resolveCorrelationId(oError, FeedbackPolicy);
            },
            isSessionExpiredError: isSessionExpiredError,
            setGlobalBanner: setGlobalBanner,
            clearGlobalBanner: clearGlobalBanner
        };
    }

    function createBundleText(component) {
        return function (sKey, aArgs) {
            return EffectTextResolver.getText(component, sKey, aArgs || [], sKey);
        };
    }

    function createApplyFacadeResult(mOptions) {
        var component = mOptions.component;
        var effectApplier = mOptions.effectApplier;
        var actionDispatcher = mOptions.actionDispatcher;
        var selectedModel = mOptions.selectedModel;
        var uiStateModel = mOptions.uiStateModel;
        var componentRuntimeSupport = mOptions.componentRuntimeSupport;
        var resolveBundleText = createBundleText(component);

        return function (oResult) {
            effectApplier.applyEffects(component, oResult && oResult.effects, {
                resolveTextKey: function (sKey) {
                    return resolveBundleText(sKey, []);
                },
                actionDispatcher: actionDispatcher
            });
            componentRuntimeSupport.syncDetailCurrentFromSelected(selectedModel, uiStateModel);
        };
    }

    function queuePendingNavigationIntent(oStateModel, StatePaths, oRouteEvent) {
        NavigationIntentService.queuePendingIntent(oStateModel, StatePaths, oRouteEvent);
    }

    function clearPendingNavigationIntent(oStateModel, StatePaths) {
        NavigationIntentService.clearPendingIntent(oStateModel, StatePaths);
    }

    function resumePendingNavigationIntent(component, oStateModel, StatePaths) {
        return NavigationIntentService.resumePendingIntent(component, oStateModel, StatePaths);
    }

    function runBootSequence(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oEnvModel = mOptions.envModel;
        var oCacheModel = mOptions.cacheModel;
        var BootstrapAppUseCase = mOptions.bootstrapAppUseCase;
        var EnsureDictLoadedUseCase = mOptions.ensureDictLoadedUseCase;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var fnLoadRuntimeSettings = mOptions.loadRuntimeSettings;
        var fnLoadCurrentUser = mOptions.loadCurrentUser;
        var fnBundleText = mOptions.bundleText;

        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/isLoading": true,
            "/masterDataLoading": true,
            "/locationsLoading": false
        });

        return BootstrapAppUseCase.execute({}, { stateModel: oStateModel }).then(function () {
            var oServerState = null;
            ComponentRuntimeSupport.ensureSessionId(oStateModel);
            ModelStateRuntime.writeOnModel(oStateModel, "/currentUser", {
                uname: "",
                fullName: "",
                permissions: [],
                permissionRules: [],
                canView: false,
                canEdit: false,
                canDelete: false,
                summaryText: "",
                fetchedAt: ""
            });
            var aRequired = [];
            var mVars = {};
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/requiredFields": aRequired,
                "/frontendVariables": mVars,
                "/frontendConfigSource": "gateway"
            });
            ModelStateRuntime.writeOnModel(oEnvModel, "/variables", mVars);
            Promise.resolve().then(function () {
                // allSettled polyfill: wraps each promise so none can reject the outer Promise.all
                function allSettledPolyfill(aPromises) {
                    return Promise.all((aPromises || []).map(function (p) {
                        return Promise.resolve(p).then(
                            function (v) { return { status: "fulfilled", value: v }; },
                            function (e) { return { status: "rejected", reason: e }; }
                        );
                    }));
                }
                return allSettledPolyfill([
                    Promise.resolve(typeof fnLoadCurrentUser === "function" ? fnLoadCurrentUser() : null),
                    fnLoadRuntimeSettings(),
                    Promise.resolve(EnsureDictLoadedUseCase.execute({}, oComponent._ctx)).catch(function () {
                        return null;
                    })
                ]);
            }).then(function (aResults) {
                var aCheckLists = [];
                ModelStateRuntime.writeOnModel(oCacheModel, "/pristineSnapshot", CloneUtil.clone(aCheckLists, []));
                var sCacheAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
                ModelStateRuntime.setManyOnModel(oCacheModel, {
                    "/lastServerState": oServerState || {
                        fetchedAt: sCacheAt,
                        count: aCheckLists.length
                    },
                    "/keyMapping": {}
                });
                ModelStateRuntime.writeOnModel(oStateModel, "/cacheValidationAt", sCacheAt);
            }).catch(function (oError) {
                ModelStateRuntime.setManyOnModel(oStateModel, {
                    "/loadError": true,
                    "/loadErrorMessage": fnBundleText("loadErrorMessage") + ": " + oError.message
                });
            });
        }).catch(function (oError) {
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/loadError": true,
                "/loadErrorMessage": fnBundleText("loadErrorMessage") + ": " + oError.message
            });
        }).finally(function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/isLoading", false);
            oComponent._startCoreManagers();
            oComponent._syncLockScopedManagers(oStateModel);
        });
    }

    function attachCrossTabRuntime(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oStatePaths = mOptions.statePaths || {};
        var fnBundleText = mOptions.bundleText;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnHandleForceReadOnly = mOptions.handleForceReadOnly;
        var sThisTabId = buildTabId();
        var fnPublishTabSignal;
        var fnHandleTabSignal;
        var STORAGE_KEY = "pcct_lock_signal";
        var CHANNEL_NAME = "pcct_lock_channel";

        fnPublishTabSignal = function (sType, mPayload) {
            var oSignal = Object.assign({}, mPayload || {}, {
                type: sType,
                tabId: sThisTabId,
                at: new Date().toISOString()
            });
            if (oComponent._oCrossTabChannel && typeof oComponent._oCrossTabChannel.postMessage === "function") {
                oComponent._oCrossTabChannel.postMessage(oSignal);
            }
            try {
                window.localStorage.setItem(STORAGE_KEY, JSON.stringify(oSignal));
            } catch (_e) {
                // no-op: storage signal is best-effort.
            }
        };

        fnHandleTabSignal = function (oSignal) {
            var oPayload = oSignal || {};
            var sSignalType = String(oPayload.type || "").toUpperCase();
            var sSignalRootId = String(oPayload.rootId || "").trim();
            var sCurrentRootId = String(ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "") || "").trim();
            var sMode = String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_EDIT_MODE, "") || "").toUpperCase();
            var sLockState = String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_LOCK_STATUS, "") || "").toUpperCase();
            if (!sSignalType || oPayload.tabId === sThisTabId || !sSignalRootId || !sCurrentRootId || sSignalRootId !== sCurrentRootId) {
                return;
            }
            if (sSignalType !== "LOCK_OWNED" || sMode !== "EDIT" || sLockState !== "LOCKED") {
                return;
            }
            ModelStateRuntime.writeOnModel(oStateModel, oStatePaths.TAB_CONFLICT_STATE, {
                active: true,
                source: "cross_tab",
                at: new Date().toISOString()
            });
            fnSetGlobalBanner(FeedbackBannerRuntime.createBannerInput({
                severity: "warning",
                textKey: "tabConflictBanner",
                details: fnBundleText("tabConflictCopyHint")
            }));
            fnHandleForceReadOnly({
                reason: "TAB_CONFLICT",
                messageKey: "tabConflictBanner",
                source: "crossTab"
            });
        };

        if (typeof window !== "undefined" && typeof window.BroadcastChannel === "function") {
            oComponent._oCrossTabChannel = new window.BroadcastChannel(CHANNEL_NAME);
            oComponent._oCrossTabChannel.onmessage = function (oEvent) {
                fnHandleTabSignal((oEvent && oEvent.data) || {});
            };
        }

        oComponent._fnCrossTabStorage = function (oStorageEvent) {
            if (!oStorageEvent || oStorageEvent.key !== STORAGE_KEY || !oStorageEvent.newValue) {
                return;
            }
            try {
                fnHandleTabSignal(JSON.parse(oStorageEvent.newValue));
            } catch (_e) {
                // no-op
            }
        };
        window.addEventListener("storage", oComponent._fnCrossTabStorage);

        return {
            publishTabSignal: fnPublishTabSignal,
            tabId: sThisTabId
        };
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
                return;
            }
            if (ModelStateRuntime.readOnModel(oStateModel, "/isDirty", false)) {
                oEvent.preventDefault();
                fnQueuePendingNavigationIntent(oEvent);
                FlowCoordinator.confirmUnsavedAndHandle({
                    getModel: oComponent.getModel.bind(oComponent),
                    getResourceBundle: function () {
                        return oComponent.getModel("i18n").getResourceBundle();
                    }
                }, function () {
                    return fnRunGuardedSave();
                }).then(function (sDecision) {
                    if (sDecision === "DISCARD") {
                        var oPending = ModelStateRuntime.readOnModel(oStateModel, StatePaths.PENDING_NAVIGATION_INTENT, {}) || {};
                        fnClearPendingNavigationIntent();
                        ModelStateRuntime.writeOnModel(oStateModel, "/navGuardBypass", true);
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

    function attachLockRuntime(mOptions) {
        var oComponent = mOptions.component;
        var oMainServiceModel = mOptions.mainServiceModel;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var oCacheModel = mOptions.cacheModel;
        var oStatePaths = mOptions.statePaths || {};
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var TimeConfigService = mOptions.timeConfigService;
        var DebugLogger = mOptions.debugLogger;
        var fnBundleText = mOptions.bundleText;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnHandleForceReadOnly = mOptions.handleForceReadOnly;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;

        oComponent._handleKilledLock = function (oPayload) {
            var bHadUnsavedChanges = !!ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_DIRTY, false);
            oComponent._oHeartbeat.stop();
            oComponent._oLockStatus.stop();
            oComponent._oAutoSave.stop();
            oComponent._oGcd.destroyManager();
            if (bHadUnsavedChanges) {
                fnSetGlobalBanner(FeedbackBannerRuntime.createBannerInput({
                    severity: "warning",
                    textKey: "lockLostMessage",
                    details: fnBundleText("tabConflictCopyHint")
                }));
            }
            return oComponent._detailFacade.onLockLost({
                rootId: ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", ""),
                reason: (oPayload && (oPayload.code || oPayload.reason_code)) || "KILLED",
                preserveDirty: bHadUnsavedChanges
            }, oComponent._ctx).then(function (oResult) {
                fnApplyFacadeResult(oResult);
                ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
                fnEmitTelemetry("lock.lost.detected", mOptions.telemetryRuntime.lockLost(
                    (oPayload && (oPayload.code || oPayload.reason_code)) || "KILLED",
                    "lock_probe"
                ));
                return oResult;
            });
        };

        oComponent._bLeaveReleaseSent = false;
        oComponent._fnUnregisterBeacon = oComponent._registerLockReleaseBeacon(oStateModel, oMainServiceModel);

        function applyOwnedLockState(oLockState, bResetConflict) {
            ModelStateRuntime.writeOnModel(oStateModel, "/lockExpires", oLockState.lockExpires);
            ModelStateRuntime.writeOnModel(oUiStateModel, "/lock", { ok: true, reason: "OWNED_BY_YOU", isKilled: false });
            if (bResetConflict) {
                ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", false);
            }
        }

        function onLockProbePayload(oPayload, bResetConflict) {
            var oLockState = ComponentRuntimeSupport.applyLockProbeState(oPayload, oStateModel);
            if (oLockState.killed || oLockState.lost) {
                oComponent._handleKilledLock(oPayload);
                return;
            }
            applyOwnedLockState(oLockState, bResetConflict);
        }

        oComponent._oHeartbeat.attachEvent("heartbeat", function (oEvent) {
            var oPayload = ComponentRuntimeSupport.eventPayload(oEvent);
            DebugLogger.info("Component", "lock heartbeat", oPayload);
            onLockProbePayload(oPayload, false);
            var sCheckedAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
            ModelStateRuntime.writeOnModel(oCacheModel, "/lastServerState", {
                lastChangeSet: oPayload.last_change_set || null,
                serverChangedOn: oPayload.server_changed_on || null,
                checkedAt: sCheckedAt
            });
            ModelStateRuntime.writeOnModel(oStateModel, "/cacheValidationAt", sCheckedAt);
        });
        oComponent._oHeartbeat.attachEvent("heartbeatError", function (oEvent) {
            ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", true);
            DebugLogger.info("Component", "lock heartbeat error", ComponentRuntimeSupport.eventPayload(oEvent));
        });
        oComponent._oGcd.attachEvent("gcdExpired", function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", true);
        });
        oComponent._oLockStatus.attachEvent("status", function (oEvent) {
            var oPayload = ComponentRuntimeSupport.eventPayload(oEvent);
            onLockProbePayload(oPayload, true);
        });

        oComponent._oLockStatus.attachEvent("statusError", function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", true);
        });

        oComponent._oActivity.attachEvent("idleTimeout", function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/idleExpires", new Date().toISOString());
            fnHandleForceReadOnly({
                reason: "IDLE_TIMEOUT",
                messageKey: "idleReadOnlyMessage",
                source: "activityMonitor"
            });
        });

        oComponent._oActivity.attachEvent("activity", function (oEvent) {
            var sAt = (ComponentRuntimeSupport.eventPayload(oEvent) || {}).at || new Date().toISOString();
            ModelStateRuntime.setManyOnModel(oUiStateModel, {
                "/activity/lastActiveAt": sAt,
                "/activity/idleUntil": new Date(Date.parse(sAt) + Number(TimeConfigService.read(oStateModel, "idleMs"))).toISOString()
            });
        });
    }

    function attachManagerRuntime(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var mTimerDefaults = mOptions.timerDefaults;
        var mManagers = mOptions.managers;
        var StatePaths = mOptions.statePaths;
        var DeltaPayloadBuilder = mOptions.deltaPayloadBuilder;
        var fnResolveDetailCurrent = mOptions.resolveDetailCurrent;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var DebugLogger = mOptions.debugLogger;
        var ActionContract = mOptions.actionContract;
        var fnBundleText = mOptions.bundleText;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;

        oComponent._oHeartbeat = new mManagers.HeartbeatManager({
            intervalMs: Number(mTimerDefaults.heartbeatMs),
            heartbeatFn: function () {
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") !== "EDIT" ||
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") !== "LOCKED") {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock.heartbeat !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock.heartbeat" });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock.heartbeat({
                    rootId: sRootId,
                    sessionGuid: sSessionGuid
                }).then(function (oRes) {
                    return Object.assign({ rootId: sRootId, sessionGuid: sSessionGuid }, oRes || {});
                });
            }
        });
        oComponent._oGcd = new mManagers.GCDManager({ intervalMs: Number(mTimerDefaults.gcdMs) });
        oComponent._oActivity = new mManagers.ActivityMonitor({ idleMs: Number(mTimerDefaults.idleMs) });
        oComponent._oAutoSave = new mManagers.AutoSaveCoordinator({
            intervalMs: Number(mTimerDefaults.autoSaveIntervalMs),
            debounceMs: Number(mTimerDefaults.autoSaveDebounceMs),
            lockGuardFn: function () {
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") === "LOCKED";
            },
            guardFn: function () {
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") === "EDIT" &&
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") === "LOCKED" &&
                    !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
            },
            shouldSave: function () {
                var bIsLocked = ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") === "LOCKED";
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") === "EDIT" &&
                    bIsLocked &&
                    !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false) &&
                    !!ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
            },
            buildPayload: function () {
                var sId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var oCurrent = fnResolveDetailCurrent();
                var oBase = ModelStateRuntime.readOnModel(oUiStateModel, "/_detailSnapshot", {}) || {};
                if (!sId || !oCurrent || !oCurrent.root || oCurrent.root.id !== sId) {
                    return null;
                }
                var oDelta = DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oBase);
                if (!oDelta) {
                    return null;
                }
                return { id: sId, payload: oDelta, fullPayload: CloneUtil.clone(oCurrent, {}) };
            },
            saveFn: function (oPayload) {
                if (!oComponent._detailFacade || !oComponent._ctx) {
                    return Promise.reject(new Error("Autosave unavailable: detail context missing"));
                }
                return oComponent._detailFacade.autosave({ rootId: oPayload.id, delta: oPayload.payload }, oComponent._ctx).then(function (oResult) {
                    fnApplyFacadeResult(oResult);
                    if (!oResult || oResult.ok === false) {
                        return Promise.reject((oResult && oResult.error) || new Error("Autosave usecase failed"));
                    }
                    ModelStateRuntime.writeOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
                    return oResult.data || {};
                });
            }
        });
        oComponent._oAutoSave.attachEvent("autosaveStart", function () {
            var mStart = { "/autosaveState": "SAVING" };
            mStart[StatePaths.SAVE_IN_FLIGHT] = true;
            ModelStateRuntime.setManyOnModel(oStateModel, mStart);
            DebugLogger.info("Component", "autosave start", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
            fnEmitTelemetry("autosave.triggered", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveDone", function () {
            var mDone = { "/autosaveState": "SAVED", "/autosaveAt": new Date().toISOString() };
            mDone[StatePaths.SAVE_IN_FLIGHT] = false;
            ModelStateRuntime.setManyOnModel(oStateModel, mDone);
            DebugLogger.info("Component", "autosave done", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveError", function (oEvent) {
            var mErr = { "/autosaveState": "ERROR" };
            mErr[StatePaths.SAVE_IN_FLIGHT] = false;
            ModelStateRuntime.setManyOnModel(oStateModel, mErr);
            fnSetGlobalBanner(FeedbackBannerRuntime.createRetryBannerInput("error", "objectSaveFailed", {
                textArgs: [fnBundleText("autosaveError")],
                retryAction: ActionContract.RETRY_ACTIONS.SAVE,
                retryTextKey: "retryNowButton"
            }));
            DebugLogger.info("Component", "autosave error", oEvent && oEvent.getParameters ? oEvent.getParameters() : {});
            fnEmitTelemetry("autosave.failed", ComponentRuntimeSupport.eventPayload(oEvent));
        });

        oComponent._oLockStatus = new mManagers.LockStatusMonitor({
            intervalMs: Number(mTimerDefaults.lockStatusMs),
            checkFn: function () {
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") !== "EDIT" ||
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") !== "LOCKED") {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock.status !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock.status" });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock.status({
                    rootId: sRootId,
                    sessionGuid: sSessionGuid
                }).then(function (oRes) {
                    return Object.assign({ rootId: sRootId, sessionGuid: sSessionGuid }, oRes || {});
                });
            }
        });
    }

    function buildTabId() {
        var sTabId = "";
        try {
            sTabId = window.sessionStorage.getItem("pcct_tab_id") || "";
            if (!sTabId) {
                sTabId = "tab_" + Date.now().toString(36) + "_" + Math.random().toString(36).slice(2, 10);
                window.sessionStorage.setItem("pcct_tab_id", sTabId);
            }
        } catch (_e) {
            sTabId = "tab_volatile";
        }
        return sTabId;
    }

    function runInit(aInitArgs, mDeps) {
        var UIComponent = mDeps.UIComponent;
        var ModelFactory = mDeps.ModelFactory;
        var SmartSearchAdapter = mDeps.SmartSearchAdapter;
        var Managers = mDeps.Managers || {};
        var HeartbeatManager = Managers.HeartbeatManager || mDeps.HeartbeatManager;
        var GCDManager = Managers.GCDManager || mDeps.GCDManager;
        var ActivityMonitor = Managers.ActivityMonitor || mDeps.ActivityMonitor;
        var AutoSaveCoordinator = Managers.AutoSaveCoordinator || mDeps.AutoSaveCoordinator;
        var LockStatusMonitor = Managers.LockStatusMonitor || mDeps.LockStatusMonitor;
        var JSONModel = mDeps.JSONModel;
        var FlowCoordinator = mDeps.FlowCoordinator;
        var DeltaPayloadBuilder = mDeps.DeltaPayloadBuilder;
        var GatewayBackendService = mDeps.GatewayBackendService;
        var SettingsManager = Managers.SettingsManager || mDeps.SettingsManager;
        var DebugLogger = mDeps.DebugLogger;
        var RuntimeTimerSanitizer = mDeps.RuntimeTimerSanitizer;
        var TimeConfigService = mDeps.TimeConfigService;
        var ApplyRuntimeSettingsUseCase = mDeps.ApplyRuntimeSettingsUseCase;
        var EnsureDictLoadedUseCase = mDeps.EnsureDictLoadedUseCase;
        var BootstrapAppUseCase = mDeps.BootstrapAppUseCase;
        var DiagnosticsUseCase = mDeps.DiagnosticsUseCase;
        var CtxFactory = mDeps.CtxFactory;
        var EffectApplier = mDeps.EffectApplier;
        var FeedbackPolicy = mDeps.FeedbackPolicy;
        var ComponentInitSaveGuardSupport = mDeps.ComponentInitSaveGuardSupport;
        var ComponentRuntimeSupport = mDeps.ComponentRuntimeSupport;
        var TelemetryRuntime = mDeps.TelemetryRuntime;
        var LayoutStateRuntime = mDeps.LayoutStateRuntime;
        var StatePaths = mDeps.StatePaths;
        var DetailFacade = mDeps.DetailFacade;
        var ActionDispatcher = mDeps.ActionDispatcher;
        var ActionContract = mDeps.ActionContract;
        var ODataModel = mDeps.ODataModel;
        var WorkflowTelemetry = mDeps.WorkflowTelemetry;
        var CreateSentinel = mDeps.CreateSentinel;
        var Device = mDeps.Device;
        var InteractionFX = mDeps.InteractionFX;
        var ThemeService = mDeps.ThemeService;

            UIComponent.prototype.init.apply(this, aInitArgs || []);
            this._startupPerf = this._startupPerf || {
                t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
                firstRouteReadyLogged: false,
                analyticsStartedLogged: false
            };
            ThemeService.syncDocumentRootClasses();
            var sConfiguredMode = this.getManifestEntry("/sap.ui5/config/backendMode") || "real";
            var sUiContractVersion = this.getManifestEntry("/sap.ui5/config/uiContractVersion") || "1.0.0";
            var sMainServiceUri = this.getManifestEntry("/sap.app/dataSources/mainService/uri") || "/sap/opu/odata/sap/Z_UI5_SRV/";
            var oDataModel = reuseJsonModel(this.getModel("data"), ModelFactory.createDataModel);
            var oMplModel = reuseJsonModel(this.getModel("mpl"), ModelFactory.createMplModel);
            var oStateModel = reuseJsonModel(this.getModel("state"), ModelFactory.createStateModel);
            var oUiStateModel = reuseJsonModel(this.getModel("uiState"), ModelFactory.createUiStateModel);
            var oViewModel = reuseJsonModel(this.getModel("view"), ModelFactory.createViewModel);
            var oSelectedModel = reuseJsonModel(this.getModel("selected"), function () { return new JSONModel({}); });
            var oMasterDataModel = reuseJsonModel(this.getModel("masterData"), ModelFactory.createMasterDataModel);
            var oDeviceModel = new JSONModel(Device);
            var oMainServiceModel = this.getModel("mainService") || new ODataModel(sMainServiceUri, {
                useBatch: true,
                tokenHandling: true,
                defaultBindingMode: "TwoWay",
                defaultCountMode: "Inline",
                refreshAfterChange: false
            });
            oMainServiceModel.setDeferredGroups(["changes", "autosave", "saveFlow", "locks"]);
            oMainServiceModel.setChangeGroups({
                "*": {
                    groupId: "changes",
                    changeSetId: "ChecklistSave",
                    single: false
                },
                "LockAcquireType": { groupId: "locks", single: true },
                "LockHeartbeatType": { groupId: "locks", single: true },
                "LockReleaseType": { groupId: "locks", single: true }
            });
            this.setModel(oMainServiceModel, "mainService");
            this.setModel(oMainServiceModel);
            GatewayBackendService.setModel(oMainServiceModel, { serviceUrl: sMainServiceUri });
            var fnBundleText = createBundleText(this);
            var oFeedbackRuntime = createFeedbackRuntime({
                stateModel: oStateModel,
                statePaths: StatePaths,
                feedbackPolicy: FeedbackPolicy,
                bundleText: fnBundleText
            });
            // ZERO-LEGACY: BackendAdapter has been removed. UI5 ODataModel is the single transport.
            DiagnosticsUseCase.execute({}, {
                mainServiceModel: oMainServiceModel,
                stateModel: oStateModel,
                getBackendMode: function () { return "real"; },
                onMetadataFailed: function () {
                    ModelStateRuntime.writeOnModel(oStateModel, "/backendMode", "real");
                }
            });

            this.setModel(oDataModel, "data");
            this.setModel(oMplModel, "mpl");
            this.setModel(oSelectedModel, "selected");
            this.setModel(oStateModel, "state");
            this.setModel(oUiStateModel, "uiState");
            this.setModel(oViewModel, "view");
            this.setModel(oMasterDataModel, "masterData");
            oDeviceModel.setDefaultBindingMode("OneWay");
            this.setModel(oDeviceModel, "device");
            this._oInteractionFX = InteractionFX;
            // Build a Gateway-first context (ports/adapters) for this component.
            this._ctx = CtxFactory.buildCtx(this, {});
            this._detailFacade = new DetailFacade();
            this._actionDispatcher = new ActionDispatcher();
            this._actionDispatcher.setValidators(buildActionValidators(ActionContract));
            var oLayoutModel = reuseJsonModel(this.getModel("layout"), ModelFactory.createLayoutModel);
            var oCacheModel = reuseJsonModel(this.getModel("cache"), ModelFactory.createCacheModel);
            var oEnvModel = ModelFactory.createEnvModel();
            var mTimerDefaults = TimeConfigService.buildDefaultTimerMap();
            var mInitState = { "/timers": mTimerDefaults };
            mInitState[StatePaths.SAVE_IN_FLIGHT] = false;
            mInitState[StatePaths.PENDING_NAVIGATION_INTENT] = null;
            mInitState[StatePaths.TAB_CONFLICT_STATE] = { active: false, source: "", at: "" };
            mInitState["/networkOnline"] = true;
            mInitState["/networkGraceMode"] = false;
            mInitState["/networkGraceExpiresAt"] = null;
            ModelStateRuntime.setManyOnModel(oStateModel, mInitState);
            var fnEmitTelemetry = function (sEventName, oPayload) {
                return WorkflowTelemetry.emit(sEventName, {
                    stateModel: oStateModel,
                    payload: oPayload || {}
                });
            };
            var fnResolveDetailCurrent = function () {
                return ComponentRuntimeSupport.resolveDetailCurrent(oSelectedModel, oUiStateModel);
            };
            var fnApplyFacadeResult = createApplyFacadeResult({
                component: this,
                effectApplier: EffectApplier,
                actionDispatcher: this._actionDispatcher,
                selectedModel: oSelectedModel,
                uiStateModel: oUiStateModel,
                componentRuntimeSupport: ComponentRuntimeSupport
            });
            var fnBuildLatestCtx = function () {
                this._ctx = CtxFactory.buildCtx(this, {});
                return this._ctx;
            }.bind(this);
            var fnHandleForceReadOnly = function (mInput) {
                var mForceInput = Object.assign({}, mInput || {});
                if (!Object.prototype.hasOwnProperty.call(mForceInput, "preserveDirty")) {
                    mForceInput.preserveDirty = !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
                }
                this._oHeartbeat.stop();
                this._oLockStatus.stop();
                this._oAutoSave.stop();
                this._oGcd.destroyManager();
                return this._detailFacade.forceReadOnly(mForceInput, this._ctx).then(function (oResult) {
                    fnApplyFacadeResult(oResult);
                    ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
                    fnEmitTelemetry("lock.lost.detected", TelemetryRuntime.lockLost(
                        mForceInput && mForceInput.reason,
                        mForceInput && mForceInput.source
                    ));
                    return oResult;
                });
            }.bind(this);
            var fnLoadRuntimeSettings = function () {
                return SettingsManager.load(GatewayBackendService).then(function (oRuntime) {
                    return this._applyFrontendRuntimeConfig({
                        source: "RuntimeSettingsSet(GLOBAL)",
                        runtimeSettingsPayload: oRuntime || {}
                    }, oStateModel, oEnvModel, oMasterDataModel).then(function () {
                        ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", "gateway_runtime");
                        fnEmitTelemetry("runtime.config.loaded", TelemetryRuntime.runtimeConfig("RuntimeSettingsSet(GLOBAL)"));
                        return oRuntime || {};
                    });
                }.bind(this)).catch(function (oError) {
                    ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", "gateway_runtime_error");
                    fnEmitTelemetry("runtime.config.load_failed", TelemetryRuntime.runtimeConfig(
                        "RuntimeSettingsSet(GLOBAL)",
                        (oError && oError.message) || oError || "runtime_settings_load_failed"
                    ));
                    // Non-fatal: resolve with empty config so boot sequence continues
                    return {};
                }.bind(this));
            }.bind(this);
            var fnResolveCorrelationId = oFeedbackRuntime.resolveCorrelationId;
            var fnIsSessionExpiredError = oFeedbackRuntime.isSessionExpiredError;
            var fnSetGlobalBanner = oFeedbackRuntime.setGlobalBanner;
            var fnClearGlobalBanner = oFeedbackRuntime.clearGlobalBanner;
            var fnQueuePendingNavigationIntent = function (oRouteEvent) {
                return queuePendingNavigationIntent(oStateModel, StatePaths, oRouteEvent);
            };
            var fnClearPendingNavigationIntent = function () {
                return clearPendingNavigationIntent(oStateModel, StatePaths);
            };
            var fnResumePendingNavigationIntent = function () {
                return resumePendingNavigationIntent(this, oStateModel, StatePaths);
            }.bind(this);
            var fnRunGuardedSave = ComponentInitSaveGuardSupport.createRunGuardedSave({
                component: this,
                stateModel: oStateModel,
                mainServiceModel: oMainServiceModel,
                statePaths: StatePaths,
                detailFacade: this._detailFacade,
                buildLatestCtx: fnBuildLatestCtx,
                applyFacadeResult: fnApplyFacadeResult,
                emitTelemetry: fnEmitTelemetry,
                resumePendingNavigationIntent: fnResumePendingNavigationIntent,
                resolveCorrelationId: fnResolveCorrelationId,
                isSessionExpiredError: fnIsSessionExpiredError,
                setGlobalBanner: fnSetGlobalBanner,
                clearGlobalBanner: fnClearGlobalBanner
            });
            var oCrossTabRuntime = attachCrossTabRuntime({
                component: this,
                stateModel: oStateModel,
                statePaths: StatePaths,
                bundleText: fnBundleText,
                setGlobalBanner: fnSetGlobalBanner,
                handleForceReadOnly: fnHandleForceReadOnly
            });
            var fnPublishTabSignal = oCrossTabRuntime.publishTabSignal;
            registerDefaultHandlers({
                actionDispatcher: this._actionDispatcher,
                actionContract: ActionContract,
                detailFacade: this._detailFacade,
                runGuardedSave: fnRunGuardedSave,
                buildLatestCtx: fnBuildLatestCtx,
                applyFacadeResult: fnApplyFacadeResult,
                getCtx: function () { return this._ctx; }.bind(this)
            });
                        attachManagerRuntime({
                component: this,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                timerDefaults: mTimerDefaults,
                managers: {
                    HeartbeatManager: HeartbeatManager,
                    GCDManager: GCDManager,
                    ActivityMonitor: ActivityMonitor,
                    AutoSaveCoordinator: AutoSaveCoordinator,
                    LockStatusMonitor: LockStatusMonitor
                },
                statePaths: StatePaths,
                deltaPayloadBuilder: DeltaPayloadBuilder,
                resolveDetailCurrent: fnResolveDetailCurrent,
                applyFacadeResult: fnApplyFacadeResult,
                setGlobalBanner: fnSetGlobalBanner,
                emitTelemetry: fnEmitTelemetry,
                debugLogger: DebugLogger,
                actionContract: ActionContract,
                bundleText: fnBundleText,
                componentRuntimeSupport: ComponentRuntimeSupport,
                telemetryRuntime: TelemetryRuntime
            });
            attachLockRuntime({
                component: this,
                mainServiceModel: oMainServiceModel,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                cacheModel: oCacheModel,
                statePaths: StatePaths,
                componentRuntimeSupport: ComponentRuntimeSupport,
                timeConfigService: TimeConfigService,
                debugLogger: DebugLogger,
                bundleText: fnBundleText,
                emitTelemetry: fnEmitTelemetry,
                setGlobalBanner: fnSetGlobalBanner,
                handleForceReadOnly: fnHandleForceReadOnly,
                applyFacadeResult: fnApplyFacadeResult,
                telemetryRuntime: TelemetryRuntime
            });
            attachInitListeners({
                component: this,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                selectedModel: oSelectedModel,
                layoutModel: oLayoutModel,
                cacheModel: oCacheModel,
                masterDataModel: oMasterDataModel,
                envModel: oEnvModel,
                statePaths: StatePaths,
                smartSearchAdapter: SmartSearchAdapter,
                componentRuntimeSupport: ComponentRuntimeSupport,
                timeConfigService: TimeConfigService,
                flowCoordinator: FlowCoordinator,
                bundleText: fnBundleText,
                setGlobalBanner: fnSetGlobalBanner,
                clearGlobalBanner: fnClearGlobalBanner,
                handleForceReadOnly: fnHandleForceReadOnly,
                runGuardedSave: fnRunGuardedSave,
                queuePendingNavigationIntent: fnQueuePendingNavigationIntent,
                clearPendingNavigationIntent: fnClearPendingNavigationIntent,
                resumePendingNavigationIntent: fnResumePendingNavigationIntent,
                emitTelemetry: fnEmitTelemetry,
                publishTabSignal: fnPublishTabSignal,
                telemetryRuntime: TelemetryRuntime,
                layoutStateRuntime: LayoutStateRuntime,
                actionContract: ActionContract
            });

            runBootSequence({
                component: this,
                stateModel: oStateModel,
                envModel: oEnvModel,
                cacheModel: oCacheModel,
                bootstrapAppUseCase: BootstrapAppUseCase,
                ensureDictLoadedUseCase: EnsureDictLoadedUseCase,
                componentRuntimeSupport: ComponentRuntimeSupport,
                loadRuntimeSettings: fnLoadRuntimeSettings,
                loadCurrentUser: function () {
                    return mDeps.LoadCurrentUserUseCase && mDeps.LoadCurrentUserUseCase.refresh
                        ? mDeps.LoadCurrentUserUseCase.refresh({ stateModel: oStateModel })
                        : Promise.resolve(null);
                },
                bundleText: fnBundleText
            });

    }

    return {
        runInit: runInit
    };
});

