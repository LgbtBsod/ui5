sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap_ui5/model/ModelFactory",
    "sap_ui5/service/SmartSearchAdapter",
    "sap_ui5/manager/ManagerFacade",
    "sap/ui/model/json/JSONModel",
    "sap_ui5/util/FlowCoordinator",
    "sap_ui5/util/DeltaPayloadBuilder",
    "sap_ui5/service/backend/GatewayBackendService",
    "sap_ui5/util/DebugLogger",
    "sap_ui5/util/RuntimeTimerSanitizer",
    "sap_ui5/util/TimeConfigService",
    "sap_ui5/service/domain/shared/usecases/ApplyRuntimeSettingsUseCase",
    "sap_ui5/service/domain/shared/usecases/EnsureDictLoadedUseCase",
    "sap_ui5/service/domain/shared/usecases/BootstrapAppUseCase",
    "sap_ui5/service/domain/shared/usecases/LoadCurrentUserUseCase",
    "sap_ui5/service/domain/shared/usecases/StartManagersUseCase",
    "sap_ui5/service/domain/shared/usecases/DiagnosticsUseCase",
    "sap_ui5/service/framework/CtxFactory",
    "sap_ui5/service/framework/EffectApplier",
    "sap_ui5/service/framework/FeedbackPolicy",
    "sap_ui5/service/framework/ComponentRuntimeSupport",
    "sap_ui5/service/framework/ComponentLockReleaseSupport",
    "sap_ui5/service/framework/ComponentInitFeedbackSupport",
    "sap_ui5/service/framework/ComponentInitSaveGuardSupport",
    "sap_ui5/service/framework/ComponentInitLockRuntimeSupport",
    "sap_ui5/service/framework/ComponentInitListenersSupport",
    "sap_ui5/service/framework/ComponentInitBootSupport",
    "sap_ui5/service/framework/ComponentInitActionRoutingSupport",
    "sap_ui5/service/framework/ComponentInitCrossTabSupport",
    "sap_ui5/service/framework/ComponentInitManagerRuntimeSupport",
    "sap_ui5/service/framework/ComponentInitRuntimeSupport",
    "sap_ui5/service/framework/ComponentInitRuntime",
    "sap_ui5/model/StatePaths",
    "sap_ui5/service/domain/detail/DetailFacade",
    "sap_ui5/service/framework/ActionDispatcher",
    "sap_ui5/service/framework/ActionContract",
    "sap/ui/model/odata/v2/ODataModel",
    "sap_ui5/util/WorkflowTelemetry",
    "sap_ui5/util/CreateSentinel",
    "sap/ui/Device",
    "sap_ui5/util/InteractionFX",
    "sap_ui5/util/ThemeService"
], function (
    UIComponent,
    ModelFactory,
    SmartSearchAdapter,
    ManagerFacade,
    JSONModel,
    FlowCoordinator,
    DeltaPayloadBuilder,
    GatewayBackendService,
    DebugLogger,
    RuntimeTimerSanitizer,
    TimeConfigService,
    ApplyRuntimeSettingsUseCase,
    EnsureDictLoadedUseCase,
    BootstrapAppUseCase,
    LoadCurrentUserUseCase,
    StartManagersUseCase,
    DiagnosticsUseCase,
    CtxFactory,
    EffectApplier,
    FeedbackPolicy,
    ComponentRuntimeSupport,
    ComponentLockReleaseSupport,
    ComponentInitFeedbackSupport,
    ComponentInitSaveGuardSupport,
    ComponentInitLockRuntimeSupport,
    ComponentInitListenersSupport,
    ComponentInitBootSupport,
    ComponentInitActionRoutingSupport,
    ComponentInitCrossTabSupport,
    ComponentInitManagerRuntimeSupport,
    ComponentInitRuntimeSupport,
    ComponentInitRuntime,
    StatePaths,
    DetailFacade,
    ActionDispatcher,
    ActionContract,
    ODataModel,
    WorkflowTelemetry,
    CreateSentinel,
    Device,
    InteractionFX,
    ThemeService
) {
    "use strict";

    return UIComponent.extend("sap_ui5.Component", {
        metadata: {
            manifest: "json"
        },
        createContent: function () {
            return sap.ui.xmlview({
                id: this.createId("app"),
                viewName: "sap_ui5.view.App"
            });
        },
        init: function () {
            return ComponentInitRuntime.runInit.call(this, arguments, {
                UIComponent: UIComponent,
                ModelFactory: ModelFactory,
                SmartSearchAdapter: SmartSearchAdapter,
                Managers: ManagerFacade,
                JSONModel: JSONModel,
                FlowCoordinator: FlowCoordinator,
                DeltaPayloadBuilder: DeltaPayloadBuilder,
                GatewayBackendService: GatewayBackendService,
                DebugLogger: DebugLogger,
                RuntimeTimerSanitizer: RuntimeTimerSanitizer,
                TimeConfigService: TimeConfigService,
                ApplyRuntimeSettingsUseCase: ApplyRuntimeSettingsUseCase,
                EnsureDictLoadedUseCase: EnsureDictLoadedUseCase,
                BootstrapAppUseCase: BootstrapAppUseCase,
                LoadCurrentUserUseCase: LoadCurrentUserUseCase,
                DiagnosticsUseCase: DiagnosticsUseCase,
                CtxFactory: CtxFactory,
                EffectApplier: EffectApplier,
                FeedbackPolicy: FeedbackPolicy,
                ComponentRuntimeSupport: ComponentRuntimeSupport,
                ComponentLockReleaseSupport: ComponentLockReleaseSupport,
                ComponentInitFeedbackSupport: ComponentInitFeedbackSupport,
                ComponentInitSaveGuardSupport: ComponentInitSaveGuardSupport,
                ComponentInitLockRuntimeSupport: ComponentInitLockRuntimeSupport,
                ComponentInitListenersSupport: ComponentInitListenersSupport,
                ComponentInitBootSupport: ComponentInitBootSupport,
                ComponentInitActionRoutingSupport: ComponentInitActionRoutingSupport,
                ComponentInitCrossTabSupport: ComponentInitCrossTabSupport,
                ComponentInitManagerRuntimeSupport: ComponentInitManagerRuntimeSupport,
                ComponentInitRuntimeSupport: ComponentInitRuntimeSupport,
                StatePaths: StatePaths,
                DetailFacade: DetailFacade,
                ActionDispatcher: ActionDispatcher,
                ActionContract: ActionContract,
                ODataModel: ODataModel,
                WorkflowTelemetry: WorkflowTelemetry,
                CreateSentinel: CreateSentinel,
                Device: Device,
                InteractionFX: InteractionFX,
                ThemeService: ThemeService
            });
        },
        _startCoreManagers: function () {
            return StartManagersUseCase.execute({ scope: "core" }, { managers: this._collectManagers() });
        },
        _stopLockScopedManagers: function () {
            return StartManagersUseCase.execute({ scope: "lock", lockRuntimeActive: false }, { managers: this._collectManagers() });
        },
        _startLockScopedManagers: function () {
            return StartManagersUseCase.execute({ scope: "lock", lockRuntimeActive: true }, { managers: this._collectManagers() });
        },
        _stopAllManagers: function () {
            this._stopLockScopedManagers();
            if (this._oConnectivity) {
                this._oConnectivity.stop();
            }
        },
        _collectManagers: function () {
            return {
                heartbeat: this._oHeartbeat,
                activity: this._oActivity,
                autosave: this._oAutoSave,
                connectivity: this._oConnectivity,
                lockStatus: this._oLockStatus,
                gcd: this._oGcd
            };
        },
        _isLockRuntimeActive: function (oStateModel) {
            return oStateModel.getProperty(StatePaths.WORKFLOW_EDIT_MODE) === "EDIT" && oStateModel.getProperty(StatePaths.WORKFLOW_LOCK_STATUS) === "LOCKED";
        },
        _syncLockScopedManagers: function (oStateModel) {
            var bActive = this._isLockRuntimeActive(oStateModel);
            if (bActive) {
                this._startLockScopedManagers();
                return;
            }
            this._stopLockScopedManagers();
        },
        _applyFrontendRuntimeConfig: function (oFrontendConfig, oStateModel, oEnvModel, oMasterDataModel) {
            RuntimeTimerSanitizer.sanitizeTimers((oFrontendConfig && oFrontendConfig.runtimeSettingsPayload) || {}, oStateModel.getProperty("/timers") || {});
            oStateModel.setProperty("/timers", TimeConfigService.normalize((oFrontendConfig && oFrontendConfig.runtimeSettingsPayload) || {}, oStateModel.getProperty("/timers") || {}));
            return ApplyRuntimeSettingsUseCase.execute({ frontendConfig: oFrontendConfig || {} }, {
                stateModel: oStateModel,
                envModel: oEnvModel,
                masterDataModel: oMasterDataModel
            }).then(function () {
                this._applyManagersTimerConfig(oStateModel.getProperty("/timers") || {});
            }.bind(this));
        },
        _applyManagersTimerConfig: function (mTimers) {
            if (this._oHeartbeat && this._oHeartbeat.setIntervalMs) {
                this._oHeartbeat.setIntervalMs(mTimers.heartbeatMs);
            }
            if (this._oLockStatus && this._oLockStatus.setIntervalMs) {
                this._oLockStatus.setIntervalMs(mTimers.lockStatusMs);
            }
            if (this._oGcd && this._oGcd.setIntervalMs) {
                this._oGcd.setIntervalMs(mTimers.gcdMs);
            }
            if (this._oActivity && this._oActivity.setIdleMs) {
                this._oActivity.setIdleMs(mTimers.idleMs);
            }
            if (this._oAutoSave && this._oAutoSave.setIntervals) {
                this._oAutoSave.setIntervals({
                    intervalMs: mTimers.autoSaveIntervalMs,
                    debounceMs: mTimers.autoSaveDebounceMs
                });
            }
            if (this._oConnectivity && this._oConnectivity.setGraceMs) {
                this._oConnectivity.setGraceMs(mTimers.networkGraceMs);
            }
            if (this._oSmartCache && this._oSmartCache.configureFreshness) {
                this._oSmartCache.configureFreshness({ freshMs: mTimers.cacheFreshMs, staleOkMs: mTimers.cacheStaleOkMs });
            }
        },
        _registerLockReleaseBeacon: function (oStateModel, oMainServiceModel) {
            var that = this;
            var fnPageHide = function () {
                that._releaseActiveLockOnLeave(oStateModel, oMainServiceModel);
            };
            window.addEventListener("pagehide", fnPageHide);
            return function () {
                window.removeEventListener("pagehide", fnPageHide);
            };
        },
        _releaseActiveLockOnLeave: function (oStateModel, oMainServiceModel) {
            var oPayload;
            var sUrl;
            var sToken;
            if (this._bLeaveReleaseSent) {
                return;
            }
            oPayload = ComponentLockReleaseSupport.readActiveLockPayload(oStateModel);
            sUrl = ComponentLockReleaseSupport.buildLockReleaseUrl(oStateModel);
            sToken = oMainServiceModel && oMainServiceModel.getSecurityToken ? String(oMainServiceModel.getSecurityToken() || "").trim() : "";
            if (!oPayload || !sUrl) {
                return;
            }
            this._bLeaveReleaseSent = true;
            ComponentLockReleaseSupport.tryBeaconLockRelease(sUrl, oPayload, sToken);
        },
        attachInteractionFxToApp: function (oDomRef) {
            if (this._oInteractionFxHandle && this._oInteractionFxHandle.destroy) {
                this._oInteractionFxHandle.destroy();
                this._oInteractionFxHandle = null;
            }
            if (this._oInteractionFX && typeof this._oInteractionFX.attach === "function") {
                this._oInteractionFxHandle = this._oInteractionFX.attach(oDomRef);
            }
        },
        exit: function () {
            if (this._oLifecycleRouter && this._fnBeforeRouteMatched && this._oLifecycleRouter.detachBeforeRouteMatched) {
                this._oLifecycleRouter.detachBeforeRouteMatched(this._fnBeforeRouteMatched, this);
            }
            if (this._oDirtyStateBinding && this._fnDirtyStateBindingChange) {
                this._oDirtyStateBinding.detachChange(this._fnDirtyStateBindingChange);
                this._oDirtyStateBinding.destroy();
            }
            (this._aLockScopedStateBindings || []).forEach(function (oEntry) {
                if (oEntry && oEntry.binding && oEntry.handler) {
                    oEntry.binding.detachChange(oEntry.handler);
                    oEntry.binding.destroy();
                }
            });
            if (typeof this._detachInitRuntimeListeners === "function") {
                this._detachInitRuntimeListeners();
            }
            if (this._oInteractionFxHandle && this._oInteractionFxHandle.destroy) {
                this._oInteractionFxHandle.destroy();
                this._oInteractionFxHandle = null;
            }
            this._stopAllManagers();
            if (this._fnUnregisterBeacon) {
                this._fnUnregisterBeacon();
            }
            if (this._fnCrossTabStorage) {
                window.removeEventListener("storage", this._fnCrossTabStorage);
            }
            if (this._oCrossTabChannel && typeof this._oCrossTabChannel.close === "function") {
                this._oCrossTabChannel.close();
            }
            if (this._fnOnFullSave) {
                window.removeEventListener("pcct:fullSave", this._fnOnFullSave);
            }
            if (this._iSaveWorkingTimer) {
                clearTimeout(this._iSaveWorkingTimer);
                this._iSaveWorkingTimer = null;
            }
            this._fnCrossTabStorage = null;
            this._oCrossTabChannel = null;
            this._oLifecycleRouter = null;
            this._fnBeforeRouteMatched = null;
            this._oDirtyStateBinding = null;
            this._fnDirtyStateBindingChange = null;
            this._aLockScopedStateBindings = null;
            this._oStateLifecycleModel = null;
            this._oSelectedLifecycleModel = null;
            this._fnStateModelPropertyChange = null;
            this._fnSelectedModelPropertyChange = null;
            this._detachInitRuntimeListeners = null;
        }
    });
});

