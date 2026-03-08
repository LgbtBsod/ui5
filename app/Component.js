sap.ui.define([
    "sap/ui/core/UIComponent",
    "checklist/app/model/ModelFactory",
    "checklist/app/service/SmartSearchAdapter",
    "checklist/app/service/runtime/ManagerFacade",
    "sap/ui/model/json/JSONModel",
    "checklist/app/util/FlowCoordinator",
    "checklist/app/util/DeltaPayloadBuilder",
    "checklist/app/service/backend/GatewayBackendService",
    "checklist/app/util/DebugLogger",
    "checklist/app/util/RuntimeTimerSanitizer",
    "checklist/app/util/TimeConfigService",
    "checklist/app/service/domain/shared/usecases/ApplyRuntimeSettingsUseCase",
    "checklist/app/service/domain/shared/usecases/EnsureDictLoadedUseCase",
    "checklist/app/service/domain/shared/usecases/BootstrapAppUseCase",
    "checklist/app/service/domain/shared/usecases/LoadCurrentUserUseCase",
    "checklist/app/service/domain/shared/usecases/StartManagersUseCase",
    "checklist/app/service/domain/shared/usecases/DiagnosticsUseCase",
    "checklist/app/service/framework/CtxFactory",
    "checklist/app/service/framework/EffectApplier",
    "checklist/app/service/framework/FeedbackPolicy",
    "checklist/app/service/framework/ComponentRuntimeSupport",
    "checklist/app/service/framework/ComponentLockReleaseSupport",
    "checklist/app/service/framework/ComponentInitSaveGuardSupport",
    "checklist/app/service/framework/ComponentInitRuntime",
    "checklist/app/service/framework/TelemetryRuntime",
    "checklist/app/service/framework/LayoutStateRuntime",
    "checklist/app/model/StatePaths",
    "checklist/app/service/domain/detail/DetailFacade",
    "checklist/app/service/framework/ActionDispatcher",
    "checklist/app/service/framework/ActionContract",
    "sap/ui/model/odata/v2/ODataModel",
    "checklist/app/util/WorkflowTelemetry",
    "checklist/app/util/CreateSentinel",
    "sap/ui/Device",
    "checklist/app/util/InteractionFX",
    "checklist/app/util/ThemeService",
    "checklist/app/service/framework/SchedulingRuntime"
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
    ComponentInitSaveGuardSupport,
    ComponentInitRuntime,
    TelemetryRuntime,
    LayoutStateRuntime,
    StatePaths,
    DetailFacade,
    ActionDispatcher,
    ActionContract,
    ODataModel,
    WorkflowTelemetry,
    CreateSentinel,
    Device,
    InteractionFX,
    ThemeService,
    SchedulingRuntime
) {
    "use strict";

    // Polyfill: Promise.prototype.finally (not available in IE11 / older SAP WebAS Chromium)
    if (typeof Promise === "function" && typeof Promise.prototype.finally !== "function") {
        Promise.prototype.finally = function (fnCallback) {
            var P = this.constructor || Promise;
            return this.then(
                function (vValue) { return P.resolve(typeof fnCallback === "function" ? fnCallback() : undefined).then(function () { return vValue; }); },
                function (oReason) { return P.resolve(typeof fnCallback === "function" ? fnCallback() : undefined).then(function () { throw oReason; }); }
            );
        };
    }


    return UIComponent.extend("checklist.app.Component", {
        metadata: {
            manifest: "json"
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
                ComponentInitSaveGuardSupport: ComponentInitSaveGuardSupport,
                TelemetryRuntime: TelemetryRuntime,
                LayoutStateRuntime: LayoutStateRuntime,
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
            this._iSaveWorkingTimer = SchedulingRuntime.clearTimer(this._iSaveWorkingTimer);
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

