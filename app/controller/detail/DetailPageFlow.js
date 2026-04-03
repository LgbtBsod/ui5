sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/runtime/DetailEditSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailMatchedRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (DetailAccessViewState, DetailEditSessionRuntime, DetailInfoCardLayoutRuntime, DetailMatchedRuntime, ModelPathContracts, ViewPathContracts, ControllerViewStateRuntime, ModelStateRuntime, ReadinessTelemetryRuntime, SchedulingRuntime, StatePaths, ReadinessTelemetryContracts, JsRuntime, ModelContracts, DetailUseCaseConstants) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_CODES = DetailUseCaseConstants.CODES;
    var oDetailEditSessionRuntime = new DetailEditSessionRuntime();
    var DETAIL_ROW_BUSY_PATHS = Object.freeze({
        CHECKS: "/checksBusy",
        BARRIERS: "/barriersBusy"
    });

    function runDetailCommand(oController, sMethod, mInput) {
        if (!oController || typeof oController._dispatchDetailCommand !== TYPE_FUNCTION) {
            return Promise.resolve(false);
        }
        return oController._dispatchDetailCommand(sMethod, mInput || {});
    }

    function getModel(oController, sName) {
        return oController && oController.getModel ? oController.getModel(sName) : null;
    }

    function markDetailReady(oController, mDetails) {
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.DETAIL_READY, mDetails);
    }

    function shouldSuppressMatchFailureBanner(oController, oError) {
        var oAccessState = ControllerViewStateRuntime.get(oController, "/accessState", {}) || {};
        var sErrorCode = String((oError && oError.code) || (oError && oError.error && oError.error.code) || "").trim();
        return !!oAccessState.denied
            || sErrorCode === DETAIL_CODES.NO_VIEW_PERMISSION
            || sErrorCode === DETAIL_CODES.PERMISSION_CHECK_FAILED;
    }

    function handleMatchFailure(oController, oError) {
        oDetailEditSessionRuntime.clearAnalyticsReturnRestore(oController);
        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false);
        ControllerViewStateRuntime.set(oController, ViewPathContracts.DETAIL_SKELETON_BUSY, false);
        clearDeferredRowBusy(oController);
        if (!shouldSuppressMatchFailureBanner(oController, oError)
            && oController && typeof oController.showI18nError === TYPE_FUNCTION) {
            oController.showI18nError("unexpectedError");
        }
        return { ok: false, error: oError };
    }

    function cleanupRouteArtifacts(oController) {
        if (oController._mLazyDialogs) {
            Object.keys(oController._mLazyDialogs).forEach(function (sKey) {
                var oDialog = oController._mLazyDialogs[sKey];
                if (oDialog && typeof oDialog[METHODS.CLOSE] === TYPE_FUNCTION) {
                    oDialog[METHODS.CLOSE]();
                }
            });
        }
        oController._iAttachmentDropZoneBindTimer = SchedulingRuntime.clearTimer(oController._iAttachmentDropZoneBindTimer);
        oController._iDetailPhaseLoadTimer = SchedulingRuntime.clearTimer(oController._iDetailPhaseLoadTimer);
        if (typeof oController._clearLocationValueHelpSearchTimer === TYPE_FUNCTION) {
            oController._clearLocationValueHelpSearchTimer();
        }
        oController._iLocationVhTableSyncTimer = SchedulingRuntime.clearTimer(oController._iLocationVhTableSyncTimer);
    }

    function sameActiveDbKey(oController, sDbKey) {
        var sActiveDbKey = String(ModelStateRuntime.read(oController, STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, "") || "").trim();
        return !!sDbKey && sActiveDbKey === sDbKey;
    }

    function clearDeferredRowBusy(oController) {
        var mBusyPatch = {};
        mBusyPatch[DETAIL_ROW_BUSY_PATHS.CHECKS] = false;
        mBusyPatch[DETAIL_ROW_BUSY_PATHS.BARRIERS] = false;
        ControllerViewStateRuntime.setMany(oController, mBusyPatch);
    }

    function writeDeferredRows(oController, sDbKey, oRows) {
        var oDetailModel;
        if (!sameActiveDbKey(oController, sDbKey)) {
            return false;
        }
        oDetailModel = getModel(oController, ModelContracts.MODELS.DETAIL);
        if (!oDetailModel) {
            return false;
        }
        ModelStateRuntime.setManyOnModel(oDetailModel, {
            "/current/checks": (oRows && oRows.checks) || [],
            "/current/barriers": (oRows && oRows.barriers) || [],
            "/base/checks": (oRows && oRows.checks) || [],
            "/base/barriers": (oRows && oRows.barriers) || []
        });
        clearDeferredRowBusy(oController);
        return true;
    }

    function scheduleDeferredRowHydration(oController, sDbKey, mHooks) {
        var fnBuildContext = mHooks && mHooks.buildCommandContext;
        if (!sDbKey || typeof fnBuildContext !== TYPE_FUNCTION) {
            clearDeferredRowBusy(oController);
            return Promise.resolve(null);
        }
        oController._iDetailPhaseLoadTimer = SchedulingRuntime.restartTimer(oController._iDetailPhaseLoadTimer, function () {
            var oCtx = fnBuildContext();
            var oRepo = oCtx && oCtx.repo;
            if (!oRepo || typeof oRepo.loadDetailRows !== TYPE_FUNCTION) {
                clearDeferredRowBusy(oController);
                return;
            }
            Promise.resolve(oRepo.loadDetailRows({ dbKey: sDbKey })).then(function (oRows) {
                if (!writeDeferredRows(oController, sDbKey, oRows || {})) {
                    clearDeferredRowBusy(oController);
                }
            }).catch(function () {
                clearDeferredRowBusy(oController);
            });
        }, 0);
        return Promise.resolve(null);
    }

    function syncDetailCommandSurface(oController, mOptions) {
        ControllerViewStateRuntime.setMany(oController, {
            "/isEditMode": !!(mOptions && mOptions.isEditMode),
            "/isCreateMode": !!(mOptions && mOptions.isCreateMode),
            "/hasPersistedObject": !!(mOptions && mOptions.hasPersistedObject),
            "/deleteChecklistConfirmArmed": false
        });
    }

    function onRouteLeave(oController) {
        var oOwner = oController && oController.getOwnerComponent && oController.getOwnerComponent();
        var oStateModel = getModel(oController, STATE_MODEL);
        if (oOwner && typeof oOwner._stopLockScopedManagers === TYPE_FUNCTION) {
            oOwner._stopLockScopedManagers();
        }
        if (oOwner && typeof oOwner._releaseActiveLockOnLeave === TYPE_FUNCTION) {
            oOwner._releaseActiveLockOnLeave(oStateModel, oController.getModel && oController.getModel());
        }
        cleanupRouteArtifacts(oController);
    }

    function onMatched(oController, oEvent, mOptions) {
        var mContext = DetailMatchedRuntime.prepareMatchedContext(oController, oEvent);
        var sApplyMode = DetailMatchedRuntime.applyMatchedState(oController, mContext, {
            applyLayoutState: mOptions.applyLayoutState,
            createAccessState: DetailAccessViewState.createDefaultState,
            openChecklist: function (mInput) {
                return runDetailCommand(oController, "open", mInput);
            },
            scheduleAttachmentDropZoneBind: mOptions.scheduleAttachmentDropZoneBind,
            validationSummaryPath: mOptions.validationSummaryPath,
            writeInfoCards: function (aCards) {
                DetailInfoCardLayoutRuntime.writeCards(oController, aCards);
            }
        });

        if (sApplyMode === "layoutOnly") {
            markDetailReady(oController, { mode: "layoutOnly", dbKey: mContext.sId });
            return;
        }

        if (mContext.bCreate) {
            return Promise.resolve(DetailMatchedRuntime.openCreateDraft(oController, mContext, {
                openChecklist: function (mInput) {
                    return runDetailCommand(oController, "open", mInput);
                }
            })).then(function (oResult) {
                if (!oResult || oResult.ok !== false) {
                    syncDetailCommandSurface(oController, {
                        isEditMode: true,
                        isCreateMode: true,
                        hasPersistedObject: false
                    });
                    markDetailReady(oController, { mode: "create", dbKey: mContext.sId });
                }
                return oResult;
            }).catch(function (oError) {
                return handleMatchFailure(oController, oError);
            });
        }

        if (mContext.sPostOpenHydratedRootId && mContext.sPostOpenHydratedRootId === mContext.sId && mContext.sSelectedRootId === mContext.sId) {
            ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "");
            ControllerViewStateRuntime.set(oController, ViewPathContracts.DETAIL_SKELETON_BUSY, false);
            markDetailReady(oController, { mode: "hydratedReturn", dbKey: mContext.sId });
            oDetailEditSessionRuntime.restoreAnalyticsEditIfNeeded(oController, mContext.sId, {
                enterEdit: function (mInput) {
                    return runDetailCommand(oController, "enterEdit", mInput);
                },
                onToggleEdit: oController.onToggleEdit && oController.onToggleEdit.bind(oController)
            });
            return;
        }

        ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, mContext.sId);
        return runDetailCommand(oController, "open", { id: mContext.sId, dbKey: mContext.sId }).then(function (oResult) {
            var oAccessState;
            if (oResult && oResult.ok === false) {
                if (!oResult.error || oResult.error.code !== DETAIL_CODES.NO_VIEW_PERMISSION) {
                    oDetailEditSessionRuntime.clearAnalyticsReturnRestore(oController);
                    return oResult;
                }
                oAccessState = ControllerViewStateRuntime.get(oController, "/accessState", {}) || {};
                ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.DETAIL_ACCESS_GUARD, {
                    dbKey: String(oAccessState.dbKey || mContext.sId || "").trim(),
                    userId: String(oAccessState.userId || "").trim(),
                    canView: false,
                    canEdit: !!oAccessState.canEdit,
                    canDelete: !!oAccessState.canDelete,
                    reasonCode: String(oAccessState.reasonCode || DETAIL_CODES.NO_VIEW_PERMISSION).trim(),
                    message: String(oAccessState.message || "").trim(),
                    checkedAt: new Date().toISOString()
                });
                oDetailEditSessionRuntime.clearAnalyticsReturnRestore(oController);
                return oResult;
            }
            syncDetailCommandSurface(oController, {
                isEditMode: false,
                isCreateMode: false,
                hasPersistedObject: true
            });
            return oDetailEditSessionRuntime.restoreAnalyticsEditIfNeeded(oController, mContext.sId, {
                enterEdit: function (mInput) {
                    return runDetailCommand(oController, "enterEdit", mInput);
                },
                onToggleEdit: oController.onToggleEdit && oController.onToggleEdit.bind(oController)
            }).then(function () {
                return Promise.resolve((oResult && oResult.data && oResult.data.deferredRows)
                    ? scheduleDeferredRowHydration(oController, mContext.sId, mOptions)
                    : null
                ).then(function () {
                    markDetailReady(oController, { mode: "open", dbKey: mContext.sId });
                    return oResult;
                });
            });
        }).catch(function (oError) {
            return handleMatchFailure(oController, oError);
        });
    }

    return {
        cleanupRouteArtifacts: cleanupRouteArtifacts,
        onMatched: onMatched,
        onRouteLeave: onRouteLeave
    };
});
