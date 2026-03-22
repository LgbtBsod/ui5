sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailEditRestoreRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailMatchedRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (DetailAccessViewState, DetailCommandPolicy, DetailEditRestoreRuntime, DetailInfoCardLayoutRuntime, DetailMatchedRuntime, ModelPathContracts, ViewPathContracts, ControllerModelRuntime, ControllerViewStateRuntime, ModelStateRuntime, ReadinessTelemetryRuntime, SchedulingRuntime, StatePaths, ReadinessTelemetryContracts, JsRuntime, ModelContracts, DetailUseCaseConstants) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_CODES = DetailUseCaseConstants.CODES;

    function markDetailReady(oController, mDetails) {
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.DETAIL_READY, mDetails);
    }

    function handleMatchFailure(oController, oError) {
        DetailEditRestoreRuntime.clearAnalyticsReturnRestore(oController);
        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false);
        ControllerViewStateRuntime.set(oController, ViewPathContracts.DETAIL_SKELETON_BUSY, false);
        if (oController && typeof oController.showI18nError === TYPE_FUNCTION) {
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
        if (typeof oController._clearLocationValueHelpSearchTimer === TYPE_FUNCTION) {
            oController._clearLocationValueHelpSearchTimer();
        }
        oController._iLocationVhTableSyncTimer = SchedulingRuntime.clearTimer(oController._iLocationVhTableSyncTimer);
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
        var oStateModel = ControllerModelRuntime.state(oController);
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
                return DetailCommandPolicy.open(oController, mInput);
            },
            scheduleAttachmentDropZoneBind: mOptions.scheduleAttachmentDropZoneBind,
            validationSummaryPath: mOptions.validationSummaryPath,
            writeInfoCards: function (aCards) {
                DetailInfoCardLayoutRuntime.writeCards(oController, aCards);
            }
        });

        if (sApplyMode === "layoutOnly") {
            markDetailReady(oController, { mode: "layoutOnly", rootId: mContext.sId });
            return;
        }

        if (mContext.bCreate) {
            return Promise.resolve(DetailMatchedRuntime.openCreateDraft(oController, mContext, {
                openChecklist: function (mInput) {
                    return DetailCommandPolicy.open(oController, mInput);
                }
            })).then(function (oResult) {
                if (!oResult || oResult.ok !== false) {
                    syncDetailCommandSurface(oController, {
                        isEditMode: true,
                        isCreateMode: true,
                        hasPersistedObject: false
                    });
                    markDetailReady(oController, { mode: "create", rootId: mContext.sId });
                }
                return oResult;
            }).catch(function (oError) {
                return handleMatchFailure(oController, oError);
            });
        }

        if (mContext.sPostOpenHydratedRootId && mContext.sPostOpenHydratedRootId === mContext.sId && mContext.sSelectedRootId === mContext.sId) {
            ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "");
            ControllerViewStateRuntime.set(oController, ViewPathContracts.DETAIL_SKELETON_BUSY, false);
            markDetailReady(oController, { mode: "hydratedReturn", rootId: mContext.sId });
            DetailEditRestoreRuntime.restoreAnalyticsEditIfNeeded(oController, mContext.sId, {
                enterEdit: function (mInput) {
                    return DetailCommandPolicy.enterEdit(oController, mInput);
                },
                onToggleEdit: oController.onToggleEdit && oController.onToggleEdit.bind(oController)
            });
            return;
        }

        ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, mContext.sId);
        return DetailCommandPolicy.open(oController, { id: mContext.sId, rootId: mContext.sId }).then(function (oResult) {
            var oAccessState;
            if (oResult && oResult.ok === false) {
                if (!oResult.error || oResult.error.code !== DETAIL_CODES.NO_VIEW_PERMISSION) {
                    DetailEditRestoreRuntime.clearAnalyticsReturnRestore(oController);
                    return oResult;
                }
                oAccessState = ControllerViewStateRuntime.get(oController, "/accessState", {}) || {};
                ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.DETAIL_ACCESS_GUARD, {
                    rootId: String(oAccessState.rootId || mContext.sId || "").trim(),
                    userId: String(oAccessState.userId || "").trim(),
                    canView: false,
                    canEdit: !!oAccessState.canEdit,
                    canDelete: !!oAccessState.canDelete,
                    reasonCode: String(oAccessState.reasonCode || DETAIL_CODES.NO_VIEW_PERMISSION).trim(),
                    message: String(oAccessState.message || "").trim(),
                    checkedAt: new Date().toISOString()
                });
                DetailEditRestoreRuntime.clearAnalyticsReturnRestore(oController);
                return oResult;
            }
            syncDetailCommandSurface(oController, {
                isEditMode: false,
                isCreateMode: false,
                hasPersistedObject: true
            });
            return DetailEditRestoreRuntime.restoreAnalyticsEditIfNeeded(oController, mContext.sId, {
                enterEdit: function (mInput) {
                    return DetailCommandPolicy.enterEdit(oController, mInput);
                },
                onToggleEdit: oController.onToggleEdit && oController.onToggleEdit.bind(oController)
            }).then(function () {
                markDetailReady(oController, { mode: "open", rootId: mContext.sId });
                return oResult;
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
