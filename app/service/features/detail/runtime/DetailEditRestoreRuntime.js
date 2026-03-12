sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DomainStatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DetailRuntimePolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
], function (DomainStatePaths, StatePaths, ModelStateRuntime, DetailRuntimePolicy, WorkflowContracts, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function isEditLockedState(oController) {
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        return WorkflowContracts.isEditLocked(sMode, sLockState);
    }

    function buildEditToggleEvent() {
        return {
            getParameter: function (sName) {
                return sName === "state" ? true : undefined;
            }
        };
    }

    function readAnalyticsReturnRestore(oController) {
        return ModelStateRuntime.read(oController, STATE_MODEL, DomainStatePaths.ANALYTICS_RETURN_RESTORE_EDIT, null) || null;
    }

    function clearAnalyticsReturnRestore(oController) {
        ModelStateRuntime.write(oController, STATE_MODEL, DomainStatePaths.ANALYTICS_RETURN_RESTORE_EDIT, null);
    }

    function requestAnalyticsEditRestore(oController, sRootId, mHooks) {
        if (!oController) {
            return Promise.resolve(false);
        }
        if (typeof mHooks.onToggleEdit === "function") {
            return Promise.resolve(mHooks.onToggleEdit(buildEditToggleEvent())).then(function () {
                return isEditLockedState(oController);
            });
        }
        return Promise.resolve(mHooks.enterEdit({ rootId: sRootId })).then(function () {
            return isEditLockedState(oController);
        });
    }

    function restoreAnalyticsEditIfNeeded(oController, sRootId, mHooks) {
        var oRestore = readAnalyticsReturnRestore(oController);
        var sRestoreRootId = String((oRestore && oRestore.rootId) || "").trim();
        var iAttempts;
        var oRestorePlan;

        if (!sRootId || !sRestoreRootId || sRestoreRootId !== sRootId) {
            return Promise.resolve(false);
        }
        if (isEditLockedState(oController)) {
            clearAnalyticsReturnRestore(oController);
            return Promise.resolve(true);
        }
        iAttempts = Number((oRestore && oRestore.attempts) || 0);
        oRestorePlan = DetailRuntimePolicy.analyticsEditRestorePlan({
            controller: oController,
            rootId: sRootId,
            restoreState: oRestore
        });
        if (iAttempts >= oRestorePlan.maxAttempts) {
            clearAnalyticsReturnRestore(oController);
            return Promise.resolve(false);
        }
        ModelStateRuntime.write(oController, STATE_MODEL, DomainStatePaths.ANALYTICS_RETURN_RESTORE_EDIT, {
            rootId: sRestoreRootId,
            requestedAt: oRestore && oRestore.requestedAt ? oRestore.requestedAt : new Date().toISOString(),
            attempts: iAttempts + 1
        });

        return requestAnalyticsEditRestore(oController, sRootId, mHooks).then(function (bRestored) {
            if (bRestored) {
                clearAnalyticsReturnRestore(oController);
                return true;
            }
            return new Promise(function (resolve) {
                setTimeout(function () {
                    requestAnalyticsEditRestore(oController, sRootId, mHooks)
                        .then(function (bRetryRestored) {
                            if (bRetryRestored) {
                                clearAnalyticsReturnRestore(oController);
                                resolve(true);
                                return;
                            }
                            resolve(false);
                        })
                        .catch(function () {
                            resolve(false);
                        });
                }, oRestorePlan.retryDelayMs);
            });
        }).catch(function () {
            return false;
        });
    }

    return {
        clearAnalyticsReturnRestore: clearAnalyticsReturnRestore,
        readAnalyticsReturnRestore: readAnalyticsReturnRestore,
        restoreAnalyticsEditIfNeeded: restoreAnalyticsEditIfNeeded
    };
});
