sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/CloseDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts"
], function (CloseDetailUseCase, StatePaths, WorkflowContracts, NavigationContracts) {
    "use strict";

    QUnit.module("CloseDetailUseCase");

    function uiState(mValues) {
        return {
            get: function (sModelName, sPath) {
                var sKey = sModelName + "|" + sPath;
                return Object.prototype.hasOwnProperty.call(mValues, sKey) ? mValues[sKey] : null;
            }
        };
    }

    function findEffect(aEffects, sType, sModelName, sPath) {
        return (aEffects || []).filter(function (oEffect) {
            return oEffect.type === sType
                && (sModelName === undefined || oEffect.modelName === sModelName)
                && (sPath === undefined || oEffect.path === sPath);
        })[0];
    }

    QUnit.test("releases the lock when closing an editable, persisted object", function (assert) {
        var done = assert.async();
        var oUseCase = CloseDetailUseCase();
        var oReleaseCall = null;

        oUseCase.execute({}, {
            uiState: uiState({
                "state|" + StatePaths.ACTIVE_OBJECT_ID: "CHK-1",
                "state|" + StatePaths.WORKFLOW_DETAIL_EDIT_MODE: WorkflowContracts.EDIT_MODES.EDIT,
                "state|" + StatePaths.WORKFLOW_DETAIL_LOCK_STATE: WorkflowContracts.LOCK_STATES.EDIT_LOCKED
            }),
            lock: {
                release: function (mArgs) {
                    oReleaseCall = mArgs;
                    return Promise.resolve({ ok: true, released: true });
                }
            }
        }).then(function (oResult) {
            var oNavigateEffect = findEffect(oResult.effects, "navigate");
            var oDirtyEffect = findEffect(oResult.effects, "modelPatch", "state", StatePaths.WORKFLOW_DIRTY);
            var oWarnEffect = findEffect(oResult.effects, "dialog");

            assert.ok(oResult.ok, "close succeeds");
            assert.ok(oReleaseCall, "lock release was requested for an editable, persisted object");
            assert.strictEqual(oReleaseCall.dbKey, "CHK-1", "release targets the active object");
            assert.ok(oNavigateEffect, "navigation effect is emitted");
            assert.strictEqual(oNavigateEffect.route, NavigationContracts.ROUTES.SEARCH, "navigates back to search");
            assert.strictEqual(oDirtyEffect && oDirtyEffect.value, false, "dirty flag is cleared");
            assert.notOk(oWarnEffect, "no warning is raised when release succeeds");
            done();
        });
    });

    QUnit.test("skips the lock release for a read-only session", function (assert) {
        var done = assert.async();
        var oUseCase = CloseDetailUseCase();
        var bReleaseCalled = false;

        oUseCase.execute({}, {
            uiState: uiState({
                "state|" + StatePaths.ACTIVE_OBJECT_ID: "CHK-2",
                "state|" + StatePaths.WORKFLOW_DETAIL_EDIT_MODE: WorkflowContracts.EDIT_MODES.READ,
                "state|" + StatePaths.WORKFLOW_DETAIL_LOCK_STATE: WorkflowContracts.LOCK_STATES.IDLE
            }),
            lock: {
                release: function () {
                    bReleaseCalled = true;
                    return Promise.resolve({ ok: true, released: true });
                }
            }
        }).then(function (oResult) {
            assert.ok(oResult.ok, "close still succeeds");
            assert.notOk(bReleaseCalled, "no release call for an already read-only session");
            done();
        });
    });

    QUnit.test("skips the lock release for a create-mode (unpersisted) object", function (assert) {
        var done = assert.async();
        var oUseCase = CloseDetailUseCase();
        var bReleaseCalled = false;

        oUseCase.execute({}, {
            uiState: uiState({
                "state|" + StatePaths.ACTIVE_OBJECT_ID: "$$create$$1",
                "state|" + StatePaths.WORKFLOW_DETAIL_EDIT_MODE: WorkflowContracts.EDIT_MODES.CREATE,
                "state|" + StatePaths.WORKFLOW_DETAIL_LOCK_STATE: WorkflowContracts.LOCK_STATES.EDIT_LOCKED
            }),
            lock: {
                release: function () {
                    bReleaseCalled = true;
                    return Promise.resolve({ ok: true, released: true });
                }
            }
        }).then(function (oResult) {
            assert.ok(oResult.ok, "close succeeds without a persisted key");
            assert.notOk(bReleaseCalled, "no release call for an object that was never persisted");
            done();
        });
    });

    QUnit.test("surfaces a warning effect when lock release fails", function (assert) {
        var done = assert.async();
        var oUseCase = CloseDetailUseCase();

        oUseCase.execute({}, {
            uiState: uiState({
                "state|" + StatePaths.ACTIVE_OBJECT_ID: "CHK-3",
                "state|" + StatePaths.WORKFLOW_DETAIL_EDIT_MODE: WorkflowContracts.EDIT_MODES.EDIT,
                "state|" + StatePaths.WORKFLOW_DETAIL_LOCK_STATE: WorkflowContracts.LOCK_STATES.EDIT_LOCKED
            }),
            lock: {
                release: function () {
                    return Promise.resolve({ ok: false, released: false });
                }
            }
        }).then(function (oResult) {
            var oWarnEffect = findEffect(oResult.effects, "dialog");
            var oNavigateEffect = findEffect(oResult.effects, "navigate");

            assert.ok(oResult.ok, "close still completes so the user is not stuck on the page");
            assert.ok(oWarnEffect, "a warning effect is raised for the failed release");
            assert.ok(oNavigateEffect, "navigation still proceeds despite the release failure");
            done();
        });
    });

    QUnit.test("does not reject when the lock port throws", function (assert) {
        var done = assert.async();
        var oUseCase = CloseDetailUseCase();

        oUseCase.execute({}, {
            uiState: uiState({
                "state|" + StatePaths.ACTIVE_OBJECT_ID: "CHK-4",
                "state|" + StatePaths.WORKFLOW_DETAIL_EDIT_MODE: WorkflowContracts.EDIT_MODES.EDIT,
                "state|" + StatePaths.WORKFLOW_DETAIL_LOCK_STATE: WorkflowContracts.LOCK_STATES.EDIT_LOCKED
            }),
            lock: {
                release: function () {
                    return Promise.reject(new Error("network down"));
                }
            }
        }).then(function (oResult) {
            var oWarnEffect = findEffect(oResult.effects, "dialog");
            assert.ok(oResult.ok, "close completes even when the release call rejects");
            assert.ok(oWarnEffect, "a warning effect is raised for the technical failure");
            done();
        });
    });
});
