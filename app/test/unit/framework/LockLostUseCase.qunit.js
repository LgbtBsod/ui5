sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/LockLostUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants"
], function (LockLostUseCase, StatePaths, WorkflowContracts) {
    "use strict";

    QUnit.module("LockLostUseCase");

    QUnit.test("lock lost forces snapshot rollback and clears pending navigation intent", function (assert) {
        var done = assert.async();
        var oUseCase = LockLostUseCase();
        var oSnapshot = {
            root: { id: "CHK-1" },
            basic: { Profession: "SNAPSHOT" }
        };

        oUseCase.execute({
            reason: "LOCK_EXPIRED",
            rootId: "CHK-1",
            sessionGuid: "SESSION-1"
        }, {
            uiState: {
                get: function (sModelName, sPath) {
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                        return WorkflowContracts.EDIT_MODES.EDIT;
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
                        return WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
                    }
                    if (sModelName === "snapshot" && sPath === "/") {
                        return oSnapshot;
                    }
                    if (sModelName === "selected" && sPath === "/") {
                        return {
                            root: { id: "CHK-1" },
                            basic: { Profession: "DIRTY" }
                        };
                    }
                    return null;
                }
            },
            lock: {
                release: function () {
                    return Promise.resolve({ ok: true, released: true });
                }
            }
        }).then(function (oResult) {
            var oSelectedEffect = (oResult.effects || []).filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "selected" && oEffect.path === "/";
            }).pop();
            var oPendingIntentEffect = (oResult.effects || []).filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "state" && oEffect.path === StatePaths.PENDING_NAVIGATION_INTENT;
            })[0];

            assert.ok(oResult && oResult.ok, "lock lost handling succeeds");
            assert.deepEqual(oSelectedEffect && oSelectedEffect.value, oSnapshot, "lock lost reverts the draft to snapshot");
            assert.ok(oPendingIntentEffect, "pending navigation cleanup effect exists");
            assert.strictEqual(oPendingIntentEffect.value, null, "stale pending navigation is cleared");
            done();
        });
    });
});
