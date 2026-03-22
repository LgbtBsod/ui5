sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ForceReadOnlyUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (ForceReadOnlyUseCase, StatePaths, WorkflowContracts, DetailMessageKeyConstants) {
    "use strict";

    QUnit.module("ForceReadOnlyUseCase");

    QUnit.test("preserveDirty downgrade keeps selected state instead of reverting to stale snapshot", function (assert) {
        var done = assert.async();
        var oUseCase = ForceReadOnlyUseCase();
        var oSelected = {
            root: { id: "CHK-1" },
            basic: { Profession: "CURRENT" }
        };
        var oSnapshot = {
            root: { id: "CHK-1" },
            basic: { Profession: "SNAPSHOT" }
        };

        oUseCase.execute({
            reason: "IDLE_TIMEOUT",
            messageKey: DetailMessageKeyConstants.LOCK_IDLE_TIMEOUT,
            preserveDirty: true,
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
                    if (sModelName === "detail" && sPath === "/base") {
                        return oSnapshot;
                    }
                    if (sModelName === "detail" && sPath === "/current") {
                        return oSelected;
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_SESSION_GUID) {
                        return "SESSION-1";
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
            var aSelectedEffects = (oResult.effects || []).filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "detail" && oEffect.path === "/current";
            });
            var oLastSelectedEffect = aSelectedEffects[aSelectedEffects.length - 1];
            var oDirtyEffect = (oResult.effects || []).filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "state" && oEffect.path === StatePaths.WORKFLOW_DIRTY;
            })[0];
            var oPendingIntentEffect = (oResult.effects || []).filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "state" && oEffect.path === StatePaths.PENDING_NAVIGATION_INTENT;
            })[0];

            assert.ok(oResult && oResult.ok, "force read-only succeeds");
            assert.ok(oLastSelectedEffect, "selected state effect is emitted");
            assert.deepEqual(oLastSelectedEffect.value, oSelected, "selected state stays on the current draft for preserveDirty downgrade");
            assert.ok(oDirtyEffect, "dirty effect is emitted");
            assert.strictEqual(oDirtyEffect.value, true, "dirty flag stays set for recoverable downgrade");
            assert.ok(oPendingIntentEffect, "pending navigation is cleared");
            assert.strictEqual(oPendingIntentEffect.value, null, "stale pending navigation intent is dropped");
            done();
        });
    });
});
