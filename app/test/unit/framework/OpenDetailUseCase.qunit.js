sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/OpenDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (OpenDetailUseCase, StatePaths) {
    "use strict";

    QUnit.module("OpenDetailUseCase");

    QUnit.test("opening detail clears stale lock incident and conflict state", function (assert) {
        var done = assert.async();
        var oUseCase = new OpenDetailUseCase();
        var oSnapshot = {
            root: { id: "ROOT-1", checklistId: "CHK-00001" },
            basic: { Profession: "Operator" }
        };

        oUseCase.execute({
            rootId: "ROOT-1"
        }, {
            repo: {
                checkChecklistPermission: function () {
                    return Promise.resolve({
                        rootId: "ROOT-1",
                        canView: true,
                        canEdit: true,
                        canDelete: true,
                        reasonCode: "AUTHORIZED"
                    });
                },
                loadDetailSnapshot: function () {
                    return Promise.resolve(oSnapshot);
                }
            },
            uiState: {
                get: function () {
                    return null;
                }
            }
        }).then(function (oResult) {
            var aEffects = oResult.effects || [];
            function findPatch(sModelName, sPath) {
                return aEffects.filter(function (oEffect) {
                    return oEffect.type === "modelPatch" && oEffect.modelName === sModelName && oEffect.path === sPath;
                }).pop();
            }

            assert.ok(oResult && oResult.ok, "open detail succeeds");
            assert.strictEqual(findPatch("state", "/isKilled").value, false, "stale killed flag is cleared");
            assert.strictEqual(findPatch("state", "/hasConflict").value, false, "stale conflict flag is cleared");
            assert.strictEqual(findPatch("state", StatePaths.WORKFLOW_LOCK_LOST_REASON).value, "", "lock lost reason is reset");
            assert.strictEqual(findPatch("state", StatePaths.PENDING_NAVIGATION_INTENT).value, null, "pending navigation intent is reset");
            assert.deepEqual(findPatch("uiState", "/lock").value, {
                ok: false,
                reason: "FREE",
                isKilled: false
            }, "ui lock state is reset to a neutral value");
            assert.deepEqual(findPatch("state", StatePaths.TAB_CONFLICT_STATE).value, {
                active: false,
                source: "",
                at: ""
            }, "cross-tab conflict state is reset");
            done();
        });
    });
});
