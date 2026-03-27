sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/OpenDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (OpenDetailUseCase, StatePaths) {
    "use strict";

    QUnit.module("OpenDetailUseCase");

    QUnit.test("opening detail clears stale lock incident and conflict state", function (assert) {
        var done = assert.async();
        var oUseCase = OpenDetailUseCase();
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
            assert.deepEqual(findPatch("shell", "/lock").value, {
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

    QUnit.test("opening already hydrated editable detail preserves edit lock state", function (assert) {
        var done = assert.async();
        var oUseCase = OpenDetailUseCase();
        var oSnapshot = {
            root: { id: "ROOT-2", checklistId: "CHK-00002" },
            attachments: [{ AttachmentKey: "ATT-1", FileName: "copy.txt" }]
        };

        oUseCase.execute({
            rootId: "ROOT-2"
        }, {
            repo: {
                checkChecklistPermission: function () {
                    return Promise.resolve({
                        rootId: "ROOT-2",
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
                get: function (sModelName, sPath) {
                    if (sModelName === "state" && sPath === "/postOpenHydratedRootId") {
                        return "ROOT-2";
                    }
                    if (sModelName === "state" && sPath === "/activeObjectId") {
                        return "ROOT-2";
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                        return "EDIT";
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
                        return "EDIT_LOCKED";
                    }
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
            assert.strictEqual(findPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE).value, "EDIT", "edit mode is preserved");
            assert.strictEqual(findPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE).value, "EDIT_LOCKED", "lock state is preserved");
            assert.strictEqual(findPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED).value, true, "autosave stays enabled");
            assert.deepEqual(findPatch("detail", "/current/attachments").value, oSnapshot.attachments, "snapshot attachments are preserved");
            assert.deepEqual(findPatch("view", "/sessionAttachments").value, [], "persisted attachments are not mirrored into session attachments");
            done();
        });
    });

    QUnit.test("opening same root preserves missing basic fields from current session snapshot", function (assert) {
        var done = assert.async();
        var oUseCase = OpenDetailUseCase();
        var oSnapshot = {
            root: { id: "ROOT-3", checklistId: "CHK-00003" },
            basic: { equipment: "" }
        };
        var oCurrentSelected = {
            root: { id: "ROOT-3", checklistId: "CHK-00003" },
            basic: { equipment: "Session equipment" }
        };

        oUseCase.execute({
            rootId: "ROOT-3"
        }, {
            repo: {
                checkChecklistPermission: function () {
                    return Promise.resolve({
                        rootId: "ROOT-3",
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
                get: function (sModelName, sPath) {
                    if (sModelName === "detail" && sPath === "/current") {
                        return oCurrentSelected;
                    }
                    if (sModelName === "detail" && sPath === "/base") {
                        return oCurrentSelected;
                    }
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
            assert.strictEqual(findPatch("detail", "/current").value.basic.equipment, "Session equipment", "missing basic equipment is preserved for the same root");
            assert.strictEqual(findPatch("detail", "/base").value.basic.equipment, "Session equipment", "base snapshot keeps preserved basic equipment");
            done();
        });
    });

    QUnit.test("opening detail uses valid cache snapshot without backend reload", function (assert) {
        var done = assert.async();
        var oUseCase = OpenDetailUseCase();
        var bLoaded = false;
        var oCachedSnapshot = {
            root: { id: "ROOT-CACHE-1", checklistId: "CHK-CACHE-1" },
            basic: { equipment: "Cached equipment" }
        };

        oUseCase.execute({
            rootId: "ROOT-CACHE-1"
        }, {
            repo: {
                checkChecklistPermission: function () {
                    return Promise.resolve({
                        rootId: "ROOT-CACHE-1",
                        canView: true,
                        canEdit: true,
                        canDelete: true,
                        reasonCode: "AUTHORIZED"
                    });
                },
                loadDetailSnapshot: function () {
                    bLoaded = true;
                    return Promise.resolve({});
                }
            },
            cacheValidation: {
                execute: function () {
                    return Promise.resolve({
                        ok: true,
                        data: {
                            valid: true,
                            snapshot: oCachedSnapshot
                        }
                    });
                }
            },
            uiState: {
                get: function () {
                    return null;
                }
            }
        }).then(function (oResult) {
            var oSelectedPatch = (oResult.effects || []).filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "detail" && oEffect.path === "/current";
            }).pop();

            assert.ok(oResult && oResult.ok, "open detail succeeds");
            assert.notOk(bLoaded, "backend reload is skipped for valid cache");
            assert.strictEqual(oSelectedPatch.value.basic.equipment, "Cached equipment", "cached snapshot is used");
            done();
        });
    });

    QUnit.test("opening detail uses runtime cache tolerance when provided", function (assert) {
        var done = assert.async();
        var oUseCase = OpenDetailUseCase();
        var aValidationCalls = [];

        oUseCase.execute({
            rootId: "ROOT-CACHE-2"
        }, {
            repo: {
                checkChecklistPermission: function () {
                    return Promise.resolve({
                        rootId: "ROOT-CACHE-2",
                        canView: true,
                        canEdit: true,
                        canDelete: true,
                        reasonCode: "AUTHORIZED"
                    });
                },
                loadDetailSnapshot: function () {
                    return Promise.resolve({
                        root: { id: "ROOT-CACHE-2", checklistId: "CHK-CACHE-2" },
                        basic: {}
                    });
                }
            },
            runtimeSettings: {
                cacheToleranceMs: 9100
            },
            cacheValidation: {
                execute: function (mInput) {
                    aValidationCalls.push(mInput);
                    return Promise.resolve({ ok: true, data: { valid: false } });
                }
            },
            uiState: {
                get: function () {
                    return null;
                }
            }
        }).then(function (oResult) {
            assert.ok(oResult && oResult.ok, "open detail succeeds");
            assert.strictEqual(aValidationCalls.length, 1, "cache validation runs once");
            assert.strictEqual(aValidationCalls[0].toleranceMs, 9100, "runtime cache tolerance is forwarded");
            done();
        });
    });

    QUnit.test("opening detail defers checks and barriers on initial read open", function (assert) {
        var done = assert.async();
        var oUseCase = OpenDetailUseCase();

        oUseCase.execute({
            rootId: "ROOT-PHASE-1"
        }, {
            repo: {
                checkChecklistPermission: function () {
                    return Promise.resolve({
                        rootId: "ROOT-PHASE-1",
                        canView: true,
                        canEdit: true,
                        canDelete: true,
                        reasonCode: "AUTHORIZED"
                    });
                },
                loadDetailSnapshot: function () {
                    return Promise.resolve({
                        root: { id: "ROOT-PHASE-1", checklistId: "CHK-PHASE-1" },
                        basic: { equipment: "Pump" },
                        checks: [{ Key: "CHECK-1" }],
                        barriers: [{ Key: "BARRIER-1" }]
                    });
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
            assert.strictEqual(oResult.data.deferredRows, true, "result marks deferred child hydration");
            assert.deepEqual(findPatch("detail", "/current").value.checks, [], "checks are deferred from initial open snapshot");
            assert.deepEqual(findPatch("detail", "/current").value.barriers, [], "barriers are deferred from initial open snapshot");
            assert.strictEqual(findPatch("view", "/checksBusy").value, true, "checks busy stays active until phase 2");
            assert.strictEqual(findPatch("view", "/barriersBusy").value, true, "barriers busy stays active until phase 2");
            done();
        });
    });
});
