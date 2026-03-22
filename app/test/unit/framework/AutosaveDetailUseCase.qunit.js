sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AutosaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (AutosaveDetailUseCase, StatePaths, WorkflowContracts) {
    "use strict";

    QUnit.module("AutosaveDetailUseCase");

    QUnit.test("autosave keeps dirty flag when staged attachments are still pending", function (assert) {
        var done = assert.async();
        var oUseCase = AutosaveDetailUseCase();
        var oCurrent = {
            root: { id: "CHK-1", version_number: 7 },
            basic: {},
            attachments: [
                {
                    fileName: "evidence.txt",
                    mediaSrc: "blob:test",
                    uploadState: "pendingUpload"
                }
            ]
        };
        var oCtx = {
            repo: {
                autosaveChecklist: function () {
                    return Promise.resolve({
                        autosavedAt: "2026-03-19T10:00:00.000Z",
                        serverSnapshot: {
                            root: { id: "CHK-1", version_number: 8 },
                            basic: {}
                        }
                    });
                }
            },
            uiState: {
                get: function (sModelName, sPath) {
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                        return WorkflowContracts.EDIT_MODES.EDIT;
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
                        return WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DIRTY) {
                        return true;
                    }
                    if (sModelName === "detail" && sPath === "/current") {
                        return oCurrent;
                    }
                    if (sModelName === "detail" && sPath === "/base") {
                        return {
                            root: { id: "CHK-1", version_number: 7 },
                            basic: {},
                            attachments: []
                        };
                    }
                    if (sModelName === "state" && sPath === StatePaths.SESSION_ID) {
                        return "SESSION-1";
                    }
                    return null;
                }
            }
        };

        oUseCase.execute({ rootId: "CHK-1", delta: { client_version: 7, root: { pcct_uuid: "CHK-1" } } }, oCtx).then(function (oResult) {
            var oDirtyEffect = (oResult.effects || []).filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "state" && oEffect.path === StatePaths.WORKFLOW_DIRTY;
            })[0];

            assert.ok(oResult && oResult.ok, "autosave succeeds");
            assert.ok(oDirtyEffect, "dirty effect is emitted");
            assert.strictEqual(oDirtyEffect.value, true, "dirty flag stays true while staged attachments still require manual sync");
            done();
        });
    });

    QUnit.test("autosave does not require client version when lock is active", function (assert) {
        var done = assert.async();
        var oUseCase = AutosaveDetailUseCase();
        var bCalled = false;
        var aCacheWrites = [];
        var oCtx = {
            repo: {
                autosaveChecklist: function (mArgs) {
                    bCalled = true;
                    assert.strictEqual(mArgs.rootId, "CHK-2", "autosave uses current root id");
                    return Promise.resolve({
                        autosavedAt: "2026-03-19T10:05:00.000Z",
                        serverSnapshot: {
                            root: { id: "CHK-2", version_number: 1 },
                            basic: { equipment: "Edited equipment" }
                        }
                    });
                }
            },
            cacheWrite: {
                execute: function (mInput) {
                    aCacheWrites.push(mInput);
                    return Promise.resolve({ ok: true });
                }
            },
            uiState: {
                get: function (sModelName, sPath) {
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                        return WorkflowContracts.EDIT_MODES.EDIT;
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
                        return WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DIRTY) {
                        return true;
                    }
                    if (sModelName === "state" && sPath === StatePaths.SESSION_ID) {
                        return "SESSION-2";
                    }
                    if (sModelName === "detail" && sPath === "/current") {
                        return {
                            root: { id: "CHK-2" },
                            basic: { equipment: "Edited equipment" },
                            checks: [],
                            barriers: [],
                            attachments: []
                        };
                    }
                    if (sModelName === "detail" && sPath === "/base") {
                        return {
                            root: { id: "CHK-2" },
                            basic: { equipment: "Original equipment" },
                            checks: [],
                            barriers: [],
                            attachments: []
                        };
                    }
                    return null;
                }
            }
        };

        oUseCase.execute({ rootId: "CHK-2" }, oCtx).then(function (oResult) {
            var aEffects = oResult.effects || [];
            var oSnapshotPatch = aEffects.filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "detail" && oEffect.path === "/base";
            }).pop();
            var oRefreshPatch = aEffects.filter(function (oEffect) {
                return oEffect.type === "modelPatch" && oEffect.modelName === "state" && oEffect.path === StatePaths.SEARCH_RETURN_CONTEXT;
            }).pop();

            assert.ok(bCalled, "autosave request was sent even without client version");
            assert.ok(oResult && oResult.ok, "autosave succeeds");
            assert.strictEqual(oSnapshotPatch.value.basic.equipment, "Edited equipment", "current basic fields are preserved when backend snapshot is partial");
            assert.strictEqual(oRefreshPatch, undefined, "autosave does not arm search return rediscovery");
            assert.strictEqual(aCacheWrites.length, 1, "autosave refreshes cache snapshot");
            assert.strictEqual(aCacheWrites[0].rootId, "CHK-2", "autosave cache write keeps persisted root id");
            done();
        });
    });
});
