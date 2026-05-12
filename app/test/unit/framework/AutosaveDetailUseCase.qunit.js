sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AutosaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPayloadMapper"
], function (AutosaveDetailUseCase, StatePaths, WorkflowContracts, ODataChecklistPayloadMapper) {
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
                    if (sModelName === "state" && sPath === StatePaths.PERSISTENCE_HAS_VALID_LOCK) {
                        return true;
                    }
                    if (sModelName === "state" && sPath === StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES) {
                        return true;
                    }
                    if (sModelName === "state" && sPath === "/hasConflict") {
                        return false;
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

        oUseCase.execute({ rootId: "CHK-1", delta: { client_version: 7, root: { db_key: "CHK-1" } } }, oCtx).then(function (oResult) {
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
                    if (sModelName === "state" && sPath === StatePaths.PERSISTENCE_HAS_VALID_LOCK) {
                        return true;
                    }
                    if (sModelName === "state" && sPath === StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES) {
                        return true;
                    }
                    if (sModelName === "state" && sPath === "/hasConflict") {
                        return false;
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

    QUnit.test("autosave skips when lock health is invalid", function (assert) {
        var done = assert.async();
        var oUseCase = AutosaveDetailUseCase();
        var bCalled = false;

        oUseCase.execute({ rootId: "CHK-3" }, {
            repo: {
                autosaveChecklist: function () {
                    bCalled = true;
                    return Promise.resolve({});
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
                    if (sModelName === "state" && sPath === StatePaths.PERSISTENCE_HAS_VALID_LOCK) {
                        return false;
                    }
                    if (sModelName === "state" && sPath === StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES) {
                        return true;
                    }
                    if (sModelName === "state" && sPath === "/hasConflict") {
                        return false;
                    }
                    return null;
                }
            }
        }).then(function (oResult) {
            assert.notOk(bCalled, "autosave request is skipped");
            assert.ok(oResult && oResult.ok, "result stays successful");
            assert.strictEqual(!!(oResult.data && oResult.data.reason), true, "skip reason is returned");
            done();
        });
    });

    QUnit.test("shared payload mapper keeps LPC and profession aliases canonical", function (assert) {
        var oBasic = {
            LPC_KEY: "L1",
            PROF_KEY: "P1"
        };

        assert.strictEqual(ODataChecklistPayloadMapper.mapBasicFieldName("LPC_KEY"), "Lpc", "LPC alias resolves to OData field");
        assert.strictEqual(ODataChecklistPayloadMapper.mapBasicFieldName("PROF_KEY"), "Profession", "profession alias resolves to OData field");
        assert.strictEqual(ODataChecklistPayloadMapper.pickBasicFieldValue(oBasic, "Lpc"), "L1", "shared picker resolves LPC alias value");
        assert.strictEqual(ODataChecklistPayloadMapper.pickBasicFieldValue(oBasic, "Profession"), "P1", "shared picker resolves profession alias value");
    });

    QUnit.test("autosave merges staged attachment delta rows into canonical payload", function (assert) {
        var done = assert.async();
        var oUseCase = AutosaveDetailUseCase();
        var oCapturedArgs = null;

        oUseCase.execute({ rootId: "CHK-AUTO-ATT-1" }, {
            repo: {
                autosaveChecklist: function (mArgs) {
                    oCapturedArgs = mArgs;
                    return Promise.resolve({
                        autosavedAt: "2026-03-27T12:00:00.000Z",
                        serverSnapshot: {
                            root: { id: "CHK-AUTO-ATT-1", version_number: 2 },
                            basic: {},
                            attachments: []
                        }
                    });
                },
                loadAttachments: function () {
                    return Promise.resolve({ attachments: [] });
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
                    if (sModelName === "state" && sPath === StatePaths.PERSISTENCE_HAS_VALID_LOCK) {
                        return true;
                    }
                    if (sModelName === "state" && sPath === StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES) {
                        return true;
                    }
                    if (sModelName === "state" && sPath === "/hasConflict") {
                        return false;
                    }
                    if (sModelName === "state" && sPath === StatePaths.SESSION_ID) {
                        return "SESSION-AUTO-ATT-1";
                    }
                    if (sModelName === "detail" && sPath === "/current") {
                        return {
                            root: { id: "CHK-AUTO-ATT-1", version_number: 1 },
                            basic: {},
                            checks: [],
                            barriers: [],
                            attachments: []
                        };
                    }
                    if (sModelName === "detail" && sPath === "/base") {
                        return {
                            root: { id: "CHK-AUTO-ATT-1", version_number: 1 },
                            basic: {},
                            checks: [],
                            barriers: [],
                            attachments: []
                        };
                    }
                    if (sModelName === "view" && sPath === "/sessionAttachments") {
                        return [{
                            client_row_id: "AUTO-ATT-1",
                            fileName: "autosave.txt",
                            mimeType: "text/plain",
                            fileSize: 24,
                            categoryKey: "GEN",
                            folderKey: "CHK-AUTO-ATT-1",
                            uploadState: "pendingUpload",
                            _file: {}
                        }];
                    }
                    return null;
                }
            }
        }).then(function (oResult) {
            assert.ok(oResult && oResult.ok, "autosave succeeds");
            assert.ok(oCapturedArgs, "autosave request is captured");
            assert.strictEqual(oCapturedArgs.delta.attachments.length, 1, "staged attachment is merged into canonical autosave delta");
            assert.strictEqual(oCapturedArgs.delta.attachments[0].file_name, "autosave.txt", "canonical delta keeps attachment mutation fields");
            done();
        });
    });
});
