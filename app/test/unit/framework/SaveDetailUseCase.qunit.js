sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/SaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (SaveDetailUseCase, StatePaths) {
    "use strict";

    QUnit.module("SaveDetailUseCase");

    QUnit.test("create save does not block on missing required fields and writes search return context from current snapshot", function (assert) {
        var done = assert.async();
        var oUseCase = SaveDetailUseCase();
        var bCreateCalled = false;
        var aCacheWrites = [];
        var oCurrentChecklist = {
            Id: "CHK-09001",
            root: { id: "__CREATE", status: "DRAFT" },
            basic: {
                equipment: ""
            },
            attachments: []
        };

        oUseCase.execute({
            rootId: "__CREATE",
            delta: {
                root: { status: "DRAFT" },
                basic: { equipment: "" }
            }
        }, {
            repo: {
                createChecklist: function () {
                    bCreateCalled = true;
                    return Promise.resolve({
                        serverSnapshot: {
                            root: { id: "ROOT-9001", status: "DRAFT" },
                            basic: { equipment: "" }
                        }
                    });
                },
                loadAttachments: function () {
                    return Promise.resolve([]);
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
                    if (sModelName === "detail" && sPath === "/current") {
                        return oCurrentChecklist;
                    }
                    if (sModelName === "detail" && sPath === "/base") {
                        return {
                            root: { id: "__CREATE", status: "DRAFT" },
                            basic: {},
                            attachments: []
                        };
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                        return "CREATE";
                    }
                    if (sModelName === "state" && sPath === StatePaths.REQUIRED_FIELDS) {
                        return ["/basic/equipment"];
                    }
                    return null;
                }
            }
        }).then(function (oResult) {
            var oReturnPatch = (oResult.effects || []).filter(function (oEffect) {
                return oEffect.type === "modelPatch"
                    && oEffect.modelName === "state"
                    && oEffect.path === StatePaths.SEARCH_RETURN_CONTEXT;
            }).pop();

            assert.ok(bCreateCalled, "create request is still executed on regular save");
            assert.ok(oResult && oResult.ok, "save succeeds");
            assert.ok(oReturnPatch, "search return context is written");
            assert.strictEqual(oReturnPatch.value.rootId, "ROOT-9001", "return context keeps server root id");
            assert.strictEqual(oReturnPatch.value.checklistId, "CHK-09001", "return context falls back to current display id");
            assert.strictEqual(aCacheWrites.length, 1, "fresh snapshot is written to cache after create save");
            assert.strictEqual(aCacheWrites[0].rootId, "ROOT-9001", "cache uses persisted root id");
            done();
        });
    });

    QUnit.test("save merges staged attachment delta rows into canonical payload", function (assert) {
        var done = assert.async();
        var oUseCase = SaveDetailUseCase();
        var oCapturedArgs = null;

        oUseCase.execute({ rootId: "CHK-ATT-1" }, {
            repo: {
                saveChecklist: function (mArgs) {
                    oCapturedArgs = mArgs;
                    return Promise.resolve({
                        serverSnapshot: {
                            root: { id: "CHK-ATT-1", version_number: 8 },
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
                    if (sModelName === "detail" && sPath === "/current") {
                        return {
                            root: { id: "CHK-ATT-1", version_number: 7 },
                            basic: {},
                            checks: [],
                            barriers: [],
                            attachments: []
                        };
                    }
                    if (sModelName === "detail" && sPath === "/base") {
                        return {
                            root: { id: "CHK-ATT-1", version_number: 7 },
                            basic: {},
                            checks: [],
                            barriers: [],
                            attachments: []
                        };
                    }
                    if (sModelName === "view" && sPath === "/sessionAttachments") {
                        return [{
                            client_row_id: "ATT-STAGED-1",
                            fileName: "evidence.txt",
                            mimeType: "text/plain",
                            fileSize: 42,
                            categoryKey: "GEN",
                            folderKey: "CHK-ATT-1",
                            uploadState: "pendingUpload",
                            _file: {}
                        }];
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                        return "EDIT";
                    }
                    if (sModelName === "state" && sPath === StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
                        return "EDIT_LOCKED";
                    }
                    if (sModelName === "state" && sPath === StatePaths.SESSION_ID) {
                        return "SESSION-ATT-1";
                    }
                    return null;
                }
            }
        }).then(function () {
            assert.ok(oCapturedArgs, "save request is captured");
            assert.ok(Array.isArray(oCapturedArgs.delta.attachments), "attachment delta array is present");
            assert.strictEqual(oCapturedArgs.delta.attachments.length, 1, "staged attachment is merged into canonical delta");
            assert.strictEqual(oCapturedArgs.delta.attachments[0].file_name, "evidence.txt", "merged attachment keeps canonical file name field");
            done();
        });
    });
});
