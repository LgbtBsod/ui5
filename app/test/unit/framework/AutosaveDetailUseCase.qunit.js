sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AutosaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (AutosaveDetailUseCase, StatePaths, WorkflowContracts) {
    "use strict";

    QUnit.module("AutosaveDetailUseCase");

    QUnit.test("autosave keeps dirty flag when staged attachments are still pending", function (assert) {
        var done = assert.async();
        var oUseCase = new AutosaveDetailUseCase();
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
                    if (sModelName === "selected" && sPath === "/") {
                        return oCurrent;
                    }
                    if (sModelName === "snapshot" && sPath === "/") {
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
});
