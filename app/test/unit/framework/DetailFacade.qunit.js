sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailFacade"
], function (DetailFacade) {
    "use strict";

    QUnit.module("framework/DetailFacade");

    QUnit.test("routes detail flows through grouped scenario runtimes", function (assert) {
        var aCalls = [];
        var oFacade = new DetailFacade({
            openMatchRuntime: {
                open: function () {
                    aCalls.push("open");
                    return Promise.resolve("open");
                }
            },
            editSessionRuntime: {
                enterEdit: function () {
                    aCalls.push("enterEdit");
                    return Promise.resolve("enterEdit");
                },
                confirmTakeover: function () {
                    aCalls.push("confirmTakeover");
                    return Promise.resolve("confirmTakeover");
                },
                cancelEnterEdit: function () {
                    aCalls.push("cancelEnterEdit");
                    return Promise.resolve("cancelEnterEdit");
                },
                discardChanges: function () {
                    aCalls.push("discardChanges");
                    return Promise.resolve("discardChanges");
                },
                onLockLost: function () {
                    aCalls.push("onLockLost");
                    return Promise.resolve("onLockLost");
                },
                forceReadOnly: function () {
                    aCalls.push("forceReadOnly");
                    return Promise.resolve("forceReadOnly");
                },
                close: function () {
                    aCalls.push("close");
                    return Promise.resolve("close");
                }
            },
            saveFlowRuntime: {
                save: function () {
                    aCalls.push("save");
                    return Promise.resolve("save");
                },
                validate: function () {
                    aCalls.push("validate");
                    return Promise.resolve("validate");
                },
                autosave: function () {
                    aCalls.push("autosave");
                    return Promise.resolve("autosave");
                },
                deleteChecklist: function () {
                    aCalls.push("deleteChecklist");
                    return Promise.resolve("deleteChecklist");
                },
                resolveConflict: function () {
                    aCalls.push("resolveConflict");
                    return Promise.resolve("resolveConflict");
                }
            },
            attachmentFlowRuntime: {
                attachmentLoad: function () {
                    aCalls.push("attachmentLoad");
                    return Promise.resolve("attachmentLoad");
                },
                attachmentUpload: function () {
                    aCalls.push("attachmentUpload");
                    return Promise.resolve("attachmentUpload");
                },
                attachmentDelete: function () {
                    aCalls.push("attachmentDelete");
                    return Promise.resolve("attachmentDelete");
                }
            },
            fieldAssistRuntime: {
                rowOps: function () {
                    aCalls.push("rowOps");
                    return Promise.resolve("rowOps");
                },
                valueHelpLocation: function () {
                    aCalls.push("valueHelpLocation");
                    return Promise.resolve("valueHelpLocation");
                },
                personSuggest: function () {
                    aCalls.push("personSuggest");
                    return Promise.resolve("personSuggest");
                }
            }
        });
        var done = assert.async();

        Promise.all([
            oFacade.open(),
            oFacade.enterEdit(),
            oFacade.confirmTakeover(),
            oFacade.cancelEnterEdit(),
            oFacade.discardChanges(),
            oFacade.onLockLost(),
            oFacade.forceReadOnly(),
            oFacade.close(),
            oFacade.save(),
            oFacade.validate(),
            oFacade.autosave(),
            oFacade.deleteChecklist(),
            oFacade.resolveConflict(),
            oFacade.attachmentLoad(),
            oFacade.attachmentUpload(),
            oFacade.attachmentDelete(),
            oFacade.rowOps(),
            oFacade.valueHelpLocation(),
            oFacade.personSuggest()
        ]).then(function () {
            assert.deepEqual(aCalls, [
                "open",
                "enterEdit",
                "confirmTakeover",
                "cancelEnterEdit",
                "discardChanges",
                "onLockLost",
                "forceReadOnly",
                "close",
                "save",
                "validate",
                "autosave",
                "deleteChecklist",
                "resolveConflict",
                "attachmentLoad",
                "attachmentUpload",
                "attachmentDelete",
                "rowOps",
                "valueHelpLocation",
                "personSuggest"
            ], "detail facade delegates to grouped flow runtimes");
            done();
        });
    });
});
