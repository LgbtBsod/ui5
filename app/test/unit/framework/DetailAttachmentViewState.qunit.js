sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentViewState"
], function (JSONModel, DetailAttachmentViewState) {
    "use strict";

    function createController(mSeed) {
        var mModels = {
            state: new JSONModel((mSeed && mSeed.state) || {}),
            detail: new JSONModel((mSeed && mSeed.detail) || {}),
            view: new JSONModel((mSeed && mSeed.view) || {})
        };

        return {
            getView: function () {
                return {
                    getModel: function (sName) {
                        return sName === "view" || sName === "detail" ? mModels[sName] : null;
                    }
                };
            },
            getOwnerComponent: function () {
                return {
                    getModel: function (sName) {
                        return sName === "state" ? mModels[sName] : null;
                    }
                };
            },
            getModel: function (sName) {
                return mModels[sName] || null;
            }
        };
    }

    QUnit.module("DetailAttachmentViewState");

    QUnit.test("enables attachment actions only for editable detail with active root", function (assert) {
        var oController = createController({
            state: {
                workflow: { detail: { editMode: "EDIT" } },
                selectedId: "ROOT-1",
                activeObjectId: "ROOT-1"
            },
            detail: {
                current: { root: { id: "ROOT-1" } }
            },
            view: {
                attachmentsExpanded: false,
                narrowDetailViewport: true
            }
        });

        DetailAttachmentViewState.sync(oController);

        assert.ok(oController.getModel("view").getProperty("/attachmentActionsEnabled"), "actions enabled");
        assert.ok(oController.getModel("view").getProperty("/attachmentMetaEditable"), "meta enabled");
        assert.ok(oController.getModel("view").getProperty("/showSessionAttachments"), "collapsed state keeps session list visible");
        assert.strictEqual(oController.getModel("view").getProperty("/attachmentActionsColumnWidth"), "9rem", "narrow viewport width applied");
    });

    QUnit.test("disables attachment actions in read mode", function (assert) {
        var oController = createController({
            state: {
                workflow: { detail: { editMode: "READ" } },
                selectedId: "ROOT-1"
            },
            detail: {
                current: { root: { id: "ROOT-1" } }
            },
            view: {}
        });

        DetailAttachmentViewState.sync(oController);

        assert.notOk(oController.getModel("view").getProperty("/attachmentActionsEnabled"), "actions disabled");
        assert.notOk(oController.getModel("view").getProperty("/attachmentMetaEditable"), "meta disabled");
    });
});
