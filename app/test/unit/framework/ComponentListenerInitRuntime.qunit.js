sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerInitRuntime"
], function (JSONModel, ComponentListenerInitRuntime) {
    "use strict";

    QUnit.module("framework/ComponentListenerInitRuntime");

    QUnit.test("beforeunload handler prompts only for editable dirty state", function (assert) {
        var oStateModel = new JSONModel({
            workflow: {
                detail: {
                    editMode: "EDIT"
                }
            },
            isDirty: true
        });
        var fnHandler = ComponentListenerInitRuntime.createBeforeUnloadHandler(oStateModel, {
            statePaths: {
                WORKFLOW_DETAIL_EDIT_MODE: "/workflow/detail/editMode"
            }
        });
        var oEvent = {
            prevented: false,
            returnValue: "",
            preventDefault: function () {
                this.prevented = true;
            }
        };

        assert.strictEqual(fnHandler(oEvent), "You have unsaved changes", "dirty editable state triggers browser prompt");
        assert.strictEqual(oEvent.prevented, true, "navigation is blocked");

        oStateModel.setProperty("/isDirty", false);
        oEvent.prevented = false;
        oEvent.returnValue = "";
        assert.strictEqual(fnHandler(oEvent), undefined, "clean state skips prompt");
        assert.strictEqual(oEvent.prevented, false, "clean state does not block unload");
    });
});
