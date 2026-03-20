sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/SearchUiConfig"
], function (JSONModel, ComponentListenerInitRuntime, SearchUiConfig) {
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

    QUnit.test("initializeListeners seeds search layout without SmartSearchAdapter", function (assert) {
        var oStateModel = new JSONModel({
            workflow: {
                detail: {
                    editMode: "READ"
                }
            },
            isDirty: false
        });
        var oUiStateModel = new JSONModel({});
        var oLayoutModel = new JSONModel({
            smartFilter: {},
            smartTable: {}
        });
        var oCacheModel = new JSONModel({});
        var oMasterDataModel = new JSONModel({});
        var oEnvModel = new JSONModel({});
        var aAdded = [];
        var aRemoved = [];
        var fnOriginalAdd = window.addEventListener;
        var fnOriginalRemove = window.removeEventListener;
        var oComponent = {
            setModel: function () {},
            _oAutoSave: {
                touch: function () {}
            },
            _syncLockScopedManagers: function () {}
        };

        window.addEventListener = function (sEventName) {
            aAdded.push(sEventName);
        };
        window.removeEventListener = function (sEventName) {
            aRemoved.push(sEventName);
        };

        ComponentListenerInitRuntime.initializeListeners({
            component: oComponent,
            stateModel: oStateModel,
            uiStateModel: oUiStateModel,
            layoutModel: oLayoutModel,
            cacheModel: oCacheModel,
            masterDataModel: oMasterDataModel,
            envModel: oEnvModel,
            statePaths: {
                WORKFLOW_DETAIL_EDIT_MODE: "/workflow/detail/editMode",
                WORKFLOW_DETAIL_LOCK_STATE: "/workflow/detail/lockState"
            },
            componentRuntimeSupport: {
                syncUiStateMode: function () {}
            },
            searchConfig: SearchUiConfig.getLayoutSeed()
        });

        assert.deepEqual(oLayoutModel.getProperty("/smartFilter/fields"), SearchUiConfig.getLayoutSeed().smartFilter.fields, "smart filter config is seeded from canonical search config");
        assert.deepEqual(oLayoutModel.getProperty("/smartTable/columns"), SearchUiConfig.getLayoutSeed().smartTable.columns, "smart table columns are seeded from canonical search config");
        assert.strictEqual(oLayoutModel.getProperty("/smartTable/selectionMode"), "MultiSelect", "selection mode is seeded from canonical search config");
        assert.ok(aAdded.indexOf("beforeunload") >= 0, "beforeunload listener is attached");

        window.addEventListener = fnOriginalAdd;
        window.removeEventListener = fnOriginalRemove;
    });
});
