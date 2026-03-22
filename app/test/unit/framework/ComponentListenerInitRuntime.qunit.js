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
        var oShellModel = new JSONModel({
            smartFilter: {},
            smartTable: {}
        });
        var oMasterDataModel = new JSONModel({});
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
            shellModel: oShellModel,
            masterDataModel: oMasterDataModel,
            statePaths: {
                WORKFLOW_DETAIL_EDIT_MODE: "/workflow/detail/editMode",
                WORKFLOW_DETAIL_LOCK_STATE: "/workflow/detail/lockState"
            },
            searchConfig: SearchUiConfig.getLayoutSeed()
        });

        assert.deepEqual(oShellModel.getProperty("/smartFilter/fields"), SearchUiConfig.getLayoutSeed().smartFilter.fields, "smart filter config is seeded from canonical search config");
        assert.deepEqual(oShellModel.getProperty("/smartTable/columns"), SearchUiConfig.getLayoutSeed().smartTable.columns, "smart table columns are seeded from canonical search config");
        assert.strictEqual(oShellModel.getProperty("/smartTable/selectionMode"), "MultiSelect", "selection mode is seeded from canonical search config");
        assert.ok(aAdded.indexOf("beforeunload") >= 0, "beforeunload listener is attached");

        window.addEventListener = fnOriginalAdd;
        window.removeEventListener = fnOriginalRemove;
    });

    QUnit.test("initializeListeners rebinds dirty listener without duplicate change handlers", function (assert) {
        var iDetachCalls = 0;
        var iAttachCalls = 0;
        var oDirtyBinding = {
            attachChange: function () { iAttachCalls += 1; },
            detachChange: function () { iDetachCalls += 1; }
        };
        var oStateModel = new JSONModel({
            workflow: {
                detail: {
                    editMode: "READ",
                    lockState: "READ_ONLY"
                }
            },
            isDirty: false
        });
        var oShellModel = new JSONModel({
            smartFilter: {},
            smartTable: {}
        });
        var oComponent = {
            setModel: function () {},
            _oAutoSave: {
                touch: function () {}
            },
            _syncLockScopedManagers: function () {}
        };
        var fnOriginalBindProperty = oStateModel.bindProperty.bind(oStateModel);
        oStateModel.bindProperty = function (sPath) {
            if (sPath === "/isDirty") {
                return oDirtyBinding;
            }
            return fnOriginalBindProperty(sPath);
        };

        ComponentListenerInitRuntime.initializeListeners({
            component: oComponent,
            stateModel: oStateModel,
            shellModel: oShellModel,
            masterDataModel: new JSONModel({}),
            statePaths: {
                WORKFLOW_DETAIL_EDIT_MODE: "/workflow/detail/editMode",
                WORKFLOW_DETAIL_LOCK_STATE: "/workflow/detail/lockState"
            }
        });
        ComponentListenerInitRuntime.initializeListeners({
            component: oComponent,
            stateModel: oStateModel,
            shellModel: oShellModel,
            masterDataModel: new JSONModel({}),
            statePaths: {
                WORKFLOW_DETAIL_EDIT_MODE: "/workflow/detail/editMode",
                WORKFLOW_DETAIL_LOCK_STATE: "/workflow/detail/lockState"
            }
        });

        assert.strictEqual(iAttachCalls, 2, "listener is attached for each fresh binding");
        assert.strictEqual(iDetachCalls, 1, "previous dirty binding listener is detached before rebind");
    });
});
