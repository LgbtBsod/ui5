sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (JSONModel, ShellLayoutRuntime, NavigationContracts, ModelContracts) {
    "use strict";

    function createClassListStub() {
        return {
            add: function () {},
            remove: function () {},
            toggle: function () {}
        };
    }

    function createController(mStateData, mShellData) {
        var oStateModel = new JSONModel(mStateData || {});
        var oShellModel = new JSONModel(mShellData || {});
        var oLayoutState = {
            currentLayout: "",
            currentPage: null
        };
        var oHost = {
            classList: createClassListStub(),
            querySelector: function () {
                return null;
            }
        };
        var oFcl = {
            getCurrentMidColumnPage: function () {
                return oLayoutState.currentPage;
            },
            getLayout: function () {
                return oLayoutState.currentLayout;
            },
            setLayout: function (sLayout) {
                oLayoutState.currentLayout = sLayout;
            },
            toMidColumnPage: function (oPage) {
                oLayoutState.currentPage = oPage;
            }
        };

        return {
            _layoutState: oLayoutState,
            byId: function (sId) {
                if (sId === "mainFcl") {
                    return oFcl;
                }
                return {
                    getId: function () {
                        return sId;
                    }
                };
            },
            getModel: function (sName) {
                if (sName === ModelContracts.MODELS.STATE) {
                    return oStateModel;
                }
                if (sName === ModelContracts.MODELS.SHELL) {
                    return oShellModel;
                }
                return null;
            },
            getView: function () {
                return {
                    getDomRef: function () {
                        return oHost;
                    }
                };
            }
        };
    }

    QUnit.module("ShellLayoutRuntime");

    QUnit.test("search route collapses shell layout to OneColumn", function (assert) {
        var oController = createController({
            currentRouteName: NavigationContracts.ROUTES.SEARCH,
            selectedId: "CHK-STALE-1",
            activeObjectId: ""
        }, {
            layout: NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN
        });

        ShellLayoutRuntime.syncLayoutState(oController, oController.getModel(ModelContracts.MODELS.STATE));

        assert.strictEqual(
            oController.getModel(ModelContracts.MODELS.SHELL).getProperty(ModelContracts.MODEL_PATHS.SHELL_LAYOUT),
            NavigationContracts.LAYOUTS.ONE_COLUMN,
            "search route owns OneColumn layout"
        );
        assert.strictEqual(oController._layoutState.currentLayout, NavigationContracts.LAYOUTS.ONE_COLUMN, "FCL layout stays in sync");
    });

    QUnit.test("detail route restores split layout for persisted detail", function (assert) {
        var oController = createController({
            currentRouteName: NavigationContracts.ROUTES.DETAIL,
            selectedId: "CHK-100",
            activeObjectId: "CHK-100"
        }, {
            layout: NavigationContracts.LAYOUTS.ONE_COLUMN
        });

        ShellLayoutRuntime.syncLayoutState(oController, oController.getModel(ModelContracts.MODELS.STATE));

        assert.strictEqual(
            oController.getModel(ModelContracts.MODELS.SHELL).getProperty(ModelContracts.MODEL_PATHS.SHELL_LAYOUT),
            NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED,
            "detail route owns split layout"
        );
    });

    QUnit.test("analytics route forces fullscreen layout", function (assert) {
        var oController = createController({
            currentRouteName: NavigationContracts.ROUTES.ANALYTICS,
            selectedId: "CHK-200",
            activeObjectId: "CHK-200"
        }, {
            layout: NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED
        });

        ShellLayoutRuntime.syncLayoutState(oController, oController.getModel(ModelContracts.MODELS.STATE));

        assert.strictEqual(
            oController.getModel(ModelContracts.MODELS.SHELL).getProperty(ModelContracts.MODEL_PATHS.SHELL_LAYOUT),
            NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN,
            "analytics route owns fullscreen layout"
        );
    });
});
