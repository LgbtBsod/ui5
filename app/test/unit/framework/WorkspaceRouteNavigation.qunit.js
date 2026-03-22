sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (JSONModel, WorkspaceRouteNavigation, NavigationContracts, ModelStateRuntime) {
    "use strict";

    function createShellModel(sLayout) {
        return new JSONModel({
            layout: sLayout || NavigationContracts.LAYOUTS.ONE_COLUMN
        });
    }

    QUnit.module("WorkspaceRouteNavigation");

    QUnit.test("buildCurrentIntent keeps search intent when only stale selection remains", function (assert) {
        var oStateModel = new JSONModel({
            currentRouteName: NavigationContracts.ROUTES.SEARCH,
            selectedId: "CHK-STALE-1",
            activeObjectId: "",
            postOpenHydratedRootId: "",
            layout: NavigationContracts.LAYOUTS.ONE_COLUMN
        });
        var oIntent = WorkspaceRouteNavigation.buildCurrentIntent(oStateModel, createShellModel(NavigationContracts.LAYOUTS.ONE_COLUMN));

        assert.strictEqual(oIntent.routeName, NavigationContracts.ROUTES.SEARCH, "search route stays canonical");
        assert.deepEqual(oIntent.routeArgs, {}, "stale search selection does not synthesize detail navigation");
    });

    QUnit.test("buildCurrentIntent uses active detail id for detail routes only", function (assert) {
        var oStateModel = new JSONModel({
            currentRouteName: NavigationContracts.ROUTES.DETAIL,
            selectedId: "CHK-STALE-1",
            activeObjectId: "CHK-ACTIVE-2",
            postOpenHydratedRootId: "",
            layout: NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED
        });
        var oIntent = WorkspaceRouteNavigation.buildCurrentIntent(oStateModel, createShellModel(NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED));

        assert.strictEqual(oIntent.routeName, NavigationContracts.ROUTES.DETAIL, "detail route is preserved");
        assert.strictEqual(oIntent.routeArgs.id, "CHK-ACTIVE-2", "active detail id wins over stale selected id");
    });

    QUnit.test("buildCurrentIntent treats analytics as transient and uses current active detail", function (assert) {
        var oStateModel = new JSONModel({
            currentRouteName: NavigationContracts.ROUTES.ANALYTICS,
            activeObjectId: "CHK-ACTIVE-2",
            postOpenHydratedRootId: "",
            layout: NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN,
            analyticsNavReturn: {
                hash: "checklist/CHK-STALE-1",
                rootId: "CHK-STALE-1",
                restoreEdit: true
            }
        });
        var oIntent = WorkspaceRouteNavigation.buildCurrentIntent(oStateModel, createShellModel(NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN));

        assert.strictEqual(oIntent.routeName, NavigationContracts.ROUTES.DETAIL, "analytics route resolves to current active detail");
        assert.strictEqual(oIntent.routeArgs.id, "CHK-ACTIVE-2", "active detail id stays canonical");
        assert.strictEqual(oIntent.routeArgs.layout, NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN, "fullscreen layout is preserved as route argument");
    });

    QUnit.test("setAnalyticsReturnIntent does not preserve stale detail restore state when opened from search", function (assert) {
        var oStateModel = new JSONModel({
            currentRouteName: NavigationContracts.ROUTES.SEARCH,
            activeObjectId: "CHK-STALE-9",
            selectedId: "CHK-STALE-9",
            postOpenHydratedRootId: "",
            layout: NavigationContracts.LAYOUTS.ONE_COLUMN,
            workflow: {
                detail: {
                    editMode: "EDIT",
                    lockState: "EDIT_LOCKED"
                }
            }
        });
        var oController = {
            getView: function () {
                return {
                    getModel: function () {
                        return null;
                    }
                };
            },
            getOwnerComponent: function () {
                return {
                    getModel: function (sName) {
                        if (sName === "state") {
                            return oStateModel;
                        }
                        if (sName === "shell") {
                            return createShellModel(NavigationContracts.LAYOUTS.ONE_COLUMN);
                        }
                        return null;
                    }
                };
            },
            getRouter: function () {
                return null;
            }
        };

        WorkspaceRouteNavigation.setAnalyticsReturnIntent(oController);

        assert.deepEqual(
            ModelStateRuntime.readOnModel(oStateModel, "/analyticsNavReturn", null),
            {
                hash: "",
                rootId: "",
                restoreEdit: false
            },
            "search-origin analytics snapshot ignores stale detail root and restore-edit state"
        );
    });
});
