sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts"
], function (JSONModel, WorkspaceRouteNavigation, NavigationContracts) {
    "use strict";

    QUnit.module("WorkspaceRouteNavigation");

    QUnit.test("buildCurrentIntent keeps search intent when only stale selection remains", function (assert) {
        var oStateModel = new JSONModel({
            currentRouteName: NavigationContracts.ROUTES.SEARCH,
            selectedId: "CHK-STALE-1",
            activeObjectId: "",
            postOpenHydratedRootId: "",
            layout: NavigationContracts.LAYOUTS.ONE_COLUMN
        });
        var oIntent = WorkspaceRouteNavigation.buildCurrentIntent(oStateModel);

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
        var oIntent = WorkspaceRouteNavigation.buildCurrentIntent(oStateModel);

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
        var oIntent = WorkspaceRouteNavigation.buildCurrentIntent(oStateModel);

        assert.strictEqual(oIntent.routeName, NavigationContracts.ROUTES.DETAIL_LAYOUT, "analytics route resolves to current active detail");
        assert.strictEqual(oIntent.routeArgs.id, "CHK-ACTIVE-2", "active detail id stays canonical");
    });
});
