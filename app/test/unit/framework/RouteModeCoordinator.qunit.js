sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteModeCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (JSONModel, RouteModeCoordinator, NavigationContracts, ModelStateRuntime, StatePaths) {
    "use strict";

    QUnit.module("RouteModeCoordinator");

    QUnit.test("detail route sync keeps activeObjectId canonical and mirrors selectedId", function (assert) {
        var mSeed = {};
        var oStateModel;
        mSeed[StatePaths.SELECTED_ID] = "CHK-STALE-1";
        mSeed[StatePaths.ACTIVE_OBJECT_ID] = "CHK-STALE-2";
        mSeed[StatePaths.CURRENT_ROUTE_NAME] = NavigationContracts.ROUTES.SEARCH;
        oStateModel = new JSONModel(mSeed);
        var oCoordinator = new RouteModeCoordinator({
            router: {
                attachRoutePatternMatched: function () {},
                detachRoutePatternMatched: function () {}
            },
            stateModel: oStateModel
        });

        RouteModeCoordinator.prototype._onAnyRouteMatched.call(oCoordinator, {
            getParameter: function (sParam) {
                if (sParam === "name") {
                    return NavigationContracts.ROUTES.DETAIL;
                }
                if (sParam === "arguments") {
                    return { id: "CHK-00001" };
                }
                return undefined;
            }
        });

        assert.strictEqual(ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, ""), "CHK-00001", "active object id is canonical");
        assert.strictEqual(ModelStateRuntime.readOnModel(oStateModel, StatePaths.SELECTED_ID, ""), "CHK-00001", "selected id mirrors active object id");
    });
});
