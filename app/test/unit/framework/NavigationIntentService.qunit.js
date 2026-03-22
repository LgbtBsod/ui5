sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (JSONModel, NavigationIntentService, StatePaths) {
    "use strict";

    QUnit.module("framework/NavigationIntentService");

    QUnit.test("queuePendingIntent stores navigation ownership metadata", function (assert) {
        var oStateModel = new JSONModel({});
        var oComponent = {
            getRouter: function () {
                return {
                    getURL: function () {
                        return "/analytics";
                    }
                };
            }
        };
        var oRouteEvent = {
            getParameter: function (sName) {
                if (sName === "name") {
                    return "analytics";
                }
                if (sName === "arguments") {
                    return {};
                }
                return undefined;
            }
        };

        NavigationIntentService.queuePendingIntent(oComponent, oStateModel, StatePaths, oRouteEvent, {
            owner: "navigationGuard",
            resumeMode: "afterGuardedSave"
        });

        assert.strictEqual(oStateModel.getProperty(StatePaths.PENDING_NAVIGATION_INTENT + "/owner"), "navigationGuard", "owner is stored");
        assert.strictEqual(oStateModel.getProperty(StatePaths.PENDING_NAVIGATION_INTENT + "/resumeMode"), "afterGuardedSave", "resume mode is stored");
    });
});
