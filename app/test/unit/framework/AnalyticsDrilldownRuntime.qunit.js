sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation"
], function (JSONModel, AnalyticsDrilldownRuntime, WorkspaceRouteNavigation) {
    "use strict";

    function createController() {
        var mModels = {
            view: new JSONModel({
                selectedYear: "2026",
                compareYear: "2025",
                selectedSource: "WEB"
            }),
            state: new JSONModel({})
        };

        return {
            getModel: function (sName) {
                return mModels[sName];
            }
        };
    }

    QUnit.module("AnalyticsDrilldownRuntime", {
        beforeEach: function () {
            this._fnNavigateToSearch = WorkspaceRouteNavigation.navigateToSearch;
            this._navigated = 0;
            WorkspaceRouteNavigation.navigateToSearch = function () {
                this._navigated += 1;
            }.bind(this);
        },
        afterEach: function () {
            WorkspaceRouteNavigation.navigateToSearch = this._fnNavigateToSearch;
        }
    });

    QUnit.test("queues rich drilldown intent with analytics scope", function (assert) {
        var oController = createController();
        var bQueued = AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "ProfessionText", "PR1", {
            dimension: "PROFESSION",
            metric: "FAILED_CHECKS"
        });
        var oIntent = oController.getModel("state").getProperty("/analyticsDrilldownIntent");

        return Promise.resolve(bQueued).then(function (bResult) {
            assert.strictEqual(bResult, true, "Drilldown was queued");
            assert.strictEqual(oIntent.filterKey, "ProfessionText", "Primary filter key was stored");
            assert.strictEqual(oIntent.filterValue, "PR1", "Primary filter value was stored");
            assert.strictEqual(oIntent.selectedYear, "2026", "Selected analytics year was stored");
            assert.strictEqual(oIntent.compareYear, "2025", "Compare year was stored");
            assert.strictEqual(oIntent.analyticsSource, "WEB", "Analytics source scope was stored");
            assert.strictEqual(oIntent.extras.metric, "FAILED_CHECKS", "Metric scope was stored");
            assert.strictEqual(this._navigated, 1, "Navigation to search was triggered");
        }.bind(this));
    });
});
