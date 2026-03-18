sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchAnalyticsIntentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy"
], function (JSONModel, SearchAnalyticsIntentBehavior, SearchCommandPolicy) {
    "use strict";

    function createController(oIntent, bSmartTableReady) {
        var oStateModel = new JSONModel({
            analyticsDrilldownIntent: oIntent || null,
            search: {
                checksFailSegment: "ALL",
                barriersFailSegment: "ALL"
            }
        });
        var oViewModel = new JSONModel({
            smartTableReady: !!bSmartTableReady
        });
        var oApplied = {
            filterData: null,
            controlValue: null
        };
        var oControl = {
            setSelectedKey: function (sValue) {
                oApplied.controlValue = sValue;
            },
            setValue: function (sValue) {
                oApplied.controlValue = sValue;
            },
            setTokens: function () {}
        };
        var oSmartFilterBar = {
            isInitialised: function () {
                return true;
            },
            getControlByKey: function () {
                return oControl;
            },
            getFilterData: function () {
                return {};
            },
            setFilterData: function (mFilterData) {
                oApplied.filterData = mFilterData;
            }
        };
        var mModels = {
            state: oStateModel,
            view: oViewModel
        };

        return {
            applied: oApplied,
            controller: {
                getModel: function (sName) {
                    return mModels[sName];
                },
                byId: function (sId) {
                    return sId === "searchSmartFilterBar" ? oSmartFilterBar : null;
                }
            }
        };
    }

    QUnit.module("SearchAnalyticsIntentBehavior", {
        beforeEach: function () {
            this._fnBuildFilter = SearchCommandPolicy.buildFilter;
            this._fnRebind = SearchCommandPolicy.rebind;
            this._calls = [];
            SearchCommandPolicy.buildFilter = function (_oController, mInput) {
                this._calls.push({ type: "buildFilter", input: mInput });
            }.bind(this);
            SearchCommandPolicy.rebind = function (_oController, mInput) {
                this._calls.push({ type: "rebind", input: mInput });
            }.bind(this);
        },
        afterEach: function () {
            SearchCommandPolicy.buildFilter = this._fnBuildFilter;
            SearchCommandPolicy.rebind = this._fnRebind;
        }
    });

    QUnit.test("applies source, year range and failed-check scope from analytics intent", function (assert) {
        var oFixture = createController({
            filterKey: "ProfessionText",
            filterValue: "PR1",
            selectedYear: "2026",
            analyticsSource: "WEB",
            extras: {
                metric: "FAILED_CHECKS"
            }
        }, true);
        var bApplied = SearchAnalyticsIntentBehavior.applyAnalyticsDrilldownIntent(oFixture.controller, {
            intentPath: "/analyticsDrilldownIntent",
            smartTableReadyPath: "/smartTableReady",
            source: "ANALYTICS_DRILLDOWN",
            stateModel: "state"
        });
        var mFilterData = oFixture.applied.filterData;

        assert.strictEqual(bApplied, true, "Intent was applied");
        assert.strictEqual(oFixture.applied.controlValue, "PR1", "Primary filter control received the drilldown value");
        assert.strictEqual(mFilterData.ProfessionText.items[0].key, "PR1", "Dimension filter persisted in SmartFilterBar data");
        assert.strictEqual(mFilterData.SourceKey.items[0].key, "WEB", "Source scope was transferred to search");
        assert.strictEqual(mFilterData.DateCheck.ranges[0].value1, "2026-01-01", "Selected year start date was applied");
        assert.strictEqual(mFilterData.DateCheck.ranges[0].value2, "2026-12-31", "Selected year end date was applied");
        assert.strictEqual(oFixture.controller.getModel("state").getProperty("/search/checksFailSegment"), "FAILED", "Failed checks segment was activated");
        assert.strictEqual(oFixture.controller.getModel("state").getProperty("/search/barriersFailSegment"), "ALL", "Barrier segment stayed neutral");
        assert.strictEqual(oFixture.controller.getModel("state").getProperty("/analyticsDrilldownIntent"), null, "Intent was cleared after apply");
        assert.deepEqual(this._calls.map(function (oEntry) { return oEntry.type; }), ["buildFilter", "rebind"], "Search refresh was executed");
    });

    QUnit.test("applies month drilldown as month date range", function (assert) {
        var oFixture = createController({
            filterKey: "DateCheck",
            filterValue: "Mar",
            selectedYear: "2026",
            analyticsSource: "ALL",
            extras: {
                monthLabel: "Mar",
                metric: "TOTAL"
            }
        }, false);
        var bApplied = SearchAnalyticsIntentBehavior.applyAnalyticsDrilldownIntent(oFixture.controller, {
            intentPath: "/analyticsDrilldownIntent",
            smartTableReadyPath: "/smartTableReady",
            source: "ANALYTICS_DRILLDOWN",
            stateModel: "state"
        });
        var oRange = oFixture.applied.filterData.DateCheck.ranges[0];

        assert.strictEqual(bApplied, true, "Month intent was applied");
        assert.strictEqual(oRange.value1, "2026-03-01", "Month start date was applied");
        assert.strictEqual(oRange.value2, "2026-03-31", "Month end date was applied");
        assert.deepEqual(this._calls.map(function (oEntry) { return oEntry.type; }), ["buildFilter"], "Rebind stayed deferred until SmartTable ready");
    });
});
