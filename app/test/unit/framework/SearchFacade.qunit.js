sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade"
], function (SearchFacade) {
    "use strict";

    QUnit.module("framework/SearchFacade");

    QUnit.test("routes startup and execution methods through grouped runtimes", function (assert) {
        var aCalls = [];
        var oFacade = new SearchFacade({
            bootstrapRuntime: {
                bootstrap: function () {
                    aCalls.push("bootstrap");
                    return Promise.resolve("bootstrap");
                }
            },
            executionRuntime: {
                buildFilter: function () {
                    aCalls.push("buildFilter");
                    return Promise.resolve("buildFilter");
                },
                executeSearch: function () {
                    aCalls.push("executeSearch");
                    return Promise.resolve("executeSearch");
                },
                rebind: function () {
                    aCalls.push("rebind");
                    return Promise.resolve("rebind");
                },
                applyRebindPolicy: function () {
                    aCalls.push("applyRebindPolicy");
                    return Promise.resolve("applyRebindPolicy");
                }
            },
            selectionRuntime: {
                selectRow: function () {
                    aCalls.push("selectRow");
                    return Promise.resolve("selectRow");
                },
                selectionChanged: function () {
                    aCalls.push("selectionChanged");
                    return Promise.resolve("selectionChanged");
                }
            },
            presentationRuntime: {
                analytics: function () {
                    aCalls.push("analytics");
                    return Promise.resolve("analytics");
                },
                exportFlow: function () {
                    aCalls.push("exportFlow");
                    return Promise.resolve("exportFlow");
                }
            }
        });
        var done = assert.async();

        Promise.all([
            oFacade.bootstrap(),
            oFacade.buildFilter(),
            oFacade.executeSearch(),
            oFacade.rebind(),
            oFacade.applyRebindPolicy(),
            oFacade.selectRow(),
            oFacade.selectionChanged(),
            oFacade.analytics(),
            oFacade.exportFlow()
        ]).then(function () {
            assert.deepEqual(aCalls, [
                "bootstrap",
                "buildFilter",
                "executeSearch",
                "rebind",
                "applyRebindPolicy",
                "selectRow",
                "selectionChanged",
                "analytics",
                "exportFlow"
            ], "facade delegates each scenario to the grouped runtime owner");
            done();
        });
    });
});
