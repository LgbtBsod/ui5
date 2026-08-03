sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade"
], function (SearchFacade) {
    "use strict";

    QUnit.module("framework/SearchFacade");

    QUnit.test("routes startup and execution methods through injected usecases", function (assert) {
        var aCalls = [];
        function trackedUseCase(sName) {
            return {
                execute: function () {
                    aCalls.push(sName);
                    return Promise.resolve(sName);
                }
            };
        }
        var oFacade = new SearchFacade({
            bootstrapUseCase: trackedUseCase("bootstrap"),
            buildFilterUseCase: trackedUseCase("buildFilter"),
            executeUseCase: trackedUseCase("executeSearch"),
            rebindUseCase: trackedUseCase("rebind"),
            applyRebindPolicyUseCase: trackedUseCase("applyRebindPolicy"),
            selectRowUseCase: trackedUseCase("selectRow"),
            selectionChangedUseCase: trackedUseCase("selectionChanged"),
            analyticsUseCase: trackedUseCase("analytics"),
            exportUseCase: trackedUseCase("exportFlow")
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
            ], "facade delegates each scenario to its injected usecase");
            done();
        });
    });
});
