sap.ui.define([
    "sap/ui/test/Opa5",
    "sap/ui/test/matchers/PropertyStrictEquals",
    "sap/ui/core/routing/HashChanger"
], function (Opa5, PropertyStrictEquals, HashChanger) {
    "use strict";

    Opa5.createPageObjects({
        onTheAppPage: {
            actions: {
                iLookAtTheShell: function () {
                    return this.waitFor({
                        controlType: "sap.m.Title",
                        matchers: new PropertyStrictEquals({
                            name: "text",
                            value: "Production Control Checklists"
                        }),
                        success: function () {}
                    });
                },
                iNavigateToDetailRoute: function (sKey) {
                    return this.waitFor({
                        success: function () {
                            HashChanger.getInstance().setHash("checklist/" + sKey);
                        }
                    });
                },
                iNavigateBackToSearchRoute: function () {
                    return this.waitFor({
                        success: function () {
                            HashChanger.getInstance().setHash("");
                        }
                    });
                }
            },
            assertions: {
                iShouldSeeTheProductTitle: function () {
                    return this.waitFor({
                        controlType: "sap.m.Title",
                        matchers: new PropertyStrictEquals({
                            name: "text",
                            value: "Production Control Checklists"
                        }),
                        success: function (aTitles) {
                            Opa5.assert.ok(aTitles && aTitles.length > 0, "The productive shell title is visible");
                        }
                    });
                },
                iShouldExposeStickySearchAndDetailSemantics: function () {
                    return this.waitFor({
                        controlType: "sap.ui.core.Control",
                        success: function () {
                            var oSearchSummary = document.querySelector("[id$='searchResultsSummaryRail']");
                            var oSearchActions = document.querySelector("[id$='searchResultsActionRail']");
                            var oDetailStatus = document.querySelector(".detailControlRowTop");
                            var oDetailActions = document.querySelector(".detailControlRowActions");

                            Opa5.assert.ok(!oSearchSummary || oSearchSummary.getAttribute("role") === "status", "Search summary rail exposes status semantics");
                            Opa5.assert.ok(!oSearchActions || oSearchActions.getAttribute("role") === "region", "Search action rail exposes region semantics");
                            Opa5.assert.ok(!oDetailStatus || oDetailStatus.getAttribute("role") === "status", "Detail status rail exposes status semantics");
                            Opa5.assert.ok(!oDetailActions || oDetailActions.getAttribute("role") === "region", "Detail action rail exposes region semantics");
                        }
                    });
                },
                iShouldSeeSearchView: function () {
                    return this.waitFor({
                        id: "searchSmartFilterBar",
                        success: function (oControl) {
                            Opa5.assert.ok(!!oControl, "Search view is active");
                        }
                    });
                },
                iShouldSeeDetailView: function () {
                    return this.waitFor({
                        id: "detailObjectPage",
                        viewName: "PRODUCTION_CONTROL_CHECKLIST.views.Detail",
                        success: function (oControl) {
                            Opa5.assert.ok(!!oControl, "Detail view is active");
                        }
                    });
                }
            }
        }
    });

    return Opa5;
});
