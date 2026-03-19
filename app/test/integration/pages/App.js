sap.ui.define([
    "sap/ui/test/Opa5",
    "sap/ui/test/matchers/PropertyStrictEquals"
], function (Opa5, PropertyStrictEquals) {
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
                        id: "mainFcl",
                        success: function () {
                            var oComponent = Opa5.getAppComponent();
                            var oRouter = oComponent && oComponent.getRouter && oComponent.getRouter();
                            if (oRouter && typeof oRouter.navTo === "function") {
                                oRouter.navTo("detail", { id: sKey });
                            }
                        }
                    });
                },
                iNavigateBackToSearchRoute: function () {
                    return this.waitFor({
                        id: "mainFcl",
                        success: function () {
                            var oComponent = Opa5.getAppComponent();
                            var oRouter = oComponent && oComponent.getRouter && oComponent.getRouter();
                            if (oRouter && typeof oRouter.navTo === "function") {
                                oRouter.navTo("search", {}, true);
                            }
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
                        id: "searchResultsSummaryRail",
                        success: function (oSearchSummary) {
                            var oSummaryDom = oSearchSummary && oSearchSummary.getDomRef && oSearchSummary.getDomRef();
                            Opa5.assert.ok(!oSummaryDom || oSummaryDom.getAttribute("role") === "status", "Search summary rail exposes status semantics");
                        }
                    }).and.waitFor({
                        id: "searchResultsActionRail",
                        success: function (oSearchActions) {
                            var oActionsDom = oSearchActions && oSearchActions.getDomRef && oSearchActions.getDomRef();
                            Opa5.assert.ok(!oActionsDom || oActionsDom.getAttribute("role") === "region", "Search action rail exposes region semantics");
                        }
                    }).and.waitFor({
                        id: "detailSectionAnchorRail",
                        success: function (oDetailAnchorRail) {
                            var oAnchorDom = oDetailAnchorRail && oDetailAnchorRail.getDomRef && oDetailAnchorRail.getDomRef();
                            Opa5.assert.ok(!oAnchorDom || oAnchorDom.getAttribute("role") === "navigation", "Detail anchor rail exposes navigation semantics");
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
