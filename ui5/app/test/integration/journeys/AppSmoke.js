sap.ui.define([
    "sap/ui/test/opaQunit",
    "PRODUCTION_CONTROL_CHECKLIST/test/integration/pages/App"
], function (opaTest) {
    "use strict";

    var MOCK_CHECKLIST_KEY = "4A66B18648094B708C7B0375694B15C1";

    QUnit.module("App smoke");

    opaTest("launches the shell with productive title", function (Given, When, Then) {
        Given.iStartMyApp();
        When.onTheAppPage.iLookAtTheShell();
        Then.onTheAppPage.iShouldSeeTheProductTitle();
        Then.onTheAppPage.iShouldExposeStickySearchAndDetailSemantics();
        Then.iTeardownMyApp();
    });

    opaTest("navigates search to detail and back on mock contour", function (Given, When, Then) {
        Given.iStartMyApp();
        Then.onTheAppPage.iShouldSeeSearchView();
        When.onTheAppPage.iNavigateToDetailRoute(MOCK_CHECKLIST_KEY);
        Then.onTheAppPage.iShouldSeeDetailView();
        When.onTheAppPage.iNavigateBackToSearchRoute();
        Then.onTheAppPage.iShouldSeeSearchView();
        Then.iTeardownMyApp();
    });
});
