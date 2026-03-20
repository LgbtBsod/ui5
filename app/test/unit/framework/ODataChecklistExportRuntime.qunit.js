sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistExportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (ODataChecklistExportRuntime, GatewayODataClient, GatewayContractConstants) {
    "use strict";

    QUnit.module("ODataChecklistExportRuntime");

    QUnit.test("export uses POST_FUNCTION for ReportExport", function (assert) {
        var done = assert.async();
        var fnOriginalPostFunction = GatewayODataClient.postFunction;
        var mCaptured = null;

        GatewayODataClient.postFunction = function (sPath, oBody) {
            mCaptured = {
                path: sPath,
                body: Object.assign({}, oBody || {})
            };
            return Promise.resolve({ results: [] });
        };

        ODataChecklistExportRuntime.exportSearchResults({
            entity: "screen",
            limit: 100,
            selectionMode: "all"
        }).then(function () {
            assert.ok(mCaptured, "gateway request is issued");
            assert.strictEqual(mCaptured.path, GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT, "ReportExport function import is targeted");
            assert.strictEqual(mCaptured.body.SelectionMode, "all", "payload is forwarded");
            GatewayODataClient.postFunction = fnOriginalPostFunction;
            done();
        }).catch(function (oError) {
            GatewayODataClient.postFunction = fnOriginalPostFunction;
            assert.ok(false, "export should not reject: " + (oError && oError.message));
            done();
        });
    });
});
