sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistExportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (ODataChecklistExportRuntime, GatewayClient, GatewayContractConstants) {
    "use strict";

    QUnit.module("ODataChecklistExportRuntime");

    QUnit.test("export uses POST_FUNCTION for ReportExport", function (assert) {
        var done = assert.async();
        var fnOriginalPostFunction = GatewayClient.callFunctionImport;
        var mCaptured = null;

        GatewayClient.callFunctionImport = function (sPath, oBody) {
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
            GatewayClient.callFunctionImport = fnOriginalPostFunction;
            done();
        }).catch(function (oError) {
            GatewayClient.callFunctionImport = fnOriginalPostFunction;
            assert.ok(false, "export should not reject: " + (oError && oError.message));
            done();
        });
    });
});
