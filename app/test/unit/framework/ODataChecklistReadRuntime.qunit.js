sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistReadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (ODataChecklistReadRuntime, GatewayClient) {
    "use strict";

    QUnit.module("ODataChecklistReadRuntime", {
        beforeEach: function () {
            this._rawRead = GatewayClient.rawRead;
        },
        afterEach: function () {
            GatewayClient.rawRead = this._rawRead;
        }
    });

    QUnit.test("resolveDbKey does not re-query search set for canonical RAW16 key", function (assert) {
        var done = assert.async();
        var sCanonicalKey = "40035408B0504242A05E31DD01E57902";
        var iCalls = 0;

        GatewayClient.rawRead = function () {
            iCalls += 1;
            return Promise.reject(new Error("rawRead must not be called for canonical key"));
        };

        ODataChecklistReadRuntime.resolveDbKey({
            dbKey: sCanonicalKey
        }, {
            dbKey: function (mArgs) {
                return mArgs && mArgs.dbKey;
            }
        }).then(function (sResolvedKey) {
            assert.strictEqual(sResolvedKey, sCanonicalKey, "canonical key is returned as-is");
            assert.strictEqual(iCalls, 0, "no search lookup is executed");
            done();
        });
    });
});
