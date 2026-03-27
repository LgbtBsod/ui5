sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (LockAdapter, GatewayClient, GatewayContractConstants) {
    "use strict";

    QUnit.module("framework/LockAdapter", {
        afterEach: function () {
            GatewayClient.reset();
            delete globalThis.navigator;
            delete globalThis.Blob;
        }
    });

    QUnit.test("acquire normalizes only canonical LockExpires", function (assert) {
        var done = assert.async();

        GatewayClient.setModel({
            create: function (_sPath, _oPayload, mOptions) {
                mOptions.success({
                    LockExpires: "2026-03-27T10:15:00Z",
                    Success: true
                });
            }
        }, {
            serviceUrl: "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV"
        });

        LockAdapter.acquire({
            dbKey: "ROOT-1",
            sessionGuid: "SESSION-1"
        }).then(function (oResult) {
            assert.ok(oResult.ok, "acquire succeeds");
            assert.strictEqual(oResult.expiresAt, "2026-03-27T10:15:00Z", "canonical expiry is exposed");
            done();
        });
    });

    QUnit.test("acquire ignores legacy expiry aliases", function (assert) {
        var done = assert.async();

        GatewayClient.setModel({
            create: function (_sPath, _oPayload, mOptions) {
                mOptions.success({
                    ExpiresOn: "legacy-value",
                    lock_expires: "legacy-lowercase",
                    Success: true
                });
            }
        }, {
            serviceUrl: "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV"
        });

        LockAdapter.acquire({
            dbKey: "ROOT-2",
            sessionGuid: "SESSION-2"
        }).then(function (oResult) {
            assert.ok(oResult.ok, "acquire succeeds");
            assert.strictEqual(oResult.expiresAt, "", "legacy expiry aliases are ignored");
            done();
        });
    });

    QUnit.test("releaseOnPageLeave sends only DB_KEY and SessionGuid", function (assert) {
        var oBeaconCall = null;

        globalThis.Blob = function (aChunks, mOptions) {
            this.parts = aChunks;
            this.type = mOptions && mOptions.type;
        };
        globalThis.navigator = {
            sendBeacon: function (sUrl, oBlob) {
                oBeaconCall = {
                    url: sUrl,
                    payload: JSON.parse(String((oBlob && oBlob.parts && oBlob.parts[0]) || "{}"))
                };
                return true;
            }
        };

        GatewayClient.setModel({}, {
            serviceUrl: "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/"
        });

        assert.strictEqual(LockAdapter.releaseOnPageLeave({
            dbKey: "ROOT-3",
            sessionGuid: "SESSION-3",
            forceTakeover: true
        }), true, "sendBeacon path is used");
        assert.strictEqual(
            oBeaconCall.url,
            "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/" + GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE,
            "release beacon targets canonical function import"
        );
        assert.deepEqual(oBeaconCall.payload, {
            DB_KEY: "ROOT-3",
            SessionGuid: "SESSION-3"
        }, "release beacon payload keeps only canonical release fields");
    });
});
