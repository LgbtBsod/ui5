sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheValidationUseCase"
], function (CacheValidationUseCase) {
    "use strict";

    QUnit.module("CacheValidationUseCase");

    QUnit.test("keeps cache when stamp delta is within tolerance", function (assert) {
        var done = assert.async();
        var oUseCase = new CacheValidationUseCase();
        var oSnapshot = {
            meta: {
                aggChangedOn: "2026-03-17T10:00:00.000Z"
            }
        };
        var oCache = {
            read: function () {
                return {
                    payload: oSnapshot,
                    lastChangeSet: "2026-03-17T10:00:04.000Z"
                };
            },
            clear: function () {
                assert.ok(false, "clear must not run for valid stamp window");
            }
        };
        var oLastChangeSet = {
            readAggChangedOn: function () {
                return Date.parse("2026-03-17T10:00:00.000Z");
            }
        };

        oUseCase.execute({
            rootId: "ROOT-1",
            toleranceMs: 5500
        }, {
            cache: oCache,
            lastChangeSet: oLastChangeSet
        }).then(function (oResult) {
            assert.strictEqual(oResult.ok, true, "use case succeeds");
            assert.strictEqual(oResult.data.valid, true, "snapshot stays valid");
            assert.strictEqual(oResult.data.invalidated, false, "cache is not invalidated");
            assert.strictEqual(oResult.data.stampDeltaMs, 4000, "stamp delta is reported explicitly");
            done();
        });
    });

    QUnit.test("invalidates cache when stamp delta exceeds tolerance", function (assert) {
        var done = assert.async();
        var oUseCase = new CacheValidationUseCase();
        var bCleared = false;
        var oCache = {
            read: function () {
                return {
                    payload: {
                        meta: {
                            aggChangedOn: "2026-03-17T10:00:00.000Z"
                        }
                    }
                };
            },
            clear: function () {
                bCleared = true;
            }
        };
        var oLastChangeSet = {
            readAggChangedOn: function () {
                return Date.parse("2026-03-17T10:00:10.000Z");
            }
        };

        oUseCase.execute({
            rootId: "ROOT-2",
            toleranceMs: 5500
        }, {
            cache: oCache,
            lastChangeSet: oLastChangeSet
        }).then(function (oResult) {
            assert.strictEqual(oResult.ok, true, "use case succeeds");
            assert.strictEqual(oResult.data.valid, false, "snapshot is invalid after server stamp drift");
            assert.strictEqual(oResult.data.invalidated, true, "cache is invalidated");
            assert.strictEqual(oResult.data.stampDeltaMs, 10000, "reported delta matches stamp drift");
            assert.strictEqual(bCleared, true, "cache clear is executed");
            done();
        });
    });
});
