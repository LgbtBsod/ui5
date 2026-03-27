sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/EnterEditUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimerDefaults"
], function (EnterEditUseCase, TimerDefaults) {
    "use strict";

    QUnit.module("EnterEditUseCase");

    QUnit.test("enter edit uses runtime cache tolerance when provided", function (assert) {
        var done = assert.async();
        var oUseCase = EnterEditUseCase();
        var aValidationCalls = [];

        oUseCase.execute({
            rootId: "ROOT-EDIT-1",
            state: true
        }, {
            runtimeSettings: {
                cacheToleranceMs: 9876
            },
            lock: {
                acquire: function () {
                    return Promise.resolve({ ok: true, code: "OK" });
                }
            },
            cacheValidation: {
                execute: function (mInput) {
                    aValidationCalls.push(mInput);
                    return Promise.resolve({ ok: true, data: { invalidated: false } });
                }
            },
            uiState: {
                get: function () {
                    return false;
                }
            }
        }).then(function (oResult) {
            assert.ok(oResult && oResult.ok, "enter edit succeeds");
            assert.strictEqual(aValidationCalls.length, 1, "cache validation runs once");
            assert.strictEqual(aValidationCalls[0].toleranceMs, 9876, "runtime cache tolerance is forwarded");
            done();
        });
    });

    QUnit.test("enter edit falls back to TimerDefaults cache tolerance", function (assert) {
        var done = assert.async();
        var oUseCase = EnterEditUseCase();
        var aValidationCalls = [];

        oUseCase.execute({
            rootId: "ROOT-EDIT-2",
            state: true
        }, {
            lock: {
                acquire: function () {
                    return Promise.resolve({ ok: true, code: "OK" });
                }
            },
            cacheValidation: {
                execute: function (mInput) {
                    aValidationCalls.push(mInput);
                    return Promise.resolve({ ok: true, data: { invalidated: false } });
                }
            },
            uiState: {
                get: function () {
                    return false;
                }
            }
        }).then(function (oResult) {
            assert.ok(oResult && oResult.ok, "enter edit succeeds");
            assert.strictEqual(aValidationCalls.length, 1, "cache validation runs once");
            assert.strictEqual(aValidationCalls[0].toleranceMs, TimerDefaults.cacheToleranceMs.defaultValue, "default cache tolerance is used as fallback");
            done();
        });
    });
});
