sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime"
], function (DetailPersistenceRuntime) {
    "use strict";

    QUnit.module("DetailPersistenceRuntime");

    QUnit.test("classifyError maps lock expiry to lock-lost taxonomy", function (assert) {
        var oResult = DetailPersistenceRuntime.classifyError({
            code: "LOCK_EXPIRED",
            message: "Lock expired for this session"
        });

        assert.strictEqual(oResult.taxonomy, DetailPersistenceRuntime.TAXONOMY.LOCK_EXPIRED, "lock expiry taxonomy is preserved");
        assert.strictEqual(oResult.persistenceState, DetailPersistenceRuntime.STATES.LOCK_LOST, "lock expiry forces lock-lost UI state");
        assert.strictEqual(oResult.messageKey, "persistenceLockExpired", "lock expiry gets dedicated message key");
    });

    QUnit.test("failureEffects clears in-flight flags and stores error taxonomy", function (assert) {
        var oFailure = DetailPersistenceRuntime.failureEffects("manual", {
            statusCode: 409,
            message: "Version conflict"
        });
        var oPersistencePatch = (oFailure.effects || []).filter(function (oEffect) {
            return oEffect.type === "modelPatch" && oEffect.path === "/persistence";
        })[0];

        assert.ok(oPersistencePatch, "central persistence patch exists");
        assert.strictEqual(oPersistencePatch.value.state, DetailPersistenceRuntime.STATES.CONFLICT, "conflict state is centralized");
        assert.strictEqual(oPersistencePatch.value.taxonomy, DetailPersistenceRuntime.TAXONOMY.VERSION_CONFLICT, "taxonomy is normalized");
        assert.strictEqual(oPersistencePatch.value.isManualSaveInFlight, false, "manual in-flight flag is cleared");
        assert.strictEqual(oPersistencePatch.value.currentWriteRequestId, "", "write request id is cleared");
    });

    QUnit.test("classifyError prefers backend machine code over free-text signal parsing", function (assert) {
        var oResult = DetailPersistenceRuntime.classifyError({
            code: "SYSTEM_ERROR",
            message: "Generic backend failure",
            backend: {
                code: "LOCK_NOT_OWNED_BY_SESSION"
            }
        });

        assert.strictEqual(oResult.taxonomy, DetailPersistenceRuntime.TAXONOMY.LOCK_NOT_OWNED_BY_SESSION, "backend machine code is authoritative");
        assert.strictEqual(oResult.persistenceState, DetailPersistenceRuntime.STATES.LOCK_LOST, "lock ownership mismatch forces lock-lost state");
    });
});
