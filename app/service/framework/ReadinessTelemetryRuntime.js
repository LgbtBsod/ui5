sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts"
], function (ModelStateRuntime, ReadinessTelemetryContracts) {
    "use strict";

    function nowMs() {
        return (typeof window !== "undefined" && window.performance && typeof window.performance.now === "function")
            ? window.performance.now()
            : Date.now();
    }

    function nowIso() {
        return new Date().toISOString();
    }

    function resolveStateModelFromController(oController) {
        return oController && oController.getModel ? oController.getModel("state") : null;
    }

    function ensureStart(oStateModel) {
        var iStartedAt = Number(ModelStateRuntime.readOnModel(oStateModel, ReadinessTelemetryContracts.STARTED_AT_PATH, 0) || 0);
        if (iStartedAt > 0) {
            return iStartedAt;
        }
        iStartedAt = nowMs();
        ModelStateRuntime.writeOnModel(oStateModel, ReadinessTelemetryContracts.STARTED_AT_PATH, iStartedAt);
        return iStartedAt;
    }

    function markOnModel(oStateModel, sStage, mDetails) {
        var iStartedAt;
        var iElapsedMs;
        var sStagePath;
        var mCurrent;
        var mPayload;

        if (!oStateModel || !sStage) {
            return null;
        }

        iStartedAt = ensureStart(oStateModel);
        sStagePath = ReadinessTelemetryContracts.ROOT_PATH + "/stages/" + sStage;
        mCurrent = ModelStateRuntime.readOnModel(oStateModel, sStagePath, null);
        if (mCurrent && mCurrent.ready) {
            return mCurrent;
        }

        iElapsedMs = Math.max(0, Math.round(nowMs() - iStartedAt));
        mPayload = {
            ready: true,
            stage: sStage,
            markedAt: nowIso(),
            elapsedMs: iElapsedMs,
            details: mDetails || {}
        };

        ModelStateRuntime.setManyOnModel(oStateModel, {
            [sStagePath]: mPayload,
            [ReadinessTelemetryContracts.LAST_STAGE_PATH]: sStage
        });

        if (typeof window !== "undefined" && window.performance && typeof window.performance.mark === "function") {
            window.performance.mark("pcct." + sStage);
        }

        return mPayload;
    }

    function markControllerStage(oController, sStage, mDetails) {
        return markOnModel(resolveStateModelFromController(oController), sStage, mDetails);
    }

    return {
        markControllerStage: markControllerStage,
        markOnModel: markOnModel
    };
});
