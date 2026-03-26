sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailWorkflowRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (DetailWorkflowRuntime, DetailContracts, MessageCodeConstants, MessageKeyConstants) {
    "use strict";

    QUnit.module("framework/DetailWorkflowRuntime");

    QUnit.test("decorateEnterEditResult adds takeover confirmation for expired lock", function (assert) {
        var oResult = DetailWorkflowRuntime.decorateEnterEditResult({
            ok: false,
            effects: [],
            error: {
                code: MessageCodeConstants.DETAIL.EXPIRED
            }
        }, {
            rootId: "CHK-1"
        });
        var oConfirmEffect = (oResult.effects || []).filter(function (oEffect) {
            return oEffect.type === "confirm";
        })[0];

        assert.ok(oConfirmEffect, "confirm effect is added");
        assert.strictEqual(oConfirmEffect.textKey, MessageKeyConstants.DETAIL.LOCK_EXPIRED_TAKEOVER_PROMPT, "expired lock prompt is used");
    });

    QUnit.test("decorateEnterEditResult adds integration confirmation before edit flow", function (assert) {
        var oResult = DetailWorkflowRuntime.decorateEnterEditResult({
            ok: false,
            effects: [],
            error: {
                code: MessageCodeConstants.DETAIL.INTEGRATION_CONFIRM_REQUIRED
            }
        }, {
            rootId: "CHK-2"
        });
        var oConfirmEffect = (oResult.effects || []).filter(function (oEffect) {
            return oEffect.type === "confirm";
        })[0];

        assert.ok(oConfirmEffect, "integration confirm effect is added");
        assert.strictEqual(oConfirmEffect.textKey, MessageKeyConstants.DETAIL.INTEGRATION_EDIT_CONFIRM, "integration prompt key is used");
        assert.strictEqual(oConfirmEffect.payload.confirmedIntegration, true, "confirm payload bypasses second prompt");
    });

    QUnit.test("buildDiscardEffects resets dirty and pending navigation state", function (assert) {
        var aEffects = DetailWorkflowRuntime.buildDiscardEffects({
            get: function (_sModel, sPath) {
                if (sPath === DetailContracts.MODEL_PATHS.BASE) {
                    return { root: { id: "CHK-1" } };
                }
                return null;
            }
        });
        var aPaths = aEffects.filter(function (oEffect) {
            return oEffect.type === "modelPatch";
        }).map(function (oEffect) {
            return oEffect.path;
        });

        assert.ok(aPaths.indexOf("/workflow/dirty") >= 0, "dirty flag is reset");
        assert.ok(aPaths.indexOf("/pendingNavigationIntent") >= 0, "pending navigation is cleared");
    });
});
