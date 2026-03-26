sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NormalizedError",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentSaveGuardContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (Effects, NormalizedError, ComponentSaveGuardContracts, EffectFeedbackContracts, FeedbackConstants, ModelContracts, MessageKeyConstants) {
    "use strict";

    var BANNER_TEXT_KEY = ComponentSaveGuardContracts.BANNER_TEXT_KEY;
    var FALLBACK_TEXT_KEYS = EffectFeedbackContracts.FALLBACK_TEXT_KEYS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function buildNormalizedError(mInput) {
        return NormalizedError.create(mInput || {});
    }

    function normalize(vError) {
        var aRules;
        var oMatch;
        if (!vError) {
            return buildNormalizedError({ kind: NormalizedError.KINDS.UNKNOWN });
        }
        var sMessage = String(vError.message || vError.responseText || "");
        var sCode = String(vError.code || vError.statusCode || "UNKNOWN");
        var iStatusCode = Number(vError.statusCode || vError.status || 0) || 0;
        var oHeaders = vError.responseHeaders || {};
        var sCorrelationId = String(oHeaders["x-correlation-id"] || oHeaders["x-request-id"] || oHeaders["x-vcap-request-id"] || "").trim();

        aRules = [
            {
                match: function () { return /lock/i.test(sMessage) || /LOCK/.test(sCode); },
                map: function () {
                    return { kind: NormalizedError.KINDS.LOCK, code: sCode, messageKey: "lockConflictError", retriable: true, severity: FeedbackConstants.SEVERITY.WARNING, params: { correlationId: sCorrelationId } };
                }
            },
            {
                match: function () { return /conflict/i.test(sMessage) || iStatusCode === 409; },
                map: function () {
                    return { kind: NormalizedError.KINDS.CONFLICT, code: sCode, messageKey: "saveConflictError", retriable: true, severity: FeedbackConstants.SEVERITY.WARNING, params: { correlationId: sCorrelationId } };
                }
            },
            {
                match: function () { return /network|timeout/i.test(sMessage) || iStatusCode === 0 || iStatusCode === 408; },
                map: function () {
                    return { kind: NormalizedError.KINDS.NETWORK, code: sCode, messageKey: BANNER_TEXT_KEY.NETWORK_UNAVAILABLE, retriable: true, severity: FeedbackConstants.SEVERITY.WARNING, params: { correlationId: sCorrelationId } };
                }
            },
            {
                match: function () { return iStatusCode === 401 || iStatusCode === 403 || /SESSION|AUTH/.test(String(sCode || "").toUpperCase()); },
                map: function () {
                    return { kind: NormalizedError.KINDS.BACKEND, code: sCode || "AUTH_REQUIRED", messageKey: BANNER_TEXT_KEY.SESSION_EXPIRED, retriable: false, severity: FeedbackConstants.SEVERITY.ERROR, params: { correlationId: sCorrelationId } };
                }
            },
            {
                match: function () { return /validation/i.test(sMessage); },
                map: function () {
                    return { kind: NormalizedError.KINDS.VALIDATION, code: sCode, messageKey: MessageKeyConstants.DETAIL.REQUIRED_FIELD_HINT, severity: FeedbackConstants.SEVERITY.INFO };
                }
            },
            {
                match: function () { return iStatusCode >= 400; },
                map: function () {
                    return { kind: NormalizedError.KINDS.BACKEND, code: sCode, messageKey: FALLBACK_TEXT_KEYS.LOAD_ERROR, retriable: true, severity: FeedbackConstants.SEVERITY.ERROR, params: { correlationId: sCorrelationId } };
                }
            }
        ];

        oMatch = aRules.find(function (oRule) {
            return !!(oRule && typeof oRule.match === "function" && oRule.match());
        });
        if (oMatch && typeof oMatch.map === "function") {
            return buildNormalizedError(oMatch.map());
        }
        return buildNormalizedError({ kind: NormalizedError.KINDS.UNKNOWN, code: sCode });
    }

    function toEffects(oError) {
        var oNormalized = normalize(oError);
        var aEffects = oNormalized.kind === NormalizedError.KINDS.VALIDATION
            ? [Effects.modelPatch(STATE_MODEL, "/ui/feedback/inlineErrors", oNormalized.params)]
            : [];
        var sKind = String(oNormalized.kind || "").toUpperCase();
        var bGlobalIncident = sKind === "BACKEND" && String(oNormalized.messageKey || "") === BANNER_TEXT_KEY.SESSION_EXPIRED;

        if (sKind === "CONFLICT") {
            aEffects.push(Effects.dialog(EffectFeedbackContracts.IDS.CONFLICT, "open", { messageKey: oNormalized.messageKey }));
            return aEffects;
        }
        if (sKind === "LOCK" || sKind === "NETWORK" || sKind === "BACKEND") {
            aEffects.push(Effects.banner(bGlobalIncident ? FeedbackConstants.SCOPE.GLOBAL : FeedbackConstants.SCOPE.ROUTE, true, {
                scope: bGlobalIncident ? FeedbackConstants.SCOPE.GLOBAL : FeedbackConstants.SCOPE.ROUTE,
                messageKey: oNormalized.messageKey,
                severity: oNormalized.severity,
                correlationId: (oNormalized.params && oNormalized.params.correlationId) || ""
            }));
            return aEffects;
        }
        aEffects.push(Effects.toast(oNormalized.messageKey, oNormalized.severity));
        return aEffects;
    }

    return {
        normalize: normalize,
        toEffects: toEffects
    };
});
