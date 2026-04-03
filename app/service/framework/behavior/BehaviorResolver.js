sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants"
], function (BehaviorRegistry, JsRuntime, MessageCodeConstants) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var TECHNICAL_CODES = MessageCodeConstants.TECHNICAL;

    function createMissingHandlerError(sScope, sOperation) {
        var oError = new Error(TECHNICAL_CODES.BEHAVIOR_HANDLER_MISSING);
        oError.code = TECHNICAL_CODES.BEHAVIOR_HANDLER_MISSING;
        oError.scope = String(sScope || "").trim();
        oError.operation = String(sOperation || "").trim();
        return oError;
    }

    function appendUnique(aTarget, vValue) {
        if (Array.isArray(vValue)) {
            vValue.forEach(function (vItem) {
                appendUnique(aTarget, vItem);
            });
            return;
        }
        vValue = String(vValue || "").trim();
        if (!vValue || aTarget.indexOf(vValue) >= 0) {
            return;
        }
        aTarget.push(vValue);
    }

    function collectCandidateIds(sOperation, mContext) {
        var aIds = [];
        var mConfig = mContext && mContext.config;
        var mOptions = mContext && mContext.options;
        var oEffect = mContext && mContext.effect;

        appendUnique(aIds, mContext && mContext.behaviorIds);
        appendUnique(aIds, mContext && mContext.behaviorId);
        appendUnique(aIds, mConfig && mConfig.behaviorIds);
        appendUnique(aIds, mConfig && mConfig.behaviorId);
        appendUnique(aIds, mConfig && mConfig.overrideId);
        appendUnique(aIds, mOptions && mOptions.behaviorIds);
        appendUnique(aIds, mOptions && mOptions.behaviorId);
        appendUnique(aIds, oEffect && oEffect.behaviorIds);
        appendUnique(aIds, oEffect && oEffect.behaviorId);

        if (mContext && mContext.key) {
            appendUnique(aIds, sOperation + ":" + mContext.key);
            appendUnique(aIds, mContext.key);
        }
        if (mContext && mContext.objectType) {
            appendUnique(aIds, sOperation + ":" + mContext.objectType);
            appendUnique(aIds, mContext.objectType);
        }
        if (mContext && mContext.entity) {
            appendUnique(aIds, sOperation + ":" + mContext.entity);
            appendUnique(aIds, mContext.entity);
        }
        if (mContext && mContext.route) {
            appendUnique(aIds, sOperation + ":" + mContext.route);
            appendUnique(aIds, mContext.route);
        }

        appendUnique(aIds, sOperation);
        return aIds;
    }

    function resolve(sScope, sOperation, mContext, mDefaultHandlers) {
        var aCandidates = collectCandidateIds(sOperation, mContext);
        var oResolved = BehaviorRegistry.resolveOverride(sScope, aCandidates)
            || BehaviorRegistry.resolveDefault(sScope, aCandidates);
        var fnFallback = mDefaultHandlers && mDefaultHandlers[sOperation];

        if (oResolved && typeof oResolved.handler === TYPE_FUNCTION) {
            return oResolved.handler;
        }
        return typeof fnFallback === TYPE_FUNCTION ? fnFallback : null;
    }

    function execute(sScope, sOperation, mContext, mDefaultHandlers) {
        var fnHandler = resolve(sScope, sOperation, mContext, mDefaultHandlers);
        if (typeof fnHandler !== TYPE_FUNCTION) {
            return Promise.reject(createMissingHandlerError(sScope, sOperation));
        }
        return Promise.resolve(fnHandler(mContext || {}));
    }

    function executeSync(sScope, sOperation, mContext, mDefaultHandlers) {
        var fnHandler = resolve(sScope, sOperation, mContext, mDefaultHandlers);
        if (typeof fnHandler !== TYPE_FUNCTION) {
            throw createMissingHandlerError(sScope, sOperation);
        }
        return fnHandler(mContext || {});
    }

    return {
        collectCandidateIds: collectCandidateIds,
        resolve: resolve,
        execute: execute,
        executeSync: executeSync
    };
});
