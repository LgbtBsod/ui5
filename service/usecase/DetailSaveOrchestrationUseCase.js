sap.ui.define([], function () {
    "use strict";

    var _inFlightByKey = Object.create(null);

    function _resolveIdempotencyKey(mArgs) {
        return String((mArgs && mArgs.idempotencyKey) || "").trim();
    }

    function _runSaveFlowCore(mArgs) {
        return mArgs.saveChecklist().then(function (oSavedChecklist) {
            return mArgs.loadChecklistCollection().then(function (aUpdatedCheckLists) {
                return {
                    savedChecklist: oSavedChecklist,
                    checkLists: aUpdatedCheckLists
                };
            });
        }).then(function (oResult) {
            mArgs.applySaveResult(oResult);
            return oResult;
        }).catch(function (oError) {
            return mArgs.handleSaveError(oError);
        });
    }

    function runSaveFlow(mArgs) {
        var sIdempotencyKey = _resolveIdempotencyKey(mArgs);
        if (!sIdempotencyKey) {
            return _runSaveFlowCore(mArgs);
        }

        if (_inFlightByKey[sIdempotencyKey]) {
            return _inFlightByKey[sIdempotencyKey];
        }

        _inFlightByKey[sIdempotencyKey] = _runSaveFlowCore(mArgs).finally(function () {
            delete _inFlightByKey[sIdempotencyKey];
        });

        return _inFlightByKey[sIdempotencyKey];
    }

    return {
        runSaveFlow: runSaveFlow
    };
});
