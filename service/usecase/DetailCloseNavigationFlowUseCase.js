sap.ui.define([], function () {
    "use strict";

    function resolveCloseContext(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;
        return {
            objectId: oStateModel && typeof oStateModel.getProperty === "function" ? oStateModel.getProperty("/activeObjectId") : "",
            sessionId: oStateModel && typeof oStateModel.getProperty === "function" ? oStateModel.getProperty("/sessionId") : ""
        };
    }

    function runCloseNavigation(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;
        var oCtx = resolveCloseContext(mArgs);

        var fnFinalize = function () {
            if (typeof (mArgs && mArgs.prepareCloseNavigation) === "function") {
                mArgs.prepareCloseNavigation(oStateModel);
            }
            if (typeof (mArgs && mArgs.navigateToSearch) === "function") {
                mArgs.navigateToSearch();
                return true;
            }
            return false;
        };

        if (!oCtx.objectId || typeof (mArgs && mArgs.releaseLock) !== "function") {
            return Promise.resolve({
                released: false,
                navigated: fnFinalize(),
                skippedRelease: true
            });
        }

        return Promise.resolve(mArgs.releaseLock(oCtx.objectId, oCtx.sessionId)).then(function () {
            return {
                released: true,
                navigated: fnFinalize(),
                skippedRelease: false
            };
        }).catch(function (oError) {
            if (typeof (mArgs && mArgs.onReleaseError) === "function") {
                mArgs.onReleaseError(oError);
            }
            return {
                released: false,
                navigated: fnFinalize(),
                skippedRelease: false,
                releaseError: true
            };
        });
    }

    return {
        resolveCloseContext: resolveCloseContext,
        runCloseNavigation: runCloseNavigation
    };
});
