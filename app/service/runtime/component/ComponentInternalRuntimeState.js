sap.ui.define([], function () {
    "use strict";

    function createCacheState() {
        return {
            byRootKey: {},
            pristineSnapshot: null,
            keyMapping: {},
            lastServerState: null
        };
    }

    function createEnvState() {
        return {
            source: "",
            loadedAt: "",
            variables: {},
            timers: {}
        };
    }

    function resetStateObject(oState, fnCreateState) {
        var oSeed = fnCreateState();

        Object.keys(oState || {}).forEach(function (sKey) {
            delete oState[sKey];
        });
        Object.keys(oSeed).forEach(function (sKey) {
            oState[sKey] = oSeed[sKey];
        });
        return oState;
    }

    function reuseState(oExistingState, fnCreateState) {
        if (oExistingState && typeof oExistingState === "object") {
            return resetStateObject(oExistingState, fnCreateState);
        }
        return fnCreateState();
    }

    return {
        createCacheState: createCacheState,
        createEnvState: createEnvState,
        reuseState: reuseState
    };
});
