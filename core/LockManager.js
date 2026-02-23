sap.ui.define([], function () {
    "use strict";

    return {

        createLock: function (stateModel) {
            const expires = Date.now() + 5 * 60 * 1000;

            stateModel.setProperty("/isLocked", true);
            stateModel.setProperty("/lockExpires", expires);
            stateModel.setProperty("/sessionId", "SESSION-" + Date.now());
        },

        releaseLock: function (stateModel) {
            stateModel.setProperty("/isLocked", false);
            stateModel.setProperty("/lockExpires", null);
            stateModel.setProperty("/sessionId", null);
        }

    };
});