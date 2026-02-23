sap.ui.define([], function () {
    "use strict";

    return {

        startIdleTimer: function (stateModel) {
            const expires = Date.now() + 10 * 60 * 1000;
            stateModel.setProperty("/idleExpires", expires);
        },

        resetIdleTimer: function (stateModel) {
            this.startIdleTimer(stateModel);
        }

    };
});