sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        INPUT_PATHS: Object.freeze({
            OBSERVED: "/observedInputValue",
            OBSERVER: "/observerInputValue"
        }),
        MODEL_PATHS: Object.freeze({
            OBSERVED_FULLNAME: "/current/basic/OBSERVED_FULLNAME",
            OBSERVER_FULLNAME: "/current/basic/OBSERVER_FULLNAME"
        }),
        TARGETS: Object.freeze({
            OBSERVED: "observed",
            OBSERVER: "observer"
        })
    });
});
