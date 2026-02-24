sap.ui.define([], function () {
    "use strict";

    return {
        register: function (fnRelease) {
            function onLeave() {
                try {
                    fnRelease();
                } catch (e) {
                    // best effort
                }
            }

            window.addEventListener("beforeunload", onLeave);
            document.addEventListener("visibilitychange", function () {
                if (document.visibilityState === "hidden") {
                    onLeave();
                }
            });

            return function unregister() {
                window.removeEventListener("beforeunload", onLeave);
            };
        }
    };
});
