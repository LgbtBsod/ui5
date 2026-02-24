sap.ui.define([], function () {
    "use strict";

    function _bestEffortRelease(fnRelease) {
        try {
            var vPayload = fnRelease && fnRelease();
            var oPayload = vPayload && typeof vPayload === "object" ? vPayload : null;

            if (oPayload && oPayload.url) {
                var sBody = JSON.stringify(oPayload.body || {});
                if (navigator.sendBeacon) {
                    navigator.sendBeacon(oPayload.url, new Blob([sBody], { type: "application/json" }));
                    return;
                }

                var xhr = new XMLHttpRequest();
                xhr.open("POST", oPayload.url, false);
                xhr.setRequestHeader("Content-Type", "application/json");
                xhr.send(sBody);
                return;
            }
        } catch (e) {
            // best effort fallback
        }
    }

    return {
        register: function (fnRelease) {
            function onLeave() {
                _bestEffortRelease(fnRelease);
            }

            function onVisibility() {
                if (document.visibilityState === "hidden") {
                    onLeave();
                }
            }

            window.addEventListener("beforeunload", onLeave);
            document.addEventListener("visibilitychange", onVisibility);

            return function unregister() {
                window.removeEventListener("beforeunload", onLeave);
                document.removeEventListener("visibilitychange", onVisibility);
            };
        }
    };
});
