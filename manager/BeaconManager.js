sap.ui.define([], function () {
    "use strict";

    function _bestEffortRelease(fnRelease) {
        try {
            var vPayload = fnRelease && fnRelease();
            var oPayload = vPayload && typeof vPayload === "object" ? vPayload : null;

            if (oPayload && oPayload.url) {
                var sBody = JSON.stringify(oPayload.body || {});
                if (navigator.sendBeacon) {
                    var bSent = navigator.sendBeacon(oPayload.url, new Blob([sBody], { type: "application/json" }));
                    if (window.console && window.console.info) {
                        window.console.info("[beacon] release sent", { transport: "beacon", sent: !!bSent, url: oPayload.url });
                    }
                    return;
                }

                var xhr = new XMLHttpRequest();
                xhr.open("POST", oPayload.url, false);
                xhr.setRequestHeader("Content-Type", "application/json");
                xhr.send(sBody);
                if (window.console && window.console.info) {
                    window.console.info("[beacon] release sent", { transport: "xhr-sync", url: oPayload.url });
                }
                return;
            }
        } catch (e) {
            if (window.console && window.console.warn) {
                window.console.warn("[beacon] release failed", e);
            }
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

            function onPageHide() {
                onLeave();
            }

            window.addEventListener("beforeunload", onLeave);
            window.addEventListener("pagehide", onPageHide);
            document.addEventListener("visibilitychange", onVisibility);

            return function unregister() {
                window.removeEventListener("beforeunload", onLeave);
                window.removeEventListener("pagehide", onPageHide);
                document.removeEventListener("visibilitychange", onVisibility);
            };
        }
    };
});
