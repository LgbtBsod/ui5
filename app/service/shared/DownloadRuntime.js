sap.ui.define([], function () {
    "use strict";

    function triggerDownload(sUrl, sFileName) {
        var oLink;
        if (!sUrl || typeof document === "undefined" || !document.body || !document.createElement) {
            return false;
        }
        oLink = document.createElement("a");
        oLink.href = sUrl;
        oLink.download = String(sFileName || "download").trim() || "download";
        oLink.rel = "noopener";
        oLink.style.display = "none";
        document.body.appendChild(oLink);
        oLink.click();
        document.body.removeChild(oLink);
        return true;
    }

    function withObjectUrl(oBlob, fnUseUrl) {
        var sObjectUrl;
        if (!oBlob || typeof window === "undefined" || !window.URL || typeof window.URL.createObjectURL !== "function") {
            return false;
        }
        sObjectUrl = window.URL.createObjectURL(oBlob);
        try {
            return !!fnUseUrl(sObjectUrl);
        } finally {
            window.setTimeout(function () {
                window.URL.revokeObjectURL(sObjectUrl);
            }, 0);
        }
    }

    return {
        triggerDownload: triggerDownload,
        withObjectUrl: withObjectUrl
    };
});
