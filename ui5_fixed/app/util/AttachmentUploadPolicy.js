sap.ui.define([], function () {
    "use strict";

    var DEFAULT_UPLOAD_POLICY = {
        maxSizeMb: 0,
        allowedMime: [],
        allowedExtensions: [],
        scan: "always-ok"
    };

    function normalizeStringList(aValues) {
        return (Array.isArray(aValues) ? aValues : []).map(function (vValue) {
            return String(vValue || "").trim().toLowerCase();
        }).filter(Boolean);
    }

    function normalizeMaxSizeMb(vValue) {
        var iParsed = Number(vValue);
        if (!Number.isFinite(iParsed) || iParsed < 0) {
            return Number(DEFAULT_UPLOAD_POLICY.maxSizeMb);
        }
        return iParsed;
    }

    function normalizeUploadPolicy(oPolicy) {
        var oSource = oPolicy && typeof oPolicy === "object" ? oPolicy : {};
        return Object.assign({}, oSource, {
            scan: String(oSource.scan || DEFAULT_UPLOAD_POLICY.scan),
            maxSizeMb: normalizeMaxSizeMb(oSource.maxSizeMb),
            allowedMime: normalizeStringList(oSource.allowedMime),
            allowedExtensions: normalizeStringList(oSource.allowedExtensions)
        });
    }

    return {
        DEFAULT_UPLOAD_POLICY: normalizeUploadPolicy(DEFAULT_UPLOAD_POLICY),
        normalizeUploadPolicy: normalizeUploadPolicy
    };
});
