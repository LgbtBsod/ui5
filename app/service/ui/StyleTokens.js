sap.ui.define([], function () {
    "use strict";

    var TOKENS = {
        SHELL_COMPACT: "pcctStyleShellCompact",
        SKELETON_SOFT: "pcctStyleSkeletonSoft",
        MOTION_SOFT: "pcctStyleMotionSoft",
        LABEL_ALIGN_ENTERPRISE: "pcctStyleLabelAlignEnterprise"
    };

    function isAllowed(sToken) {
        return Object.prototype.hasOwnProperty.call(TOKENS, String(sToken || ""));
    }

    function resolveClassName(sToken) {
        return isAllowed(sToken) ? TOKENS[sToken] : "";
    }

    return {
        TOKENS: TOKENS,
        isAllowed: isAllowed,
        resolveClassName: resolveClassName
    };
});
