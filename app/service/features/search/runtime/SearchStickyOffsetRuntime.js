sap.ui.define([], function () {
    "use strict";

    function readCssSizePx(sName) {
        var oRoot;
        var sValue;
        var iValue;
        if (typeof document === "undefined" || !document.documentElement || typeof window === "undefined" || typeof window.getComputedStyle !== "function") {
            return 0;
        }
        oRoot = document.documentElement;
        sValue = window.getComputedStyle(oRoot).getPropertyValue(sName) || "";
        iValue = parseFloat(String(sValue).trim());
        return isFinite(iValue) ? iValue : 0;
    }

    function isCompactStickyViewport(iBreakpointPx) {
        return typeof window !== "undefined"
            && Number(window.innerWidth || 0) > 0
            && Number(window.innerWidth || 0) <= iBreakpointPx;
    }

    function resolveShellHeaderOffset(iMinOffsetPx, iPaddingPx, oScrollHost) {
        var iShellOffset = Math.ceil(readCssSizePx("--app-shell-offset"));
        var iHostTop = 0;
        if (oScrollHost && oScrollHost.getBoundingClientRect) {
            iHostTop = Math.ceil(oScrollHost.getBoundingClientRect().top || 0);
        }
        return Math.max(iMinOffsetPx, Math.max(0, iShellOffset - iHostTop) + iPaddingPx);
    }

    return {
        isCompactStickyViewport: isCompactStickyViewport,
        resolveShellHeaderOffset: resolveShellHeaderOffset
    };
});
