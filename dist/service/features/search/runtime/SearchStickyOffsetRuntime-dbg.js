sap.ui.define([], function () {
    "use strict";

    function isCompactStickyViewport(iBreakpointPx) {
        return typeof window !== "undefined"
            && Number(window.innerWidth || 0) > 0
            && Number(window.innerWidth || 0) <= iBreakpointPx;
    }

    function resolveShellHeaderOffset(iMinOffsetPx, iPaddingPx, oScrollHost) {
        var oShellHeader = typeof document !== "undefined" ? document.querySelector(".appShellHeader") : null;
        var iShellBottom = 0;
        var iHostTop = 0;
        if (oShellHeader && oShellHeader.getBoundingClientRect) {
            iShellBottom = Math.ceil(oShellHeader.getBoundingClientRect().bottom || 0);
        }
        if (oScrollHost && oScrollHost.getBoundingClientRect) {
            iHostTop = Math.ceil(oScrollHost.getBoundingClientRect().top || 0);
        }
        return Math.max(iMinOffsetPx, iShellBottom - iHostTop + iPaddingPx);
    }

    return {
        isCompactStickyViewport: isCompactStickyViewport,
        resolveShellHeaderOffset: resolveShellHeaderOffset
    };
});
