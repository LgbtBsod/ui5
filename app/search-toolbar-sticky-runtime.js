(function () {
    "use strict";

    var iFrame = 0;
    var oObserver = null;

    function resolveScrollHost() {
        var aNodes = Array.prototype.slice.call(document.querySelectorAll(".sapMPageEnableScrolling, .sapMPageScroll"));
        var oHost = aNodes.find(function (oNode) {
            return oNode && oNode.scrollHeight > oNode.clientHeight + 4;
        });
        return oHost || document.scrollingElement || document.documentElement || document.body;
    }

    function resolveOffsetWithinHost(oNode, oHost) {
        var oNodeRect;
        var oHostRect;
        if (!oNode || !oHost || !oNode.getBoundingClientRect || !oHost.getBoundingClientRect) {
            return 0;
        }
        oNodeRect = oNode.getBoundingClientRect();
        oHostRect = oHost.getBoundingClientRect();
        return Math.round((oNodeRect.top - oHostRect.top) + (oHost.scrollTop || 0));
    }

    function applyResultsToolbarDock() {
        var oToolbar;
        var oShell;
        var oHost;
        var iCurrentTranslate;
        var iNaturalTop;
        var iShellTop;
        var iShellHeight;
        var iToolbarHeight;
        var iTargetTop;
        var iDesiredTranslate;
        var iMaxTranslate;
        var iTranslate;

        if (String(window.location.hash || "").indexOf("/search") < 0 && String(window.location.hash || "") !== "#") {
            return;
        }

        oToolbar = document.querySelector(".searchResultsTable .sapUiCompSmartTableToolbar");
        oShell = document.querySelector(".searchResultsTable");
        oHost = resolveScrollHost();
        if (!oToolbar || !oShell || !oHost) {
            return;
        }

        iCurrentTranslate = Number((oToolbar.dataset && oToolbar.dataset.searchStickyRuntimeTranslateY) || 0);
        iNaturalTop = resolveOffsetWithinHost(oToolbar, oHost) - iCurrentTranslate;
        iShellTop = resolveOffsetWithinHost(oShell, oHost);
        iShellHeight = Math.round(oShell.getBoundingClientRect().height || 0);
        iToolbarHeight = Math.round(oToolbar.getBoundingClientRect().height || 0);
        iTargetTop = Math.round(parseFloat(getComputedStyle(oToolbar).top) || 0);
        iDesiredTranslate = Math.max(0, Math.round((oHost.scrollTop || 0) + iTargetTop - iNaturalTop));
        iMaxTranslate = Math.max(0, iShellTop + iShellHeight - iToolbarHeight - iNaturalTop - 8);
        iTranslate = Math.min(iDesiredTranslate, iMaxTranslate);

        oToolbar.style.transform = iTranslate > 0 ? "translateY(" + iTranslate + "px)" : "";
        oToolbar.style.zIndex = "11";
        if (oToolbar.dataset) {
            oToolbar.dataset.searchStickyRuntimeTranslateY = String(iTranslate);
        }
    }

    function scheduleApply() {
        if (iFrame) {
            return;
        }
        iFrame = window.requestAnimationFrame(function () {
            iFrame = 0;
            applyResultsToolbarDock();
        });
    }

    function bind() {
        if (!oObserver && typeof MutationObserver === "function") {
            oObserver = new MutationObserver(function () {
                scheduleApply();
            });
            oObserver.observe(document.documentElement, {
                childList: true,
                subtree: true,
                attributes: true,
                attributeFilter: ["class", "style"]
            });
        }

        window.addEventListener("scroll", scheduleApply, { passive: true, capture: true });
        window.addEventListener("resize", scheduleApply, { passive: true });
        window.addEventListener("hashchange", scheduleApply, { passive: true });
        document.addEventListener("click", scheduleApply, true);
        setInterval(scheduleApply, 500);
        scheduleApply();
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", bind, { once: true });
    } else {
        bind();
    }
}());
