(function () {
    "use strict";

    var iFrame = 0;
    var oObserver = null;
    var oToolbarObserver = null;
    var oHost = null;
    var oToolbar = null;
    var bBound = false;
    var bSearchRoute = false;

    function onSearchRoute() {
        var sHash = String(window.location.hash || "");
        return sHash === "#" || sHash.indexOf("/search") >= 0;
    }

    function resolveScrollHost() {
        var aNodes = Array.prototype.slice.call(document.querySelectorAll(".sapMPageEnableScrolling, .sapMPageScroll"));
        var oResolved = aNodes.find(function (oNode) {
            return oNode && oNode.scrollHeight > oNode.clientHeight + 4;
        });
        return oResolved || document.scrollingElement || document.documentElement || document.body;
    }

    function clearToolbarState() {
        if (!oToolbar) {
            return;
        }
        oToolbar.classList.remove("searchToolbarStickyRuntimeActive");
        oToolbar.style.transform = "";
        oToolbar.style.zIndex = "";
        if (oToolbar.dataset) {
            delete oToolbar.dataset.searchStickyRuntimeTranslateY;
        }
    }

    function refreshTargets() {
        oToolbar = document.querySelector(".searchResultsTable .sapUiCompSmartTableToolbar");
        oHost = resolveScrollHost();
        bSearchRoute = onSearchRoute();
        if (!bSearchRoute || !oToolbar || !oHost) {
            clearToolbarState();
            return;
        }
        oToolbar.classList.add("searchToolbarStickyRuntimeActive");
        oToolbar.style.transform = "";
        oToolbar.style.zIndex = "11";
    }

    function applyResultsToolbarDock() {
        refreshTargets();
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
        if (bBound) {
            scheduleApply();
            return;
        }
        bBound = true;
        refreshTargets();

        window.addEventListener("resize", scheduleApply, { passive: true });
        window.addEventListener("hashchange", scheduleApply, { passive: true });
        document.addEventListener("transitionend", scheduleApply, true);
        document.addEventListener("animationend", scheduleApply, true);

        if (!oObserver && typeof MutationObserver === "function") {
            oObserver = new MutationObserver(function () {
                refreshTargets();
                scheduleApply();
            });
            oObserver.observe(document.body || document.documentElement, {
                childList: true,
                subtree: true
            });
        }
        if (!oToolbarObserver && typeof MutationObserver === "function") {
            oToolbarObserver = new MutationObserver(scheduleApply);
            if (oToolbar) {
                oToolbarObserver.observe(oToolbar, {
                    childList: true,
                    subtree: true,
                    attributes: true,
                    attributeFilter: ["class", "style"]
                });
            }
        }
        scheduleApply();
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", bind, { once: true });
    } else {
        bind();
    }
}());
