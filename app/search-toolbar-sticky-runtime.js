(function () {
    "use strict";

    var iFrame = 0;
    var iRetryBudget = 0;
    var oToolbar = null;
    var bBound = false;

    function isSearchRoute() {
        var sHash = String(window.location.hash || "");
        return sHash === "#" || sHash.indexOf("/search") >= 0;
    }

    function clearToolbarState() {
        if (!oToolbar) {
            return;
        }
        oToolbar.classList.remove("searchToolbarStickyRuntimeActive");
        oToolbar.style.transform = "";
        oToolbar.style.zIndex = "";
        oToolbar = null;
    }

    function resolveToolbar() {
        if (!isSearchRoute()) {
            clearToolbarState();
            return null;
        }
        oToolbar = document.querySelector(".searchResultsTable .sapUiCompSmartTableToolbar");
        if (!oToolbar) {
            clearToolbarState();
            return null;
        }
        oToolbar.classList.add("searchToolbarStickyRuntimeActive");
        oToolbar.style.transform = "";
        oToolbar.style.zIndex = "11";
        return oToolbar;
    }

    function scheduleRefresh() {
        if (iFrame) {
            return;
        }
        iFrame = window.requestAnimationFrame(function () {
            var oResolvedToolbar;
            iFrame = 0;
            oResolvedToolbar = resolveToolbar();
            if (!oResolvedToolbar && isSearchRoute() && iRetryBudget > 0) {
                iRetryBudget -= 1;
                scheduleRefresh();
            }
        });
    }

    function triggerSearchRouteRefresh() {
        iRetryBudget = isSearchRoute() ? 24 : 0;
        scheduleRefresh();
    }

    function bind() {
        if (bBound) {
            triggerSearchRouteRefresh();
            return;
        }
        bBound = true;
        window.addEventListener("hashchange", triggerSearchRouteRefresh, { passive: true });
        window.addEventListener("resize", scheduleRefresh, { passive: true });
        triggerSearchRouteRefresh();
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", bind, { once: true });
    } else {
        bind();
    }
}());
