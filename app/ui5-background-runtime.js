(function () {
    "use strict";

    var rm = window.matchMedia("(prefers-reduced-motion: reduce)");
    var bgWrap = document.getElementById("ui5-bg");
    var lightSvg = document.getElementById("bgLight");
    var darkSvg = document.getElementById("bgDark");
    var root = document.documentElement;
    var body = document.body;

    if (!bgWrap || !body) {
        return;
    }

    bgWrap.style.display = "block";
    [lightSvg, darkSvg].forEach(function (oLayer) {
        if (oLayer && oLayer.style) {
            oLayer.style.display = "block";
        }
    });
    Array.prototype.slice.call(bgWrap.querySelectorAll(".bg, .dither, .grain, .paper")).forEach(function (oLayer) {
        if (oLayer && oLayer.style) {
            oLayer.style.display = "block";
        }
    });

    var lBase = document.getElementById("lightBase");
    var lOil = document.getElementById("lightOilMicro");
    var lAur = document.getElementById("lightAurora");
    var lAurM = document.getElementById("lightAuroraMicro");

    var dBase = document.getElementById("dLayerBase");
    var dCoarse = document.getElementById("dLayerCoarse");
    var dMicro = document.getElementById("dLayerMicro");
    var dHaze = document.getElementById("dLayerHaze");

    var state = {
        theme: "light",
        enabled: true,
        interactive: true,
        visible: !document.hidden,
        quality: "high",
        resizing: false
    };

    var tx = 0;
    var ty = 0;
    var cx = 0;
    var cy = 0;
    var sy = 0;
    var raf = 0;

    var lastT = performance.now();
    var dtAvg = 16.6;
    var badFrames = 0;
    var goodFrames = 0;

    function parseBooleanAttr(vValue, bFallback) {
        if (typeof vValue === "boolean") {
            return vValue;
        }
        if (vValue === null || vValue === undefined || vValue === "") {
            return !!bFallback;
        }
        return String(vValue).toLowerCase() === "true";
    }

    function pauseSVG() {
        try {
            if (lightSvg && typeof lightSvg.pauseAnimations === "function") {
                lightSvg.pauseAnimations();
            }
        } catch (e) {
            // Ignore browser-specific SVG animation controls.
        }
        try {
            if (darkSvg && typeof darkSvg.pauseAnimations === "function") {
                darkSvg.pauseAnimations();
            }
        } catch (e2) {
            // Ignore browser-specific SVG animation controls.
        }
    }

    function unpauseSVG() {
        try {
            if (lightSvg && typeof lightSvg.unpauseAnimations === "function") {
                lightSvg.unpauseAnimations();
            }
        } catch (e) {
            // Ignore browser-specific SVG animation controls.
        }
        try {
            if (darkSvg && typeof darkSvg.unpauseAnimations === "function") {
                darkSvg.unpauseAnimations();
            }
        } catch (e2) {
            // Ignore browser-specific SVG animation controls.
        }
    }

    function setTheme(sTheme) {
        state.theme = sTheme === "dark" ? "dark" : "light";
        body.dataset.theme = state.theme;
        body.classList.toggle("appDark", state.theme === "dark");
        body.classList.toggle("appLight", state.theme !== "dark");
        body.classList.toggle("nightMode", state.theme === "dark");
        body.classList.toggle("lightMode", state.theme !== "dark");
        root.setAttribute("data-bg-theme", state.theme);
        return state.theme;
    }

    function setEnabled(bEnabled) {
        state.enabled = !!bEnabled;
        body.classList.toggle("bg-paused", !state.enabled);
        bgWrap.style.opacity = "1";

        if (!state.visible || !state.enabled) {
            pauseSVG();
        } else {
            unpauseSVG();
        }

        ensureLoop();
        return state.enabled;
    }

    function setInteractive(bInteractive) {
        state.interactive = !!bInteractive && !rm.matches;
        root.setAttribute("data-bg-interactive", state.interactive ? "true" : "false");
        ensureLoop();
        return state.interactive;
    }

    function applyParallax() {
        var lerp = 0.08;
        var css = getComputedStyle(root);
        var pBase;
        var pCoarse;
        var pMicro;
        var pHaze;
        var y;
        var norm;
        var dx;
        var dy;

        cx += (tx - cx) * lerp;
        cy += (ty - cy) * lerp;

        dx = cx;
        dy = cy;

        pBase = parseFloat(css.getPropertyValue("--parallax-base")) || 10;
        pCoarse = parseFloat(css.getPropertyValue("--parallax-coarse")) || 18;
        pMicro = parseFloat(css.getPropertyValue("--parallax-micro")) || 26;
        pHaze = parseFloat(css.getPropertyValue("--parallax-haze")) || 12;

        y = window.scrollY || 0;
        norm = Math.max(-1, Math.min(1, y / 1200));
        sy += (norm - sy) * 0.06;

        if (lBase) {
            lBase.setAttribute("transform", "translate(" + (dx * 7).toFixed(2) + " " + (dy * 7 + sy * 4).toFixed(2) + ")");
        }
        if (lOil) {
            lOil.setAttribute("transform", "translate(" + (dx * 10).toFixed(2) + " " + (dy * 10 + sy * 5).toFixed(2) + ")");
        }
        if (lAur) {
            lAur.setAttribute("transform", "translate(" + (dx * 14).toFixed(2) + " " + (dy * 14 + sy * 7).toFixed(2) + ")");
        }
        if (lAurM) {
            lAurM.setAttribute("transform", "translate(" + (dx * 18).toFixed(2) + " " + (dy * 18 + sy * 9).toFixed(2) + ")");
        }

        if (dBase) {
            dBase.setAttribute("transform", "translate(" + (dx * pBase).toFixed(2) + " " + (dy * pBase + sy * 6).toFixed(2) + ")");
        }
        if (dCoarse) {
            dCoarse.setAttribute("transform", "translate(" + (dx * pCoarse).toFixed(2) + " " + (dy * pCoarse + sy * 10).toFixed(2) + ")");
        }
        if (dMicro) {
            dMicro.setAttribute("transform", "translate(" + (dx * pMicro).toFixed(2) + " " + (dy * pMicro + sy * 14).toFixed(2) + ")");
        }
        if (dHaze) {
            dHaze.setAttribute("transform", "translate(" + (dx * pHaze).toFixed(2) + " " + (dy * pHaze + sy * 7).toFixed(2) + ")");
        }
    }

    function adaptQuality(iDt) {
        dtAvg = dtAvg * 0.92 + iDt * 0.08;

        if (dtAvg > 22) {
            badFrames += 1;
            goodFrames = Math.max(0, goodFrames - 1);
        } else {
            goodFrames += 1;
            badFrames = Math.max(0, badFrames - 1);
        }

        if (badFrames > 45 && state.quality !== "low") {
            state.quality = "low";
            body.dataset.quality = "low";
        }

        if (goodFrames > 120 && state.quality !== "high") {
            state.quality = "high";
            body.dataset.quality = "high";
        }

        if (dtAvg > 28 && state.interactive) {
            setInteractive(false);
        }
    }

    function loop(iNow) {
        var dt;

        raf = 0;
        if (!state.visible || !state.enabled || state.resizing) {
            return;
        }

        dt = iNow - lastT;
        lastT = iNow;

        adaptQuality(dt);

        if (state.interactive) {
            applyParallax();
            raf = window.requestAnimationFrame(loop);
        }
    }

    function ensureLoop() {
        if (raf) {
            window.cancelAnimationFrame(raf);
            raf = 0;
        }

        if (!state.visible || !state.enabled || !state.interactive || state.resizing) {
            return;
        }

        lastT = performance.now();
        raf = window.requestAnimationFrame(loop);
    }

    var _iPointerThrottle = 0;

    function onPointerMove(oEvent) {
        var iNow;
        var w;
        var h;
        if (!state.interactive) {
            return;
        }

        iNow = performance.now();
        if (iNow - _iPointerThrottle < 33) {
            return;
        }
        _iPointerThrottle = iNow;
        w = window.innerWidth || 1;
        h = window.innerHeight || 1;
        tx = Math.max(-1, Math.min(1, oEvent.clientX / w * 2 - 1));
        ty = Math.max(-1, Math.min(1, oEvent.clientY / h * 2 - 1));
        ensureLoop();
    }

    function onPointerLeave() {
        tx = 0;
        ty = 0;
        ensureLoop();
    }

    function setVisible(bVisible) {
        state.visible = !!bVisible;
        if (!state.visible) {
            pauseSVG();
        } else if (state.enabled) {
            unpauseSVG();
        }
        ensureLoop();
    }

    function destroy() {
        if (raf) {
            window.cancelAnimationFrame(raf);
            raf = 0;
        }
        document.removeEventListener("visibilitychange", onVisibilityChange);
        window.removeEventListener("pointermove", onPointerMove);
        window.removeEventListener("pointerleave", onPointerLeave);
        window.removeEventListener("scroll", onScroll);
        if (rm.removeEventListener) {
            rm.removeEventListener("change", onReducedMotionChange);
        }
        if (bgWrap && bgWrap.parentNode) {
            bgWrap.parentNode.removeChild(bgWrap);
        }
    }

    function onVisibilityChange() {
        setVisible(!document.hidden);
    }

    function onScroll() {
        ensureLoop();
    }

    function onReducedMotionChange(oEvent) {
        if (oEvent && oEvent.matches) {
            setInteractive(false);
        }
    }

    function initFromStartup() {
        var sTheme = body.getAttribute("data-theme") || (body.classList.contains("appDark") ? "dark" : "light");
        var bEnabled = parseBooleanAttr(body.getAttribute("data-bg-enabled"), true);
        var bInteractive = parseBooleanAttr(body.getAttribute("data-bg-interactive"), true);

        state.quality = (body.getAttribute("data-quality") || "high").toLowerCase() === "low" ? "low" : "high";
        body.dataset.quality = state.quality;

        setTheme(sTheme);
        setEnabled(rm.matches ? false : bEnabled);
        setInteractive(rm.matches ? false : bInteractive);
    }

    initFromStartup();

    document.addEventListener("visibilitychange", onVisibilityChange, { passive: true });
    window.addEventListener("pointermove", onPointerMove, { passive: true });
    window.addEventListener("pointerleave", onPointerLeave, { passive: true });
    window.addEventListener("scroll", onScroll, { passive: true });
    if (rm.addEventListener) {
        rm.addEventListener("change", onReducedMotionChange);
    }

    window.Ui5Bg = {
        setTheme: setTheme,
        toggleTheme: function () {
            return setTheme(state.theme === "light" ? "dark" : "light");
        },
        setEnabled: setEnabled,
        toggleEnabled: function () {
            return setEnabled(!state.enabled);
        },
        setInteractive: setInteractive,
        toggleInteractive: function () {
            return setInteractive(!state.interactive);
        },
        getState: function () {
            return {
                theme: state.theme,
                enabled: state.enabled,
                interactive: state.interactive,
                visible: state.visible,
                quality: state.quality,
                resizing: state.resizing
            };
        },
        destroy: destroy,
        onResizeStart: function () {
            state.resizing = true;
            pauseSVG();
            if (bgWrap) {
                bgWrap.style.transition = "none";
                bgWrap.style.opacity = "1";
            }
            ensureLoop();
        },
        onResizeEnd: function () {
            state.resizing = false;
            if (bgWrap) {
                bgWrap.style.transition = "";
                bgWrap.style.opacity = "1";
            }
            if (state.enabled && state.visible) {
                unpauseSVG();
            }
            ensureLoop();
        }
    };
}());
