(function () {
    "use strict";

    var BG_MARKUP = [
        '<div id="ui5-bg" aria-hidden="true">',
        '    <svg id="bgLight" class="bg" viewBox="0 0 1600 900" preserveAspectRatio="xMidYMid slice">',
        '        <defs>',
        '            <linearGradient id="lightBaseGradient" x1="0%" y1="0%" x2="100%" y2="100%">',
        '                <stop offset="0%" stop-color="#f6fbff"></stop>',
        '                <stop offset="42%" stop-color="#edf4ff"></stop>',
        '                <stop offset="100%" stop-color="#fff4fb"></stop>',
        '            </linearGradient>',
        '            <radialGradient id="lightAuroraGradientA" cx="20%" cy="22%" r="58%">',
        '                <stop offset="0%" stop-color="rgba(109, 226, 255, 0.78)"></stop>',
        '                <stop offset="55%" stop-color="rgba(109, 226, 255, 0.18)"></stop>',
        '                <stop offset="100%" stop-color="rgba(109, 226, 255, 0)"></stop>',
        '            </radialGradient>',
        '            <radialGradient id="lightAuroraGradientB" cx="78%" cy="18%" r="56%">',
        '                <stop offset="0%" stop-color="rgba(255, 181, 232, 0.62)"></stop>',
        '                <stop offset="58%" stop-color="rgba(255, 181, 232, 0.18)"></stop>',
        '                <stop offset="100%" stop-color="rgba(255, 181, 232, 0)"></stop>',
        '            </radialGradient>',
        '            <radialGradient id="lightAuroraGradientC" cx="48%" cy="80%" r="60%">',
        '                <stop offset="0%" stop-color="rgba(142, 255, 214, 0.48)"></stop>',
        '                <stop offset="62%" stop-color="rgba(142, 255, 214, 0.16)"></stop>',
        '                <stop offset="100%" stop-color="rgba(142, 255, 214, 0)"></stop>',
        '            </radialGradient>',
        '            <filter id="lightSoftBlur" x="-20%" y="-20%" width="140%" height="140%">',
        '                <feGaussianBlur stdDeviation="54"></feGaussianBlur>',
        '            </filter>',
        '        </defs>',
        '        <rect id="lightBase" width="1600" height="900" fill="url(#lightBaseGradient)"></rect>',
        '        <g id="lightOilMicro" filter="url(#lightSoftBlur)" opacity="0.9">',
        '            <ellipse cx="340" cy="220" rx="390" ry="280" fill="url(#lightAuroraGradientA)"></ellipse>',
        '            <ellipse cx="1220" cy="180" rx="360" ry="250" fill="url(#lightAuroraGradientB)"></ellipse>',
        '        </g>',
        '        <g id="lightAurora" filter="url(#lightSoftBlur)" opacity="0.78">',
        '            <ellipse cx="820" cy="710" rx="520" ry="260" fill="url(#lightAuroraGradientC)"></ellipse>',
        '            <ellipse cx="1380" cy="620" rx="300" ry="220" fill="rgba(112, 195, 255, 0.18)"></ellipse>',
        '        </g>',
        '        <g id="lightAuroraMicro" filter="url(#lightSoftBlur)" opacity="0.6">',
        '            <ellipse cx="860" cy="120" rx="240" ry="120" fill="rgba(255, 233, 164, 0.26)"></ellipse>',
        '            <ellipse cx="170" cy="700" rx="230" ry="160" fill="rgba(124, 221, 255, 0.18)"></ellipse>',
        '        </g>',
        '    </svg>',
        '    <svg id="bgDark" class="bg" viewBox="0 0 1600 900" preserveAspectRatio="xMidYMid slice">',
        '        <defs>',
        '            <linearGradient id="darkBaseGradient" x1="0%" y1="0%" x2="100%" y2="100%">',
        '                <stop offset="0%" stop-color="#07101e"></stop>',
        '                <stop offset="46%" stop-color="#0a1730"></stop>',
        '                <stop offset="100%" stop-color="#060b18"></stop>',
        '            </linearGradient>',
        '            <radialGradient id="darkGlowA" cx="16%" cy="20%" r="62%">',
        '                <stop offset="0%" stop-color="rgba(43, 213, 225, 0.44)"></stop>',
        '                <stop offset="56%" stop-color="rgba(43, 213, 225, 0.14)"></stop>',
        '                <stop offset="100%" stop-color="rgba(43, 213, 225, 0)"></stop>',
        '            </radialGradient>',
        '            <radialGradient id="darkGlowB" cx="80%" cy="24%" r="58%">',
        '                <stop offset="0%" stop-color="rgba(109, 93, 255, 0.34)"></stop>',
        '                <stop offset="60%" stop-color="rgba(109, 93, 255, 0.12)"></stop>',
        '                <stop offset="100%" stop-color="rgba(109, 93, 255, 0)"></stop>',
        '            </radialGradient>',
        '            <radialGradient id="darkGlowC" cx="54%" cy="82%" r="60%">',
        '                <stop offset="0%" stop-color="rgba(255, 92, 180, 0.26)"></stop>',
        '                <stop offset="60%" stop-color="rgba(255, 92, 180, 0.1)"></stop>',
        '                <stop offset="100%" stop-color="rgba(255, 92, 180, 0)"></stop>',
        '            </radialGradient>',
        '            <filter id="darkSoftBlur" x="-20%" y="-20%" width="140%" height="140%">',
        '                <feGaussianBlur stdDeviation="58"></feGaussianBlur>',
        '            </filter>',
        '        </defs>',
        '        <rect id="dLayerBase" width="1600" height="900" fill="url(#darkBaseGradient)"></rect>',
        '        <g id="dLayerCoarse" filter="url(#darkSoftBlur)" opacity="0.86">',
        '            <ellipse cx="300" cy="220" rx="400" ry="300" fill="url(#darkGlowA)"></ellipse>',
        '            <ellipse cx="1260" cy="210" rx="360" ry="260" fill="url(#darkGlowB)"></ellipse>',
        '        </g>',
        '        <g id="dLayerMicro" filter="url(#darkSoftBlur)" opacity="0.72">',
        '            <ellipse cx="860" cy="760" rx="540" ry="250" fill="url(#darkGlowC)"></ellipse>',
        '            <ellipse cx="640" cy="120" rx="220" ry="110" fill="rgba(82, 220, 214, 0.12)"></ellipse>',
        '        </g>',
        '        <g id="dLayerHaze" filter="url(#darkSoftBlur)" opacity="0.58">',
        '            <ellipse cx="1450" cy="620" rx="250" ry="210" fill="rgba(82, 220, 214, 0.1)"></ellipse>',
        '            <ellipse cx="160" cy="730" rx="220" ry="150" fill="rgba(137, 113, 255, 0.12)"></ellipse>',
        '        </g>',
        '    </svg>',
        '    <div class="dither"></div>',
        '    <div class="grain"></div>',
        '    <div class="paper"></div>',
        '</div>'
    ].join("");

    function ensureBackgroundHost() {
        var oExisting = document.getElementById("ui5-bg");
        if (oExisting) {
            return oExisting;
        }
        document.body.insertAdjacentHTML("afterbegin", BG_MARKUP);
        return document.getElementById("ui5-bg");
    }

    var rm = window.matchMedia("(prefers-reduced-motion: reduce)");
    var bgWrap = ensureBackgroundHost();
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
