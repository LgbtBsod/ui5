(function () {
    "use strict";

    var HOST_ID = "ui5-bg-host";
    var SELECTOR = "#" + HOST_ID;
    var BODY_ATTR_THEME = "data-theme";
    var BODY_ATTR_BG_ENABLED = "data-bg-enabled";
    var REDUCED_MOTION_QUERY = "(prefers-reduced-motion: reduce)";

    function buildSvgMarkup(sVariant) {
        if (sVariant === "dark") {
            return [
                '<svg class="ui5BgScene ui5BgScene--dark" viewBox="0 0 1000 1000" preserveAspectRatio="none" aria-hidden="true" xmlns="http://www.w3.org/2000/svg">',
                '<defs>',
                '<linearGradient id="bg-dark-base" x1="0" y1="0" x2="1" y2="1">',
                '<stop offset="0%" stop-color="#080d1a"/><stop offset="16%" stop-color="#0e1838"/><stop offset="34%" stop-color="#162044"/><stop offset="52%" stop-color="#161e42"/><stop offset="70%" stop-color="#0f1c3a"/><stop offset="86%" stop-color="#0a1028"/><stop offset="100%" stop-color="#080d1a"/>',
                '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="360 0.5 0.5" dur="280s" repeatCount="indefinite"/>',
                '</linearGradient>',
                '<linearGradient id="bg-dark-mid" x1="1" y1="0" x2="0" y2="1">',
                '<stop offset="0%" stop-color="#0a061c"/><stop offset="40%" stop-color="#160e38"/><stop offset="80%" stop-color="#090c24"/><stop offset="100%" stop-color="#0a061c"/>',
                '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="-360 0.5 0.5" dur="430s" repeatCount="indefinite"/>',
                '</linearGradient>',
                '<linearGradient id="bg-dark-sheen-cool" x1="0" y1="1" x2="1" y2="0">',
                '<stop offset="0%" stop-color="#30c8ea" stop-opacity="0.24"/><stop offset="35%" stop-color="#6888ff" stop-opacity="0.18"/><stop offset="70%" stop-color="#28d0f0" stop-opacity="0.21"/><stop offset="100%" stop-color="#30c8ea" stop-opacity="0.22"/>',
                '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="-360 0.5 0.5" dur="360s" repeatCount="indefinite"/>',
                '</linearGradient>',
                '<linearGradient id="bg-dark-sheen-accent" x1="1" y1="1" x2="0" y2="0">',
                '<stop offset="0%" stop-color="#8850ff" stop-opacity="0.13"/><stop offset="30%" stop-color="#c87820" stop-opacity="0.10"/><stop offset="60%" stop-color="#9060ff" stop-opacity="0.13"/><stop offset="100%" stop-color="#8850ff" stop-opacity="0.12"/>',
                '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="360 0.5 0.5" dur="480s" repeatCount="indefinite"/>',
                '</linearGradient>',
                '<linearGradient id="bg-dark-thread-a" x1="0" y1="0" x2="1" y2="0">',
                '<stop offset="0%" stop-color="#38d8ff" stop-opacity="0.00"/><stop offset="25%" stop-color="#38d8ff" stop-opacity="0.18"/><stop offset="55%" stop-color="#7868ff" stop-opacity="0.14"/><stop offset="82%" stop-color="#30f0d0" stop-opacity="0.16"/><stop offset="100%" stop-color="#30f0d0" stop-opacity="0.00"/>',
                '</linearGradient>',
                '<linearGradient id="bg-dark-thread-b" x1="0" y1="0" x2="1" y2="0">',
                '<stop offset="0%" stop-color="#9040ff" stop-opacity="0.00"/><stop offset="30%" stop-color="#9040ff" stop-opacity="0.10"/><stop offset="65%" stop-color="#20c8f0" stop-opacity="0.12"/><stop offset="100%" stop-color="#20c8f0" stop-opacity="0.00"/>',
                '</linearGradient>',
                '<pattern id="bg-dark-thread-grid" x="0" y="0" width="110" height="110" patternUnits="userSpaceOnUse" patternTransform="rotate(20)">',
                '<line x1="0" y1="0" x2="110" y2="0" stroke="url(#bg-dark-thread-a)" stroke-width="0.6" opacity="0.70"/>',
                '<line x1="0" y1="55" x2="110" y2="55" stroke="url(#bg-dark-thread-b)" stroke-width="0.4" opacity="0.50"/>',
                '<line x1="0" y1="27" x2="110" y2="27" stroke="url(#bg-dark-thread-a)" stroke-width="0.25" opacity="0.30"/>',
                '<line x1="0" y1="82" x2="110" y2="82" stroke="url(#bg-dark-thread-b)" stroke-width="0.25" opacity="0.24"/>',
                '</pattern>',
                '<pattern id="bg-dark-thread-grid-b" x="0" y="0" width="130" height="130" patternUnits="userSpaceOnUse" patternTransform="rotate(108)">',
                '<line x1="0" y1="0" x2="130" y2="0" stroke="url(#bg-dark-thread-b)" stroke-width="0.5" opacity="0.45"/>',
                '<line x1="0" y1="65" x2="130" y2="65" stroke="url(#bg-dark-thread-a)" stroke-width="0.3" opacity="0.28"/>',
                '</pattern>',
                '<filter id="bg-dark-swirl" x="-30%" y="-30%" width="160%" height="160%">',
                '<feTurbulence type="fractalNoise" baseFrequency="0.0030 0.0095" numOctaves="2" seed="7" result="noise">',
                '<animate attributeName="baseFrequency" values="0.0026 0.0086;0.0036 0.0106;0.0030 0.0095;0.0033 0.0100;0.0026 0.0086" dur="36s" repeatCount="indefinite"/>',
                '</feTurbulence>',
                '<feColorMatrix in="noise" type="matrix" result="noiseBoost" values="1.4 0 0 0 0 0 1.2 0 0 0 0 0 1.0 0 0 0 0 0 1 0"/>',
                '<feDisplacementMap in="SourceGraphic" in2="noiseBoost" scale="62" xChannelSelector="R" yChannelSelector="G" result="displaced">',
                '<animate attributeName="scale" values="40;76;52;70;40" dur="52s" repeatCount="indefinite"/>',
                '</feDisplacementMap>',
                '<feComposite in="displaced" in2="displaced" operator="arithmetic" k1="0" k2="1.08" k3="0" k4="-0.04" result="contrastEdge"/>',
                '<feGaussianBlur in="contrastEdge" stdDeviation="0.8" result="softened"/>',
                '<feBlend in="contrastEdge" in2="softened" mode="screen"/>',
                '</filter>',
                '<filter id="bg-dark-deep-blur" x="0%" y="0%" width="100%" height="100%"><feGaussianBlur stdDeviation="32"/></filter>',
                '<filter id="bg-dark-micro" x="-30%" y="-30%" width="160%" height="160%">',
                '<feTurbulence type="turbulence" baseFrequency="0.010" numOctaves="1" seed="13" result="microNoise">',
                '<animate attributeName="baseFrequency" values="0.009;0.012;0.010;0.011;0.009" dur="28s" repeatCount="indefinite"/>',
                '</feTurbulence>',
                '<feDisplacementMap in="SourceGraphic" in2="microNoise" scale="16" xChannelSelector="B" yChannelSelector="R"/>',
                '</filter>',
                '</defs>',
                '<rect x="-180" y="-180" width="1360" height="1360" fill="url(#bg-dark-mid)" opacity="0.55" filter="url(#bg-dark-deep-blur)"/>',
                '<g filter="url(#bg-dark-swirl)">',
                '<rect x="-180" y="-180" width="1360" height="1360" fill="url(#bg-dark-base)"/>',
                '<rect x="-200" y="-200" width="1400" height="1400" fill="url(#bg-dark-sheen-cool)" style="mix-blend-mode:screen"/>',
                '<rect x="-200" y="-200" width="1400" height="1400" fill="url(#bg-dark-sheen-accent)" style="mix-blend-mode:screen"/>',
                '</g>',
                '<rect x="-220" y="-220" width="1440" height="1440" fill="url(#bg-dark-base)" opacity="0.18" filter="url(#bg-dark-micro)"/>',
                '<rect x="0" y="0" width="1000" height="1000" fill="url(#bg-dark-thread-grid)" style="mix-blend-mode:screen" opacity="0.50"/>',
                '<rect x="0" y="0" width="1000" height="1000" fill="url(#bg-dark-thread-grid-b)" style="mix-blend-mode:screen" opacity="0.32"/>',
                '</svg>'
            ].join("");
        }

        return [
            '<svg class="ui5BgScene ui5BgScene--light" viewBox="0 0 1000 1000" preserveAspectRatio="none" aria-hidden="true" xmlns="http://www.w3.org/2000/svg">',
            '<defs>',
            '<linearGradient id="bg-light-base" x1="0" y1="0" x2="1" y2="1">',
            '<stop offset="0%" stop-color="#eff9ff"/><stop offset="24%" stop-color="#fdf3bf"/><stop offset="56%" stop-color="#f6d5ff"/><stop offset="100%" stop-color="#d9f6ef"/>',
            '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="360 0.5 0.5" dur="220s" repeatCount="indefinite"/>',
            '</linearGradient>',
            '<linearGradient id="bg-light-sheen-a" x1="0" y1="1" x2="1" y2="0">',
            '<stop offset="0%" stop-color="#57eeff" stop-opacity="0.26"/><stop offset="48%" stop-color="#9b83ff" stop-opacity="0.18"/><stop offset="100%" stop-color="#57ffbf" stop-opacity="0.20"/>',
            '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="-360 0.5 0.5" dur="280s" repeatCount="indefinite"/>',
            '</linearGradient>',
            '<linearGradient id="bg-light-sheen-b" x1="1" y1="0" x2="0" y2="1">',
            '<stop offset="0%" stop-color="#ffd668" stop-opacity="0.18"/><stop offset="50%" stop-color="#ff7ecb" stop-opacity="0.14"/><stop offset="100%" stop-color="#67f0ff" stop-opacity="0.16"/>',
            '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="360 0.5 0.5" dur="340s" repeatCount="indefinite"/>',
            '</linearGradient>',
            '<filter id="bg-light-swirl" x="-30%" y="-30%" width="160%" height="160%">',
            '<feTurbulence type="fractalNoise" baseFrequency="0.0058 0.0092" numOctaves="2" seed="9" result="noise">',
            '<animate attributeName="baseFrequency" values="0.0052 0.0084;0.0068 0.0100;0.0060 0.0090;0.0052 0.0084" dur="24s" repeatCount="indefinite"/>',
            '</feTurbulence>',
            '<feDisplacementMap in="SourceGraphic" in2="noise" scale="70" xChannelSelector="R" yChannelSelector="G"/>',
            '</filter>',
            '<filter id="bg-light-blur" x="0%" y="0%" width="100%" height="100%"><feGaussianBlur stdDeviation="22"/></filter>',
            '</defs>',
            '<rect x="-180" y="-180" width="1360" height="1360" fill="url(#bg-light-base)"/>',
            '<g filter="url(#bg-light-swirl)">',
            '<rect x="-180" y="-180" width="1360" height="1360" fill="url(#bg-light-base)"/>',
            '<ellipse cx="190" cy="170" rx="340" ry="230" fill="url(#bg-light-sheen-a)" style="mix-blend-mode:screen"/>',
            '<ellipse cx="820" cy="220" rx="310" ry="220" fill="url(#bg-light-sheen-b)" style="mix-blend-mode:screen"/>',
            '<ellipse cx="280" cy="820" rx="360" ry="250" fill="url(#bg-light-sheen-a)" opacity="0.82" style="mix-blend-mode:screen"/>',
            '</g>',
            '<ellipse cx="530" cy="620" rx="420" ry="320" fill="url(#bg-light-sheen-b)" opacity="0.18" filter="url(#bg-light-blur)"/>',
            '</svg>'
        ].join("");
    }

    function createHost() {
        var oHost = document.createElement("div");
        oHost.id = HOST_ID;
        oHost.className = "ui5BgHost";
        oHost.setAttribute("aria-hidden", "true");
        oHost.innerHTML = [
            '<div class="ui5BgDither"></div>',
            '<div class="ui5BgGrain"></div>',
            buildSvgMarkup("light"),
            buildSvgMarkup("dark")
        ].join("");
        return oHost;
    }

    function ensureHost() {
        var oHost = document.getElementById(HOST_ID);
        if (oHost) {
            return oHost;
        }
        oHost = createHost();
        document.body.insertBefore(oHost, document.body.firstChild || null);
        return oHost;
    }

    function isAnimationEnabled() {
        var oBody = document.body;
        if (!oBody) {
            return false;
        }
        if (window.matchMedia && window.matchMedia(REDUCED_MOTION_QUERY).matches) {
            return false;
        }
        if (oBody.getAttribute(BODY_ATTR_BG_ENABLED) === "false") {
            return false;
        }
        return !oBody.classList.contains("theme-motion-disabled");
    }

    function applyState() {
        var oBody = document.body;
        var oHost = ensureHost();
        var bEnabled;
        var bDark;
        if (!oBody || !oHost) {
            return;
        }
        bEnabled = isAnimationEnabled();
        bDark = String(oBody.getAttribute(BODY_ATTR_THEME) || "").trim().toLowerCase() === "dark";

        oHost.classList.toggle("is-disabled", !bEnabled);
        oHost.classList.toggle("is-dark", bDark);
        oHost.classList.toggle("is-light", !bDark);
    }

    function bindObservers() {
        var oMotionQuery;
        if (window.MutationObserver) {
            new MutationObserver(function () {
                applyState();
            }).observe(document.body, {
                attributes: true,
                attributeFilter: [BODY_ATTR_THEME, BODY_ATTR_BG_ENABLED, "class"]
            });
        }
        if (window.matchMedia) {
            oMotionQuery = window.matchMedia(REDUCED_MOTION_QUERY);
            if (typeof oMotionQuery.addEventListener === "function") {
                oMotionQuery.addEventListener("change", applyState);
            } else if (typeof oMotionQuery.addListener === "function") {
                oMotionQuery.addListener(applyState);
            }
        }
        window.addEventListener("resize", applyState, { passive: true });
    }

    function bootstrap() {
        if (!document.body || document.querySelector(SELECTOR)) {
            applyState();
            return;
        }
        ensureHost();
        applyState();
        bindObservers();
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", bootstrap, { once: true });
    } else {
        bootstrap();
    }
}());
