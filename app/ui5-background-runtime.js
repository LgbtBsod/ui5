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
                '<stop offset="0%" stop-color="#07111f"/><stop offset="32%" stop-color="#0a2134"/><stop offset="68%" stop-color="#16193b"/><stop offset="100%" stop-color="#091225"/>',
                '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="360 0.5 0.5" dur="240s" repeatCount="indefinite"/>',
                '</linearGradient>',
                '<linearGradient id="bg-dark-sheen-a" x1="0" y1="1" x2="1" y2="0">',
                '<stop offset="0%" stop-color="#2bd6f6" stop-opacity="0.20"/><stop offset="50%" stop-color="#7b6cff" stop-opacity="0.14"/><stop offset="100%" stop-color="#ff5ca8" stop-opacity="0.18"/>',
                '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="-360 0.5 0.5" dur="310s" repeatCount="indefinite"/>',
                '</linearGradient>',
                '<linearGradient id="bg-dark-sheen-b" x1="1" y1="0" x2="0" y2="1">',
                '<stop offset="0%" stop-color="#ffd35c" stop-opacity="0.12"/><stop offset="46%" stop-color="#ff5ca8" stop-opacity="0.10"/><stop offset="100%" stop-color="#3dffc9" stop-opacity="0.12"/>',
                '<animateTransform attributeName="gradientTransform" type="rotate" from="0 0.5 0.5" to="360 0.5 0.5" dur="360s" repeatCount="indefinite"/>',
                '</linearGradient>',
                '<filter id="bg-dark-swirl" x="-30%" y="-30%" width="160%" height="160%">',
                '<feTurbulence type="fractalNoise" baseFrequency="0.0062 0.0094" numOctaves="2" seed="11" result="noise">',
                '<animate attributeName="baseFrequency" values="0.0058 0.0088;0.0068 0.0102;0.0062 0.0094;0.0058 0.0088" dur="28s" repeatCount="indefinite"/>',
                '</feTurbulence>',
                '<feDisplacementMap in="SourceGraphic" in2="noise" scale="76" xChannelSelector="R" yChannelSelector="G"/>',
                '</filter>',
                '<filter id="bg-dark-blur" x="0%" y="0%" width="100%" height="100%"><feGaussianBlur stdDeviation="28"/></filter>',
                '</defs>',
                '<rect x="-160" y="-160" width="1320" height="1320" fill="url(#bg-dark-base)" filter="url(#bg-dark-blur)" opacity="0.92"/>',
                '<g filter="url(#bg-dark-swirl)">',
                '<rect x="-180" y="-180" width="1360" height="1360" fill="url(#bg-dark-base)"/>',
                '<ellipse cx="220" cy="180" rx="360" ry="240" fill="url(#bg-dark-sheen-a)" style="mix-blend-mode:screen"/>',
                '<ellipse cx="780" cy="260" rx="320" ry="220" fill="url(#bg-dark-sheen-b)" style="mix-blend-mode:screen"/>',
                '<ellipse cx="420" cy="820" rx="430" ry="280" fill="url(#bg-dark-sheen-a)" opacity="0.72" style="mix-blend-mode:screen"/>',
                '</g>',
                '<ellipse cx="500" cy="520" rx="420" ry="320" fill="url(#bg-dark-sheen-b)" opacity="0.12" filter="url(#bg-dark-blur)"/>',
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
