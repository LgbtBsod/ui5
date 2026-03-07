sap.ui.define([], function () {
    "use strict";

    var RIPPLE_CLASS = "is-rippling";
    var RIPPLE_DURATION_MS = 620;

    function prefersReducedMotion() {
        return !!(window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches);
    }

    function updatePointerVars(oElement, oEvent) {
        var oRect;
        if (!oElement || !oEvent) {
            return;
        }
        oRect = oElement.getBoundingClientRect();
        oElement.style.setProperty("--mx", (oEvent.clientX - oRect.left) + "px");
        oElement.style.setProperty("--my", (oEvent.clientY - oRect.top) + "px");
    }

    function attach(oRootDomRef) {
        var oRoot = oRootDomRef;
        var iRippleTimer = 0;

        function resolveTarget(oEvent) {
            var oTarget = oEvent && oEvent.target;
            return oTarget && oTarget.closest ? oTarget.closest(".chkRipple") : null;
        }

        function onPointerMove(oEvent) {
            var oTarget = resolveTarget(oEvent);
            if (oTarget) {
                updatePointerVars(oTarget, oEvent);
            }
            if (oRoot) {
                oRoot.style.setProperty("--app-mx", oEvent.clientX + "px");
                oRoot.style.setProperty("--app-my", oEvent.clientY + "px");
            }
        }

        function onPointerDown(oEvent) {
            var oTarget = resolveTarget(oEvent);
            if (!oTarget) {
                return;
            }
            updatePointerVars(oTarget, oEvent);
            if (prefersReducedMotion()) {
                return;
            }
            if (iRippleTimer) {
                window.clearTimeout(iRippleTimer);
            }
            oTarget.classList.remove(RIPPLE_CLASS);
            void oTarget.offsetWidth;
            oTarget.classList.add(RIPPLE_CLASS);
            iRippleTimer = window.setTimeout(function () {
                oTarget.classList.remove(RIPPLE_CLASS);
                iRippleTimer = 0;
            }, RIPPLE_DURATION_MS);
        }

        if (!oRoot || !oRoot.addEventListener) {
            return {
                destroy: function () {}
            };
        }

        oRoot.addEventListener("pointermove", onPointerMove, { passive: true });
        oRoot.addEventListener("pointerdown", onPointerDown, { passive: true });

        return {
            destroy: function () {
                if (iRippleTimer) {
                    window.clearTimeout(iRippleTimer);
                    iRippleTimer = 0;
                }
                oRoot.removeEventListener("pointermove", onPointerMove, { passive: true });
                oRoot.removeEventListener("pointerdown", onPointerDown, { passive: true });
            }
        };
    }

    return {
        attach: attach,
        prefersReducedMotion: prefersReducedMotion
    };
});
