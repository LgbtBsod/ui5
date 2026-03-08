sap.ui.define([
    "checklist/app/service/framework/SchedulingRuntime",
    "checklist/app/util/ThemeDomRuntime"
], function (SchedulingRuntime, ThemeDomRuntime) {
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
        ThemeDomRuntime.setStyleProperties([oElement], {
            "--mx": (oEvent.clientX - oRect.left) + "px",
            "--my": (oEvent.clientY - oRect.top) + "px"
        });
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
                ThemeDomRuntime.setStyleProperties([oRoot], {
                    "--app-mx": oEvent.clientX + "px",
                    "--app-my": oEvent.clientY + "px"
                });
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
            iRippleTimer = SchedulingRuntime.clearTimer(iRippleTimer);
            ThemeDomRuntime.removeClass([oTarget], RIPPLE_CLASS);
            void oTarget.offsetWidth;
            ThemeDomRuntime.addClass([oTarget], RIPPLE_CLASS);
            iRippleTimer = SchedulingRuntime.restartTimer(iRippleTimer, function () {
                ThemeDomRuntime.removeClass([oTarget], RIPPLE_CLASS);
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
                iRippleTimer = SchedulingRuntime.clearTimer(iRippleTimer);
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
