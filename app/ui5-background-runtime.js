(function () {
    "use strict";

    var HOST_ID = "ui5-bg-host";
    var BODY_ATTR_THEME = "data-theme";
    var BODY_ATTR_BG_ENABLED = "data-bg-enabled";

    function ensureHost() {
        return document.getElementById(HOST_ID);
    }

    function setThemeState(mState) {
        var oBody = document.body;
        var oHost = ensureHost();
        if (!oBody || !oHost) {
            return;
        }
        oBody.setAttribute(BODY_ATTR_THEME, "light");
        oBody.setAttribute(BODY_ATTR_BG_ENABLED, mState && mState.enabled === false ? "false" : "true");
        oHost.classList.toggle("is-light", true);
        oHost.classList.toggle("is-dark", false);
        oHost.classList.toggle("is-disabled", mState && mState.enabled === false);
    }

    function applyState() {
        var oBody = document.body;
        var oHost = ensureHost();
        if (!oBody || !oHost) {
            return;
        }
        oHost.classList.toggle("is-light", true);
        oHost.classList.toggle("is-dark", false);
        oHost.classList.toggle("is-disabled", oBody.getAttribute(BODY_ATTR_BG_ENABLED) === "false");
    }

    function bootstrap() {
        applyState();
    }

    window.Ui5Bg = window.Ui5Bg || {};
    window.Ui5Bg.onResizeEnd = applyState;
    window.Ui5Bg.onResizeStart = function () {};
    window.Ui5Bg.setThemeState = setThemeState;
    window.Ui5Bg.applyState = applyState;

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", bootstrap, { once: true });
    } else {
        bootstrap();
    }
}());
