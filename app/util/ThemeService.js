sap.ui.define([
    "sap/ui/core/Core",
    "sap/ui/core/theming/Parameters",
    "PRODUCTION_CONTROL_CHECKLIST/util/ThemePhilosophy",
    "PRODUCTION_CONTROL_CHECKLIST/util/ValueTokenParser",
    "PRODUCTION_CONTROL_CHECKLIST/util/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/ThemeTokenRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (Core, Parameters, ThemePhilosophy, ValueTokenParser, ThemeDomRuntime, ThemeTokenRuntime, SchedulingRuntime) {
    "use strict";

    var SWITCH_CLASS = "theme-switching";
    var MOTION_DISABLED_CLASS = "theme-motion-disabled";
    var MOTION_ENABLED_CLASS = "theme-motion-enabled";
    var THEME_PROFILE_STORAGE_KEY = "checklist_app_theme_profile";
    var LEGACY_THEME_PROFILE_STORAGE_KEY = "sap_ui5_theme_profile";
    var LEGACY_THEME_STORAGE_KEY = "sap_ui5_theme";
    var THEME_PROFILE_RECOVERY_KEY = "checklist_app_theme_profile_recovery_20260305";
    var DEFAULT_MODE = "morning";
    var DEFAULT_ANIMATION_ENABLED = true;
    var DEFAULT_BACKGROUND_INTERACTIVE = true;
    var MODE_TO_THEME = {
        morning: "sap_horizon",
        night: "sap_horizon_dark"
    };
    var NIGHT_MODE_ALIASES = {
        night: true,
        sap_fiori_3_dark: true,
        sap_horizon_dark: true
    };
    var MORNING_MODE_ALIASES = {
        morning: true,
        sap_fiori_3: true,
        sap_horizon: true
    };
    var SWITCH_DURATION_MS = 220;
    var iSwitchTimer = 0;
    var fnThemeChangedCleanup = null;
    var sPendingMode = "";

    function normalizeMode(sModeOrTheme) {
        var sValue = String(sModeOrTheme || "").toLowerCase();
        if (NIGHT_MODE_ALIASES[sValue]) {
            return "night";
        }
        if (MORNING_MODE_ALIASES[sValue]) {
            return "morning";
        }
        return DEFAULT_MODE;
    }

    function normalizeAnimationEnabled(vEnabled) {
        return ValueTokenParser.parseBooleanToken(vEnabled, DEFAULT_ANIMATION_ENABLED);
    }

    function normalizeBackgroundInteractive(vEnabled) {
        return ValueTokenParser.parseBooleanToken(vEnabled, DEFAULT_BACKGROUND_INTERACTIVE);
    }

    function buildThemeProfile(sMode, bAnimationEnabled, bBackgroundInteractive) {
        return {
            mode: normalizeMode(sMode),
            animationEnabled: normalizeAnimationEnabled(bAnimationEnabled),
            backgroundInteractive: normalizeBackgroundInteractive(bBackgroundInteractive)
        };
    }

    function recoverLegacyProfileDefaults(oProfile) {
        var oNormalized = buildThemeProfile(oProfile && oProfile.mode, oProfile && oProfile.animationEnabled, oProfile && oProfile.backgroundInteractive);
        var sRecoveredFlag;
        var bRecovered = false;
        try {
            sRecoveredFlag = window.localStorage.getItem(THEME_PROFILE_RECOVERY_KEY);
            if (!sRecoveredFlag) {
                if (oNormalized.animationEnabled !== true) {
                    oNormalized.animationEnabled = true;
                    bRecovered = true;
                }
                if (oNormalized.backgroundInteractive !== true) {
                    oNormalized.backgroundInteractive = true;
                    bRecovered = true;
                }
                if (bRecovered) {
                    setThemeProfile(oNormalized);
                }
                window.localStorage.setItem(THEME_PROFILE_RECOVERY_KEY, "done");
            }
        } catch (e) {
            // Best-effort recovery only.
        }
        return oNormalized;
    }

    function getThemeProfile() {
        var sRawProfile;
        var oParsedProfile;
        var sLegacyMode;
        try {
            sRawProfile = window.localStorage.getItem(THEME_PROFILE_STORAGE_KEY)
                || window.localStorage.getItem(LEGACY_THEME_PROFILE_STORAGE_KEY);
            if (sRawProfile) {
                oParsedProfile = JSON.parse(sRawProfile);
                return recoverLegacyProfileDefaults(oParsedProfile);
            }
        } catch (e) {
            // Fallback to legacy key and defaults.
        }
        try {
            sLegacyMode = window.localStorage.getItem(LEGACY_THEME_STORAGE_KEY);
        } catch (e2) {
            sLegacyMode = DEFAULT_MODE;
        }
        return buildThemeProfile(sLegacyMode || DEFAULT_MODE, DEFAULT_ANIMATION_ENABLED, DEFAULT_BACKGROUND_INTERACTIVE);
    }

    function setThemeProfile(oProfile) {
        var oNormalized = buildThemeProfile(oProfile && oProfile.mode, oProfile && oProfile.animationEnabled, oProfile && oProfile.backgroundInteractive);
        try {
            window.localStorage.setItem(THEME_PROFILE_STORAGE_KEY, JSON.stringify(oNormalized));
            window.localStorage.setItem("checklist_app_theme", oNormalized.mode);
        } catch (e) {
            // Best-effort persistence.
        }
        return oNormalized;
    }

    function setThemeMode(sMode) {
        var oProfile = getThemeProfile();
        oProfile.mode = normalizeMode(sMode);
        return setThemeProfile(oProfile);
    }

    function setThemeAnimationEnabled(bEnabled) {
        var oProfile = getThemeProfile();
        oProfile.animationEnabled = normalizeAnimationEnabled(bEnabled);
        return setThemeProfile(oProfile);
    }

    function setThemeBackgroundInteractive(bEnabled) {
        var oProfile = getThemeProfile();
        oProfile.backgroundInteractive = normalizeBackgroundInteractive(bEnabled);
        return setThemeProfile(oProfile);
    }

    function syncBackgroundRuntime(oProfile) {
        var oRuntime = window && window.Ui5Bg;
        var sRuntimeTheme = normalizeMode(oProfile && oProfile.mode) === "night" ? "dark" : "light";
        var bEnabled = normalizeAnimationEnabled(oProfile && oProfile.animationEnabled);
        var bInteractive = bEnabled && normalizeBackgroundInteractive(oProfile && oProfile.backgroundInteractive);

        ThemeDomRuntime.setBodyAttribute("data-theme", sRuntimeTheme);
        ThemeDomRuntime.setBodyAttribute("data-bg-enabled", bEnabled ? "true" : "false");
        ThemeDomRuntime.setBodyAttribute("data-bg-interactive", bInteractive ? "true" : "false");

        if (!oRuntime) {
            return;
        }

        if (typeof oRuntime.setTheme === "function") {
            oRuntime.setTheme(sRuntimeTheme);
        }
        if (typeof oRuntime.setEnabled === "function") {
            oRuntime.setEnabled(bEnabled);
        }
        if (typeof oRuntime.setInteractive === "function") {
            oRuntime.setInteractive(bInteractive);
        }
    }

    function themeForMode(sMode) {
        return MODE_TO_THEME[normalizeMode(sMode)] || MODE_TO_THEME[DEFAULT_MODE];
    }

    function modeForTheme(sTheme) {
        return normalizeMode(sTheme);
    }

    function syncDocumentRootClasses() {
        var oNodes = ThemeDomRuntime.getNodes();
        ThemeDomRuntime.addClass([oNodes.root, oNodes.body, oNodes.container], "chkAppRoot");
    }

    function syncAnimationClass(bAnimationEnabled) {
        var bEnabled = normalizeAnimationEnabled(bAnimationEnabled);
        var oNodes = ThemeDomRuntime.getNodes();
        var aNodes = [oNodes.root, oNodes.body, oNodes.container];
        ThemeDomRuntime.toggleClass(aNodes, MOTION_DISABLED_CLASS, !bEnabled);
        ThemeDomRuntime.toggleClass(aNodes, MOTION_ENABLED_CLASS, bEnabled);
    }

    function setFloodOrigin(oClickXY) {
        var oRoot = document && document.documentElement;
        var iX;
        var iY;
        if (!oRoot) {
            return;
        }
        iX = Number(oClickXY && oClickXY.x);
        iY = Number(oClickXY && oClickXY.y);
        if (!isFinite(iX) || !isFinite(iY)) {
            iX = window.innerWidth;
            iY = 0;
        }
        ThemeDomRuntime.setStyleProperties([oRoot], {
            "--theme-flood-x": iX + "px",
            "--theme-flood-y": iY + "px",
            "--theme-flood-origin": iX + "px " + iY + "px"
        });
    }

    function applyBodyClasses(sTheme) {
        var sMode = modeForTheme(sTheme);
        var bNight = sMode === "night";
        var oBody = document && document.body;
        var oRoot = document && document.documentElement;
        var oMeta = ThemePhilosophy.getMeta(sTheme);
        var aBodyNodes;
        if (!oBody || !oRoot) {
            return;
        }
        syncDocumentRootClasses();
        aBodyNodes = [oBody];
        ThemeDomRuntime.toggleClass(aBodyNodes, "appLight", !bNight);
        ThemeDomRuntime.toggleClass(aBodyNodes, "appDark", bNight);
        ThemeDomRuntime.toggleClass(aBodyNodes, "lightMode", !bNight);
        ThemeDomRuntime.toggleClass([oRoot], "light-mode", !bNight);
        [
            "themeLifestyleClarity",
            "themeLifestyleNightOps",
            "platformCupertinoGlass",
            "platformPrecisionEnterprise",
            "platformCalmModern",
            "themeHorizonMorning",
            "themeHorizonNight"
        ].forEach(function (sClassName) {
            ThemeDomRuntime.removeClass(aBodyNodes, sClassName);
        });
        if (oMeta.lifestyleClass) {
            ThemeDomRuntime.addClass(aBodyNodes, oMeta.lifestyleClass);
        }
        if (oMeta.platformClass) {
            ThemeDomRuntime.addClass(aBodyNodes, oMeta.platformClass);
        }
        if (oMeta.horizonClass) {
            ThemeDomRuntime.addClass(aBodyNodes, oMeta.horizonClass);
        }
    }

    function clearSwitching() {
        var oNodes = ThemeDomRuntime.getNodes();
        iSwitchTimer = SchedulingRuntime.clearTimer(iSwitchTimer);
        if (fnThemeChangedCleanup) {
            sap.ui.getCore().detachThemeChanged(fnThemeChangedCleanup);
            fnThemeChangedCleanup = null;
        }
        sPendingMode = "";
        ThemeDomRuntime.removeClass([oNodes.root, oNodes.body], SWITCH_CLASS);
    }

    function markSwitching(bAwaitThemeChanged, bAnimationEnabled) {
        var oNodes = ThemeDomRuntime.getNodes();
        var oRoot = oNodes.root;
        if (!normalizeAnimationEnabled(bAnimationEnabled)) {
            clearSwitching();
            return;
        }
        if (!oRoot) {
            return;
        }
        clearSwitching();
        ThemeDomRuntime.addClass([oNodes.root, oNodes.body], SWITCH_CLASS);
        if (bAwaitThemeChanged) {
            fnThemeChangedCleanup = function () {
                syncTokensFromUI5();
                SchedulingRuntime.nextFrame(function () {
                    clearSwitching();
                });
            };
            sap.ui.getCore().attachThemeChanged(fnThemeChangedCleanup);
        }
        iSwitchTimer = SchedulingRuntime.restartTimer(iSwitchTimer, function () {
            clearSwitching();
        }, bAwaitThemeChanged ? Math.max(SWITCH_DURATION_MS, 520) : SWITCH_DURATION_MS);
    }

    function syncTokensFromUI5() {
        return ThemeTokenRuntime.syncTokensFromUI5(Core, Parameters, syncDocumentRootClasses);
    }

    function applyThemeMode(sMode, oClickXY) {
        var mOptions = arguments.length > 2 && arguments[2] ? arguments[2] : {};
        var oStoredProfile = getThemeProfile();
        var oRequestedProfile = buildThemeProfile(
            sMode || oStoredProfile.mode,
            Object.prototype.hasOwnProperty.call(mOptions, "animationEnabled") ? mOptions.animationEnabled : oStoredProfile.animationEnabled,
            Object.prototype.hasOwnProperty.call(mOptions, "backgroundInteractive") ? mOptions.backgroundInteractive : oStoredProfile.backgroundInteractive
        );
        var sResolvedMode = oRequestedProfile.mode;
        var sTheme = themeForMode(sResolvedMode);
        var sCurrentTheme = Core && typeof Core.getConfiguration === "function" && Core.getConfiguration() && Core.getConfiguration().getTheme
            ? Core.getConfiguration().getTheme()
            : "";
        var sCurrentMode = modeForTheme(sCurrentTheme);
        var bAlreadyApplied = (sPendingMode || sCurrentMode) === sResolvedMode;

        if (mOptions.persist !== false) {
            setThemeProfile(oRequestedProfile);
        }
        setFloodOrigin(oClickXY);
        syncAnimationClass(oRequestedProfile.animationEnabled);
        syncBackgroundRuntime(oRequestedProfile);
        markSwitching(!bAlreadyApplied, oRequestedProfile.animationEnabled);
        applyBodyClasses(sTheme);
        syncTokensFromUI5();
        if (!bAlreadyApplied) {
            sPendingMode = sResolvedMode;
            SchedulingRuntime.nextFrame(function () {
                Core.applyTheme(sTheme);
            });
        }
        return {
            mode: sResolvedMode,
            theme: sTheme,
            isDark: sResolvedMode === "night",
            animationEnabled: oRequestedProfile.animationEnabled,
            backgroundInteractive: oRequestedProfile.backgroundInteractive
        };
    }

    function applyTheme(sTheme, oClickXY, mOptions) {
        return applyThemeMode(modeForTheme(sTheme), oClickXY, mOptions);
    }

    return {
        DEFAULT_MODE: DEFAULT_MODE,
        DEFAULT_ANIMATION_ENABLED: DEFAULT_ANIMATION_ENABLED,
        DEFAULT_BACKGROUND_INTERACTIVE: DEFAULT_BACKGROUND_INTERACTIVE,
        getThemeProfile: getThemeProfile,
        setThemeProfile: setThemeProfile,
        setThemeMode: setThemeMode,
        setThemeAnimationEnabled: setThemeAnimationEnabled,
        setThemeBackgroundInteractive: setThemeBackgroundInteractive,
        normalizeAnimationEnabled: normalizeAnimationEnabled,
        normalizeBackgroundInteractive: normalizeBackgroundInteractive,
        applyTheme: applyTheme,
        applyThemeMode: applyThemeMode,
        modeForTheme: modeForTheme,
        themeForMode: themeForMode,
        syncDocumentRootClasses: syncDocumentRootClasses,
        syncAnimationClass: syncAnimationClass,
        syncTokensFromUI5: syncTokensFromUI5
    };
});
