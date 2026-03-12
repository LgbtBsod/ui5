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

    var SWITCH_CLASS = "appThemeSwitching";
    var MOTION_DISABLED_CLASS = "theme-motion-disabled";
    var MOTION_ENABLED_CLASS = "theme-motion-enabled";
    var THEME_PROFILE_STORAGE_KEY = "checklist_app_theme_profile";
    var LEGACY_THEME_PROFILE_STORAGE_KEY = "sap_ui5_theme_profile";
    var LEGACY_THEME_STORAGE_KEY = "sap_ui5_theme";
    var THEME_PROFILE_RECOVERY_KEY = "checklist_app_theme_profile_recovery_20260305";
    var DEFAULT_MODE = "morning";
    var DEFAULT_ANIMATION_ENABLED = true;
    var MODE_TO_THEME = {
        morning: "sap_horizon",
        night: "sap_horizon_dark"
    };
    var SWITCH_DURATION_MS = 220;
    var iSwitchTimer = 0;
    var fnThemeChangedCleanup = null;
    var sPendingMode = "";

    function normalizeMode(sModeOrTheme) {
        var sValue = String(sModeOrTheme || "").trim().toLowerCase();
        if (!sValue) {
            return DEFAULT_MODE;
        }
        if (sValue === "night" || sValue === "dark" || sValue === "sap_horizon_dark" || sValue === "sap_fiori_3_dark") {
            return "night";
        }
        if (sValue === "morning" || sValue === "light" || sValue === "sap_horizon" || sValue === "sap_fiori_3") {
            return "morning";
        }
        return DEFAULT_MODE;
    }

    function normalizeAnimationEnabled(vEnabled) {
        return ValueTokenParser.parseBooleanToken(vEnabled, DEFAULT_ANIMATION_ENABLED);
    }

    function buildThemeProfile(sMode, bAnimationEnabled) {
        return {
            mode: normalizeMode(sMode),
            animationEnabled: normalizeAnimationEnabled(bAnimationEnabled)
        };
    }

    function recoverLegacyProfileDefaults(oProfile) {
        var oNormalized = buildThemeProfile(oProfile && oProfile.mode, oProfile && oProfile.animationEnabled);
        var sRecoveredFlag;
        try {
            sRecoveredFlag = window.localStorage.getItem(THEME_PROFILE_RECOVERY_KEY);
            if (!sRecoveredFlag) {
                setThemeProfile(oNormalized);
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
        return buildThemeProfile(sLegacyMode || DEFAULT_MODE, DEFAULT_ANIMATION_ENABLED);
    }

    function setThemeProfile(oProfile) {
        var oNormalized = buildThemeProfile(oProfile && oProfile.mode, oProfile && oProfile.animationEnabled);
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

    function syncBackgroundRuntime(oProfile) {
        var sMode = normalizeMode(oProfile && oProfile.mode);
        var sRuntimeTheme = sMode === "night" ? "dark" : "light";
        var bEnabled = normalizeAnimationEnabled(oProfile && oProfile.animationEnabled);

        ThemeDomRuntime.setBodyAttribute("data-theme", sRuntimeTheme);
        ThemeDomRuntime.setBodyAttribute("data-bg-enabled", bEnabled ? "true" : "false");
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
        var oBody = document && document.body;
        var oRoot = document && document.documentElement;
        var oMeta = ThemePhilosophy.getMeta(sTheme);
        var aBodyNodes;
        if (!oBody || !oRoot) {
            return;
        }
        syncDocumentRootClasses();
        aBodyNodes = [oBody];
        ThemeDomRuntime.addClass(aBodyNodes, "appLight");
        ThemeDomRuntime.removeClass(aBodyNodes, "appDark");
        ThemeDomRuntime.addClass(aBodyNodes, "lightMode");
        ThemeDomRuntime.addClass([oRoot], "light-mode");
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
            Object.prototype.hasOwnProperty.call(mOptions, "animationEnabled") ? mOptions.animationEnabled : oStoredProfile.animationEnabled
        );
        var sResolvedMode = normalizeMode(oRequestedProfile.mode);
        var sTheme = themeForMode(sResolvedMode);
        var sCurrentTheme = Core && typeof Core.getConfiguration === "function" && Core.getConfiguration() && Core.getConfiguration().getTheme
            ? Core.getConfiguration().getTheme()
            : "";
        var sCurrentMode = modeForTheme(sCurrentTheme);
        var bAlreadyApplied = (sPendingMode || sCurrentMode) === sResolvedMode && sCurrentTheme === sTheme;

        if (mOptions.persist !== false) {
            setThemeProfile(oRequestedProfile);
        }
        setFloodOrigin(oClickXY);
        syncAnimationClass(oRequestedProfile.animationEnabled);
        syncBackgroundRuntime(oRequestedProfile);
        markSwitching(!bAlreadyApplied, oRequestedProfile.animationEnabled);
        applyBodyClasses(sTheme);
        if (bAlreadyApplied) {
            syncTokensFromUI5();
        }
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
            animationEnabled: oRequestedProfile.animationEnabled
        };
    }

    function applyTheme(sTheme, oClickXY, mOptions) {
        return applyThemeMode(modeForTheme(sTheme), oClickXY, mOptions);
    }

    return {
        DEFAULT_MODE: DEFAULT_MODE,
        DEFAULT_ANIMATION_ENABLED: DEFAULT_ANIMATION_ENABLED,
        getThemeProfile: getThemeProfile,
        setThemeProfile: setThemeProfile,
        setThemeMode: setThemeMode,
        setThemeAnimationEnabled: setThemeAnimationEnabled,
        normalizeAnimationEnabled: normalizeAnimationEnabled,
        applyTheme: applyTheme,
        applyThemeMode: applyThemeMode,
        modeForTheme: modeForTheme,
        themeForMode: themeForMode,
        syncDocumentRootClasses: syncDocumentRootClasses,
        syncAnimationClass: syncAnimationClass,
        syncTokensFromUI5: syncTokensFromUI5
    };
});
