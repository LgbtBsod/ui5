sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (ThemeContracts, ThemeService, Ui5RuntimeFacade) {
    "use strict";

    /* Этот блок задает единственный допустимый визуальный режим приложения.
     * Результат: shell, контроллеры и тесты всегда получают стабильный светлый профиль. */
    var DEFAULT_MODE = ThemeContracts.MODES.MORNING;
    var DEFAULT_ANIMATION_ENABLED = ThemeService.DEFAULT_ANIMATION_ENABLED;

    /* Этот блок читает текущий профиль темы из сервиса и нормализует значения.
     * Результат: наружу всегда возвращается валидная структура mode + animationEnabled. */
    function readThemeProfile() {
        var oProfile = ThemeService.getThemeProfile ? ThemeService.getThemeProfile() : null;
        return {
            mode: DEFAULT_MODE,
            animationEnabled: oProfile && typeof oProfile.animationEnabled === "boolean"
                ? oProfile.animationEnabled
                : DEFAULT_ANIMATION_ENABLED
        };
    }

    /* Этот блок один раз подписывает контроллер на синхронизацию UI5-токенов темы.
     * Результат: после смены темы CSS-переменные приложения остаются согласованными. */
    function ensureThemeSyncListener(oController) {
        if (oController._fnThemeChangedHandler) {
            return;
        }
        oController._fnThemeChangedHandler = function () {
            ThemeService.syncTokensFromUI5();
        }.bind(oController);
        Ui5RuntimeFacade.attachThemeChanged(oController._fnThemeChangedHandler);
    }

    /* Этот блок применяет канонический утренний режим и при необходимости сохраняет профиль.
     * Результат: приложение всегда рендерится в поддерживаемом productive-виде. */
    function applyMorningMode(oController, oClickXY, mOptions) {
        var oProfile = readThemeProfile();
        ensureThemeSyncListener(oController);
        return ThemeService.applyThemeMode(DEFAULT_MODE, oClickXY || null, {
            animationEnabled: Object.prototype.hasOwnProperty.call(mOptions || {}, "animationEnabled")
                ? !!mOptions.animationEnabled
                : oProfile.animationEnabled,
            persist: !mOptions || mOptions.persist !== false
        });
    }

    /* Этот блок сохраняет публичный контракт setThemeMode, но не разрешает уходить в неподдерживаемый режим.
     * Результат: внешние вызовы не ломаются, а фактическая тема остается канонической. */
    function applySupportedMode(oController, _sMode, oClickXY, mOptions) {
        return applyMorningMode(oController, oClickXY, mOptions);
    }

    return {
        getCurrentTheme: function () {
            return ThemeService.themeForMode(DEFAULT_MODE);
        },
        getCurrentThemeMode: function () {
            return DEFAULT_MODE;
        },
        isThemeAnimationEnabled: function () {
            return !!readThemeProfile().animationEnabled;
        },
        setThemeAnimationEnabled: function (bEnabled) {
            if (ThemeService.setThemeAnimationEnabled) {
                ThemeService.setThemeAnimationEnabled(!!bEnabled);
            }
            return applyMorningMode(this, null, {
                animationEnabled: !!bEnabled,
                persist: false
            });
        },
        setThemeMode: function (sMode, oClickXY) {
            return applySupportedMode(this, sMode, oClickXY);
        },
        applyStoredTheme: function () {
            return applyMorningMode(this, null);
        },
        toggleTheme: function (oClickXY) {
            /* Этот блок повторно применяет единственный поддерживаемый профиль без смены каноники.
             * Результат: shell actions и автотесты продолжают работать по прежнему контракту. */
            return applySupportedMode(this, DEFAULT_MODE, oClickXY);
        },
        _ensureThemeSyncListener: function () {
            ensureThemeSyncListener(this);
        },
        _applyTheme: function (sTheme) {
            var oProfile = readThemeProfile();
            ensureThemeSyncListener(this);
            return ThemeService.applyTheme(sTheme, null, {
                animationEnabled: oProfile.animationEnabled
            });
        }
    };
});
