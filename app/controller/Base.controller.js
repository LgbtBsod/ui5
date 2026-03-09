sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/RouterMixin",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ModelAccessMixin",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/EffectMixin",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ThemeMixin"
], function (Controller, RouterMixin, ModelAccessMixin, EffectMixin, ThemeMixin) {
    "use strict";

    return Controller.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Base", Object.assign({}, RouterMixin, ModelAccessMixin, EffectMixin, ThemeMixin));
});
