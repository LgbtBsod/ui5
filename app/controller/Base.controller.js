sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "checklist/app/controller/base/RouterMixin",
    "checklist/app/controller/base/ModelAccessMixin",
    "checklist/app/controller/base/EffectMixin",
    "checklist/app/controller/base/ThemeMixin"
], function (Controller, RouterMixin, ModelAccessMixin, EffectMixin, ThemeMixin) {
    "use strict";

    return Controller.extend("checklist.app.controller.Base", Object.assign({}, RouterMixin, ModelAccessMixin, EffectMixin, ThemeMixin));
});
