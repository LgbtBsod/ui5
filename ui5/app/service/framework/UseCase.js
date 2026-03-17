sap.ui.define([], function () {
    "use strict";

    function UseCase(sName) {
        this.name = sName || "UseCase";
    }

    UseCase.prototype.getName = function () {
        return this.name;
    };

    UseCase.prototype.execute = function () {
        throw new Error("UseCase.execute(input, ctx) must be implemented by subclasses.");
    };

    return UseCase;
});
