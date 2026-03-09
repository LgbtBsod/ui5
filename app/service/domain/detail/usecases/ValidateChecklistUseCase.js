sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/util/ValidationPathMap",
    "PRODUCTION_CONTROL_CHECKLIST/util/ChecklistValidationService"
], function (UseCase, Result, Effects, DetailStateAccess, ValidationPathMap, ChecklistValidationService) {
    "use strict";

    function ValidateChecklistUseCase() {
        UseCase.call(this, "ValidateChecklistUseCase");
    }

    ValidateChecklistUseCase.prototype = Object.create(UseCase.prototype);
    ValidateChecklistUseCase.prototype.constructor = ValidateChecklistUseCase;

    ValidateChecklistUseCase.prototype.execute = function (_mInput, mCtx) {
        var oChecklist = DetailStateAccess.readCurrentChecklist(mCtx);
        var aRequiredFields = DetailStateAccess.readRequiredFields(mCtx);
        var oValidation = ChecklistValidationService.validateRequiredFields(oChecklist, {
            requiredFields: aRequiredFields
        });
        if (oValidation.unavailable) {
            return Promise.resolve(Result.fail({
                message: "Validation rules are not loaded yet",
                code: "REQUIRED_FIELDS_UNAVAILABLE"
            }, [
                Effects.modelPatch("view", "/validationShown", false),
                Effects.modelPatch("view", "/validationMissing", {}),
                Effects.modelPatch("state", "/validationSummary", {
                    hasErrors: false,
                    missingPaths: [],
                    missingKeys: [],
                    source: "validate",
                    firstMissingPath: "",
                    firstMissingKey: ""
                }),
                Effects.toast("checklistValidationUnavailableToast", "warning")
            ]));
        }
        var mMissing = ValidationPathMap.toMissingMap(oValidation.missingPaths);
        var aMissingKeys = Object.keys(mMissing || {}).filter(function (sKey) { return !!mMissing[sKey]; });

        return Promise.resolve(Result.ok({
            valid: !!oValidation.valid,
            missingPaths: oValidation.missingPaths || []
        }, [
            Effects.modelPatch("view", "/validationShown", true),
            Effects.modelPatch("view", "/validationMissing", mMissing),
            Effects.modelPatch("state", "/validationSummary", {
                hasErrors: !oValidation.valid,
                missingPaths: oValidation.missingPaths || [],
                missingKeys: aMissingKeys,
                source: "validate",
                firstMissingPath: (oValidation.missingPaths || [])[0] || "",
                firstMissingKey: aMissingKeys[0] || ""
            }),
            Effects.toast(oValidation.valid ? "checklistValidationPassedToast" : "checklistValidationFailedToast", oValidation.valid ? "success" : "warning")
        ]));
    };

    return ValidateChecklistUseCase;
});
