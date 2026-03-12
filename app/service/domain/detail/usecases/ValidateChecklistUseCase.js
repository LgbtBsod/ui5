sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/util/ValidationPathMap",
    "PRODUCTION_CONTROL_CHECKLIST/util/ChecklistValidationService",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (StatePaths, UseCase, Result, Effects, DetailStateAccess, ValidationPathMap, ChecklistValidationService, ViewPathContracts) {
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
                Effects.modelPatch("view", ViewPathContracts.VALIDATION_SHOWN, false),
                Effects.modelPatch("view", ViewPathContracts.VALIDATION_MISSING, {}),
                Effects.modelPatch("state", StatePaths.VALIDATION_SUMMARY, {
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
            Effects.modelPatch("view", ViewPathContracts.VALIDATION_SHOWN, true),
            Effects.modelPatch("view", ViewPathContracts.VALIDATION_MISSING, mMissing),
            Effects.modelPatch("state", StatePaths.VALIDATION_SUMMARY, {
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
