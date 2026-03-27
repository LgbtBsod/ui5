sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/ValidationPathMap",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/ChecklistValidationService",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (ModelContracts, StatePaths, Result, Effects, DetailStateAccess, ValidationPathMap, ChecklistValidationService, ViewPathContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;

    function ValidateChecklistUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

function execute(_mInput, mCtx) {
        var oChecklist = DetailStateAccess.readCurrentChecklist(mCtx);
        var aRequiredFields = DetailStateAccess.readRequiredFields(mCtx);
        var oValidation = ChecklistValidationService.validateRequiredFields(oChecklist, {
            requiredFields: aRequiredFields
        });
        if (oValidation.unavailable) {
            return Promise.resolve(Result.fail({
                code: "REQUIRED_FIELDS_UNAVAILABLE"
            }, [
                Effects.modelPatch(VIEW_MODEL, ViewPathContracts.VALIDATION_SHOWN, false),
                Effects.modelPatch(VIEW_MODEL, ViewPathContracts.VALIDATION_MISSING, {}),
                Effects.modelPatch(STATE_MODEL, StatePaths.VALIDATION_SUMMARY, {
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
            Effects.modelPatch(VIEW_MODEL, ViewPathContracts.VALIDATION_SHOWN, true),
            Effects.modelPatch(VIEW_MODEL, ViewPathContracts.VALIDATION_MISSING, mMissing),
            Effects.modelPatch(STATE_MODEL, StatePaths.VALIDATION_SUMMARY, {
                hasErrors: !oValidation.valid,
                missingPaths: oValidation.missingPaths || [],
                missingKeys: aMissingKeys,
                source: "validate",
                firstMissingPath: (oValidation.missingPaths || [])[0] || "",
                firstMissingKey: aMissingKeys[0] || ""
            }),
            Effects.toast(oValidation.valid ? "checklistValidationPassedToast" : "checklistValidationFailedToast", oValidation.valid ? "success" : "warning")
        ]));
    }

    return ValidateChecklistUseCase;
});
