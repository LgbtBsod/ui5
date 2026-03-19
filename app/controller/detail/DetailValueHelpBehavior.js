sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPersonInputRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailValueHelpRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (DetailCommandPolicy, DetailPersonInputRuntime, DetailValueHelpRuntime, ControllerViewStateRuntime, SchedulingRuntime) {
    "use strict";

    function autosaveHooks(oController) {
        return {
            autosave: function (mInput) {
                return DetailCommandPolicy.autosave(oController, {
                    rootId: oController._currentRootId(),
                    field: mInput.field,
                    value: mInput.value
                });
            }
        };
    }

    function numberValueHelpHooks(oController) {
        var mAutosaveHooks = autosaveHooks(oController);
        return {
            autosave: mAutosaveHooks.autosave,
            getLazyDialog: function (sKey) {
                return oController._mLazyDialogs && oController._mLazyDialogs[sKey];
            },
            rememberDialogReturnFocus: function (sDialogKey, oSource) {
                oController._rememberDialogReturnFocus(sDialogKey, oSource);
            },
            setLazyDialog: function (sKey, oDialog) {
                oController._mLazyDialogs = oController._mLazyDialogs || {};
                oController._mLazyDialogs[sKey] = oDialog;
            }
        };
    }

    function locationValueHelpHooks(oController) {
        return {
            clearSearchTimer: function () {
                oController._clearLocationValueHelpSearchTimer();
            },
            rememberDialogReturnFocus: function (sDialogKey, oSource) {
                oController._rememberDialogReturnFocus(sDialogKey, oSource);
            },
            restartSearchTimer: function (fnTask, iDelayMs) {
                oController._iLocationVhSearchTimer = SchedulingRuntime.restartTimer(0, function () {
                    oController._iLocationVhSearchTimer = null;
                    fnTask();
                }, iDelayMs);
            },
            scheduleTableSync: function () {
                oController._scheduleLocationValueHelpTableSync();
            },
            setLazyDialog: function (sKey, oDialog) {
                oController._mLazyDialogs = oController._mLazyDialogs || {};
                oController._mLazyDialogs[sKey] = oDialog;
            },
            setViewFlag: function (sPath, bValue) {
                ControllerViewStateRuntime.setFlag(oController, sPath, bValue);
            },
            valueHelpLocation: function (mInput) {
                return DetailCommandPolicy.valueHelpLocation(oController, mInput);
            },
            withViewFlag: function (sPath, fnTask) {
                return oController._withViewFlag(sPath, fnTask);
            }
        };
    }

    function personSuggestHooks(oController) {
        return {
            consumeSuggestionSelection: function (sTarget, sValue) {
                return DetailPersonInputRuntime.consumeSuggestionSelection(oController, sTarget, sValue);
            },
            isEditMode: function () {
                return oController._isEditMode();
            },
            personSuggest: function (mInput) {
                return DetailCommandPolicy.personSuggest(oController, mInput);
            },
            personTargetFromSource: function (oSource) {
                return DetailPersonInputRuntime.targetFromSource(oSource);
            },
            rememberSuggestionSelection: function (sTarget, sValue) {
                DetailPersonInputRuntime.rememberSuggestionSelection(oController, sTarget, sValue);
            }
        };
    }

    return {
        onBarriersNumberChange: function (oEvent) {
            return DetailValueHelpRuntime.onBarriersNumberChange(this, oEvent, numberValueHelpHooks(this));
        },
        onChecksNumberChange: function (oEvent) {
            return DetailValueHelpRuntime.onChecksNumberChange(this, oEvent, numberValueHelpHooks(this));
        },
        onCloseLocationValueHelp: function () {
            return DetailValueHelpRuntime.closeLocationValueHelp(this, locationValueHelpHooks(this));
        },
        onConfirmLocationValueHelp: function () {
            return DetailValueHelpRuntime.confirmLocationValueHelp(this, locationValueHelpHooks(this));
        },
        onLpcChange: function (oEvent) {
            return DetailValueHelpRuntime.onLpcChange(this, oEvent, numberValueHelpHooks(this));
        },
        onLocationTreeSelectionChange: function (oEvent) {
            return DetailValueHelpRuntime.onLocationTreeSelectionChange(this, oEvent, locationValueHelpHooks(this));
        },
        onLocationValueHelpSearch: function (oEvent) {
            return DetailValueHelpRuntime.onLocationValueHelpSearch(this, oEvent, locationValueHelpHooks(this));
        },
        onLocationValueHelpSearchSubmit: function (oEvent) {
            return DetailValueHelpRuntime.onLocationValueHelpSearchSubmit(this, oEvent, locationValueHelpHooks(this));
        },
        onOpenBarriersNumberValueHelp: function (oEvent) {
            return DetailValueHelpRuntime.onOpenBarriersNumberValueHelp(this, oEvent, numberValueHelpHooks(this));
        },
        onOpenChecksNumberValueHelp: function (oEvent) {
            return DetailValueHelpRuntime.onOpenChecksNumberValueHelp(this, oEvent, numberValueHelpHooks(this));
        },
        onOpenLocationValueHelp: function (oEvent) {
            return DetailValueHelpRuntime.onOpenLocationValueHelp(this, oEvent, locationValueHelpHooks(this));
        },
        onPersonInputChange: function (oEvent) {
            return DetailValueHelpRuntime.onPersonInputChange(this, oEvent, personSuggestHooks(this));
        },
        onPersonSuggest: function (oEvent) {
            return DetailValueHelpRuntime.onPersonSuggest(this, oEvent, personSuggestHooks(this));
        },
        onPersonSuggestionSelected: function (oEvent) {
            return DetailValueHelpRuntime.onPersonSuggestionSelected(this, oEvent, personSuggestHooks(this));
        },
        onProfessionChange: function (oEvent) {
            return DetailValueHelpRuntime.onProfessionChange(this, oEvent, numberValueHelpHooks(this));
        }
    };
});
