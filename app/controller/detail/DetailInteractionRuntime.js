sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPersonInputRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentOpenRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInputAssistRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (
    AttachmentUploadCore,
    DetailCommandPolicy,
    DetailPersonInputRuntime,
    DetailAttachmentRuntime,
    DetailAttachmentOpenRuntime,
    DetailInputAssistRuntime,
    ControllerViewStateRuntime,
    NavigationIntentService,
    SchedulingRuntime
) {
    "use strict";

    function withCurrentRootId(oController, mInput) {
        var oNormalized = Object.assign({}, mInput || {});
        oNormalized.rootId = oNormalized.rootId || (oController && typeof oController._currentRootId === "function" ? oController._currentRootId() : "");
        return oNormalized;
    }

    function attachmentSectionHooks(oController) {
        return {
            attachmentDelete: function (mInput) {
                return DetailCommandPolicy.attachmentDelete(oController, withCurrentRootId(oController, mInput));
            },
            attachmentLoad: function () {
                return DetailCommandPolicy.attachmentLoad(oController, withCurrentRootId(oController));
            },
            scheduleAttachmentDropZoneBind: function () {
                if (typeof oController._scheduleAttachmentDropZoneBind === "function") {
                    oController._scheduleAttachmentDropZoneBind();
                }
            },
            unbindAttachmentDropZone: function () {
                if (typeof oController._unbindAttachmentDropZone === "function") {
                    oController._unbindAttachmentDropZone();
                }
            }
        };
    }

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
            attachmentDelete: function (oController, mInput) {
            return DetailCommandPolicy.attachmentDelete(oController, withCurrentRootId(oController, mInput));
        },
        attachmentLoad: function (oController, mInput) {
            return DetailCommandPolicy.attachmentLoad(oController, withCurrentRootId(oController, mInput));
        },
        openWorkflowAnalytics: function (oController) {
            NavigationIntentService.navigateToAnalytics(oController);
            return Promise.resolve();
        },
        toggleAttachmentsSection: function (oController, mInput) {
            return DetailAttachmentRuntime.toggleAttachmentsSection(oController, mInput || {});
        },
        onAttachmentUploadChange: function (oEvent) {
            return AttachmentUploadCore.onUploaderChange(this, oEvent);
        },
        onDeleteAttachment: function (oEvent) {
            return DetailAttachmentRuntime.deleteAttachment(this, oEvent, attachmentSectionHooks(this));
        },
        onOpenAttachment: function (oEvent) {
            return DetailAttachmentOpenRuntime.openAttachment(this, oEvent);
        },
        onOpenAttachmentPicker: function () {
            return AttachmentUploadCore.openNativeFilePicker(this);
        },
        onOpenWorkflowAnalytics: function () {
            NavigationIntentService.navigateToAnalytics(this);
            return Promise.resolve();
        },
        onToggleAttachmentsSection: function () {
            return DetailAttachmentRuntime.toggleAttachmentsSection(this, attachmentSectionHooks(this));
        },
        onBarriersNumberChange: function (oEvent) {
            return DetailInputAssistRuntime.onBarriersNumberChange(this, oEvent, numberValueHelpHooks(this));
        },
        onChecksNumberChange: function (oEvent) {
            return DetailInputAssistRuntime.onChecksNumberChange(this, oEvent, numberValueHelpHooks(this));
        },
        onCloseLocationValueHelp: function () {
            return DetailInputAssistRuntime.closeLocationValueHelp(this, locationValueHelpHooks(this));
        },
        onConfirmLocationValueHelp: function () {
            return DetailInputAssistRuntime.confirmLocationValueHelp(this, locationValueHelpHooks(this));
        },
        onLpcChange: function (oEvent) {
            return DetailInputAssistRuntime.onLpcChange(this, oEvent, numberValueHelpHooks(this));
        },
        onLocationTreeSelectionChange: function (oEvent) {
            return DetailInputAssistRuntime.onLocationTreeSelectionChange(this, oEvent, locationValueHelpHooks(this));
        },
        onLocationValueHelpSearch: function (oEvent) {
            return DetailInputAssistRuntime.onLocationValueHelpSearch(this, oEvent, locationValueHelpHooks(this));
        },
        onLocationValueHelpSearchSubmit: function (oEvent) {
            return DetailInputAssistRuntime.onLocationValueHelpSearchSubmit(this, oEvent, locationValueHelpHooks(this));
        },
        onOpenBarriersNumberValueHelp: function (oEvent) {
            return DetailInputAssistRuntime.onOpenBarriersNumberValueHelp(this, oEvent, numberValueHelpHooks(this));
        },
        onOpenChecksNumberValueHelp: function (oEvent) {
            return DetailInputAssistRuntime.onOpenChecksNumberValueHelp(this, oEvent, numberValueHelpHooks(this));
        },
        onOpenLocationValueHelp: function (oEvent) {
            return DetailInputAssistRuntime.onOpenLocationValueHelp(this, oEvent, locationValueHelpHooks(this));
        },
        onPersonInputChange: function (oEvent) {
            return DetailInputAssistRuntime.onPersonInputChange(this, oEvent, personSuggestHooks(this));
        },
        onPersonSuggest: function (oEvent) {
            return DetailInputAssistRuntime.onPersonSuggest(this, oEvent, personSuggestHooks(this));
        },
        onPersonSuggestionSelected: function (oEvent) {
            return DetailInputAssistRuntime.onPersonSuggestionSelected(this, oEvent, personSuggestHooks(this));
        },
        onProfessionChange: function (oEvent) {
            return DetailInputAssistRuntime.onProfessionChange(this, oEvent, numberValueHelpHooks(this));
        }
    };
});
