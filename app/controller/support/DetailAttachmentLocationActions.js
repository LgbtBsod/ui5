sap.ui.define([
    "checklist/app/controller/support/AttachmentUploadSupport",
    "checklist/app/controller/support/DetailCommandPolicy",
    "checklist/app/controller/support/DetailPersonInputSupport",
    "checklist/app/service/framework/AttachmentFlowService",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/SchedulingRuntime"
], function (AttachmentUploadSupport, DetailCommandPolicy, DetailPersonInputSupport, AttachmentFlowService, NavigationIntentService, SchedulingRuntime) {
    "use strict";

    return {
        onAttachmentUploadChange: function (oEvent) {
            return AttachmentFlowService.onUploaderChange(this, oEvent, {
                onUploaderChange: AttachmentUploadSupport.onUploaderChange
            });
        },

        onDeleteAttachment: function (oEvent) {
            return AttachmentFlowService.deleteAttachment(this, oEvent);
        },

        onToggleAttachmentsSection: function () {
            return AttachmentFlowService.toggleHistory(this, {
                bindDropZone: function (oController) {
                    if (oController && typeof oController._scheduleAttachmentDropZoneBind === "function") {
                        oController._scheduleAttachmentDropZoneBind();
                    }
                },
                unbindDropZone: AttachmentUploadSupport.unbindDropZone
            });
        },
        onOpenWorkflowAnalytics: function () {
            NavigationIntentService.navigateToAnalytics(this);
            return Promise.resolve();
        },

        onOpenAttachment: function (oEvent) {
            return AttachmentFlowService.openAttachment(this, oEvent);
        },

        onOpenLocationValueHelp: function (oEvent) {
            this._clearLocationValueHelpSearchTimer();
            this._rememberDialogReturnFocus("locationValueHelp", oEvent && oEvent.getSource && oEvent.getSource());
            return this._withViewFlag("/locationVhBusy", function () {
                return DetailCommandPolicy.valueHelpLocation(this, { intent: "open" });
            }.bind(this)).then(function (oResult) {
                this._scheduleLocationValueHelpTableSync();
                return oResult;
            }.bind(this));
        },

        onCloseLocationValueHelp: function () {
            this._clearLocationValueHelpSearchTimer();
            return DetailCommandPolicy.valueHelpLocation(this, { intent: "close" }).finally(function () {
                this._setViewFlag("/locationVhBusy", false);
            }.bind(this));
        },

        onConfirmLocationValueHelp: function () {
            return this._withViewFlag("/locationVhBusy", function () {
                return DetailCommandPolicy.valueHelpLocation(this, { intent: "confirm" });
            }.bind(this));
        },

        onLpcChange: function (oEvent) {
            DetailCommandPolicy.autosave(this, { rootId: this._currentRootId(), field: "LPC_KEY", value: oEvent.getParameter("value") });
        },

        onProfessionChange: function (oEvent) {
            DetailCommandPolicy.autosave(this, { rootId: this._currentRootId(), field: "PROF_KEY", value: oEvent.getParameter("value") });
        },

        onPersonSuggest: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            if (!this._isEditMode()) {
                return;
            }
            DetailCommandPolicy.personSuggest(this, {
                term: oEvent.getParameter("suggestValue"),
                target: DetailPersonInputSupport.targetFromSource(oSource)
            });
        },

        onPersonSuggestionSelected: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            if (!this._isEditMode()) {
                return;
            }
            DetailCommandPolicy.personSuggest(this, {
                intent: "selected",
                item: oEvent.getParameter("selectedItem"),
                target: DetailPersonInputSupport.targetFromSource(oSource)
            });
        },

        onPersonInputChange: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            if (!this._isEditMode()) {
                return;
            }
            DetailCommandPolicy.personSuggest(this, {
                intent: "manualChange",
                value: oEvent.getParameter("value"),
                target: DetailPersonInputSupport.targetFromSource(oSource)
            });
        },

        onLocationValueHelpSearch: function (oEvent) {
            var sValue = oEvent.getParameter("newValue");
            this._clearLocationValueHelpSearchTimer();
            this._iLocationVhSearchTimer = SchedulingRuntime.restartTimer(0, function () {
                this._iLocationVhSearchTimer = null;
                this._withViewFlag("/locationVhBusy", function () {
                    return DetailCommandPolicy.valueHelpLocation(this, { intent: "search", value: sValue });
                }.bind(this)).then(function () {
                    this._scheduleLocationValueHelpTableSync();
                }.bind(this));
            }.bind(this), 180);
        },

        onLocationValueHelpSearchSubmit: function (oEvent) {
            var sQuery = oEvent.getParameter("query");
            this._clearLocationValueHelpSearchTimer();
            return this._withViewFlag("/locationVhBusy", function () {
                return DetailCommandPolicy.valueHelpLocation(this, { intent: "search", value: sQuery });
            }.bind(this)).then(function () {
                this._scheduleLocationValueHelpTableSync();
            }.bind(this));
        },

        onLocationTreeSelectionChange: function (oEvent) {
            DetailCommandPolicy.valueHelpLocation(this, { intent: "treeSelection", event: oEvent });
        }
    };
});
