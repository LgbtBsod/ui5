sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailPersonInputSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (AttachmentUploadCore, DetailCommandPolicy, DetailPersonInputSupport, ControllerViewStateRuntime, ModelStateRuntime, NavigationIntentService, RootIdRuntime, SchedulingRuntime) {
    "use strict";

    function resolveAttachmentContext(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        return (oSource && oSource.getBindingContext && (oSource.getBindingContext("selected") || oSource.getBindingContext("view"))) || null;
    }

    function deleteAttachment(oController, oEvent) {
        var oCtx = resolveAttachmentContext(oEvent);
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        if (!sAttachmentId) {
            return Promise.resolve(false);
        }
        return ModelStateRuntime.withFlag(oController, "view", "/attachmentBusy", function () {
            return DetailCommandPolicy.attachmentDelete(oController, RootIdRuntime.withCurrentRootId(oController, {
                attachmentId: sAttachmentId,
                attachment: oRow || null
            }));
        });
    }

    function toggleAttachmentsSection(oController) {
        var bExpanded = !!ModelStateRuntime.read(oController, "view", "/attachmentsExpanded", false);
        var bLoaded = !!ModelStateRuntime.read(oController, "view", "/attachmentsLoaded", false);
        if (bExpanded) {
            ModelStateRuntime.write(oController, "view", "/attachmentsExpanded", false);
            if (oController && typeof oController._unbindAttachmentDropZone === "function") {
                oController._unbindAttachmentDropZone();
            }
            return Promise.resolve({ collapsed: true });
        }
        ModelStateRuntime.write(oController, "view", "/attachmentsExpanded", true);
        if (oController && typeof oController._scheduleAttachmentDropZoneBind === "function") {
            oController._scheduleAttachmentDropZoneBind();
        }
        if (bLoaded) {
            return Promise.resolve({ expanded: true, loaded: true });
        }
        return ModelStateRuntime.withFlag(oController, "view", "/attachmentBusy", function () {
            return DetailCommandPolicy.attachmentLoad(oController, RootIdRuntime.withCurrentRootId(oController));
        });
    }

    function openAttachment(oController, oEvent) {
        var oCtx = resolveAttachmentContext(oEvent);
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        var sLocalObjectUrl = String((oRow && oRow.localObjectUrl) || "").trim();
        var oMainService = oController && oController.getModel && oController.getModel("mainService");
        var sBaseUrl = String((oMainService && oMainService.sServiceUrl) || "").replace(/\/+$/, "");
        var sFileName = String((oRow && oRow.FileName) || "attachment").trim() || "attachment";
        var oLink;
        var sHref;

        function triggerDownload(sUrl) {
            if (!sUrl) {
                return false;
            }
            oLink = document.createElement("a");
            oLink.href = sUrl;
            oLink.download = sFileName;
            oLink.rel = "noopener";
            oLink.style.display = "none";
            document.body.appendChild(oLink);
            oLink.click();
            document.body.removeChild(oLink);
            return true;
        }

        if (sLocalObjectUrl) {
            return triggerDownload(sLocalObjectUrl);
        }
        if (!sAttachmentId || !sBaseUrl) {
            return false;
        }
        sHref = sBaseUrl + "/AttachmentSet(Key='" + sAttachmentId + "')/$value";
        return triggerDownload(sHref);
    }

    return {
        onAttachmentUploadChange: function (oEvent) {
            return AttachmentUploadCore.onUploaderChange(this, oEvent);
        },

        onDeleteAttachment: function (oEvent) {
            return deleteAttachment(this, oEvent);
        },

        onToggleAttachmentsSection: function () {
            return toggleAttachmentsSection(this);
        },
        onOpenWorkflowAnalytics: function () {
            NavigationIntentService.navigateToAnalytics(this);
            return Promise.resolve();
        },

        onOpenAttachment: function (oEvent) {
            return openAttachment(this, oEvent);
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
                ControllerViewStateRuntime.setFlag(this, "/locationVhBusy", false);
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
            var oSelectedItem = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
            var sTarget = DetailPersonInputSupport.targetFromSource(oSource);
            var sSelectedValue = "";
            if (!this._isEditMode()) {
                return;
            }
            if (oSelectedItem && typeof oSelectedItem.getText === "function") {
                sSelectedValue = String(oSelectedItem.getText() || "");
            }
            DetailPersonInputSupport.rememberSuggestionSelection(this, sTarget, sSelectedValue);
            DetailCommandPolicy.personSuggest(this, {
                intent: "selected",
                item: oSelectedItem,
                target: sTarget
            });
        },

        onPersonInputChange: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var sTarget = DetailPersonInputSupport.targetFromSource(oSource);
            var sValue;
            if (!this._isEditMode()) {
                return;
            }
            sValue = String((oEvent && oEvent.getParameter && oEvent.getParameter("value")) || "");
            if (DetailPersonInputSupport.consumeSuggestionSelection(this, sTarget, sValue)) {
                return;
            }
            DetailCommandPolicy.personSuggest(this, {
                intent: "manualChange",
                value: sValue,
                target: sTarget
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
