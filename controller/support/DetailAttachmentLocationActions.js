sap.ui.define([
    "sap_ui5/controller/support/AttachmentUploadSupport",
    "sap_ui5/controller/support/DetailCommandPolicy",
    "sap_ui5/controller/support/DetailPersonInputSupport",
    "sap_ui5/infra/navigation/WorkspaceRouteNavigation",
    "sap_ui5/controller/support/ControllerModelWriteSupport"
], function (AttachmentUploadSupport, DetailCommandPolicy, DetailPersonInputSupport, WorkspaceRouteNavigation, ControllerModelWriteSupport) {
    "use strict";

    return {
        onAttachmentUploadChange: function (oEvent) {
            AttachmentUploadSupport.onUploaderChange(this, oEvent);
        },

        onDeleteAttachment: function (oEvent) {
            var oCtx = oEvent && oEvent.getSource && oEvent.getSource().getBindingContext("selected");
            var oRow = oCtx && oCtx.getObject && oCtx.getObject();
            var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
            if (!sAttachmentId) {
                return;
            }
            return ControllerModelWriteSupport.withFlag(this, "view", "/attachmentBusy", function () {
                return DetailCommandPolicy.attachmentDelete(this, {
                    rootId: this._currentRootId(),
                    attachmentId: sAttachmentId,
                    attachment: oRow || null
                });
            }.bind(this));
        },

        onToggleAttachmentsSection: function () {
            var bExpanded = !!ControllerModelWriteSupport.get(this, "view", "/attachmentsExpanded", false);
            var bLoaded = !!ControllerModelWriteSupport.get(this, "view", "/attachmentsLoaded", false);
            if (bExpanded) {
                ControllerModelWriteSupport.set(this, "view", "/attachmentsExpanded", false);
                AttachmentUploadSupport.unbindDropZone(this);
                return Promise.resolve({ collapsed: true });
            }
            ControllerModelWriteSupport.set(this, "view", "/attachmentsExpanded", true);
            this._scheduleAttachmentDropZoneBind();
            if (bLoaded) {
                return Promise.resolve({ expanded: true, loaded: true });
            }
            ControllerModelWriteSupport.set(this, "view", "/attachmentBusy", true);
            return DetailCommandPolicy.attachmentLoad(this, {
                rootId: this._currentRootId()
            });
        },
        onOpenWorkflowAnalytics: function () {
            WorkspaceRouteNavigation.navigateToAnalytics(this);
            return Promise.resolve();
        },

        onOpenAttachment: function (oEvent) {
            var oCtx = oEvent && oEvent.getSource && oEvent.getSource().getBindingContext("selected");
            var oRow = oCtx && oCtx.getObject && oCtx.getObject();
            var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
            var sLocalObjectUrl = String((oRow && oRow.localObjectUrl) || "").trim();
            var oMainService = this.getModel("mainService");
            var sBaseUrl = String((oMainService && oMainService.sServiceUrl) || "").replace(/\/+$/, "");
            var sFileName = String((oRow && oRow.FileName) || "attachment").trim() || "attachment";
            var oLink;
            var sHref;
            function triggerDownload(sUrl) {
                if (!sUrl) {
                    return;
                }
                oLink = document.createElement("a");
                oLink.href = sUrl;
                oLink.download = sFileName;
                oLink.rel = "noopener";
                oLink.style.display = "none";
                document.body.appendChild(oLink);
                oLink.click();
                document.body.removeChild(oLink);
            }
            if (sLocalObjectUrl) {
                triggerDownload(sLocalObjectUrl);
                return;
            }
            if (!sAttachmentId || !sBaseUrl) {
                return;
            }
            sHref = sBaseUrl + "/AttachmentSet(Key='" + sAttachmentId + "')/$value";
            triggerDownload(sHref);
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
            this._iLocationVhSearchTimer = setTimeout(function () {
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
