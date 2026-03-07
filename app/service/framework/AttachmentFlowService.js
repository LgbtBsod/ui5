sap.ui.define([
    "checklist/app/controller/support/DetailCommandPolicy",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/RootIdRuntime"
], function (DetailCommandPolicy, ModelStateRuntime, RootIdRuntime) {
    "use strict";

    function deleteAttachment(oController, oEvent) {
        var oCtx = oEvent && oEvent.getSource && oEvent.getSource().getBindingContext("selected");
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        if (!sAttachmentId) {
            return Promise.resolve(false);
        }
        ModelStateRuntime.write(oController, "view", "/attachmentBusy", true);
        return Promise.resolve(DetailCommandPolicy.attachmentDelete(oController, {
            rootId: RootIdRuntime.resolveCurrentRootId(oController),
            attachmentId: sAttachmentId,
            attachment: oRow || null
        })).finally(function () {
            ModelStateRuntime.write(oController, "view", "/attachmentBusy", false);
        });
    }

    function toggleHistory(oController, mRuntime) {
        var bExpanded = !!ModelStateRuntime.read(oController, "view", "/attachmentsExpanded", false);
        var bLoaded = !!ModelStateRuntime.read(oController, "view", "/attachmentsLoaded", false);
        if (bExpanded) {
            ModelStateRuntime.write(oController, "view", "/attachmentsExpanded", false);
            if (mRuntime && typeof mRuntime.unbindDropZone === "function") {
                mRuntime.unbindDropZone(oController);
            }
            return Promise.resolve({ collapsed: true });
        }
        ModelStateRuntime.write(oController, "view", "/attachmentsExpanded", true);
        if (mRuntime && typeof mRuntime.bindDropZone === "function") {
            mRuntime.bindDropZone(oController);
        } else if (oController && typeof oController._scheduleAttachmentDropZoneBind === "function") {
            oController._scheduleAttachmentDropZoneBind();
        }
        if (bLoaded) {
            return Promise.resolve({ expanded: true, loaded: true });
        }
        ModelStateRuntime.write(oController, "view", "/attachmentBusy", true);
        return Promise.resolve(DetailCommandPolicy.attachmentLoad(oController, {
            rootId: RootIdRuntime.resolveCurrentRootId(oController)
        })).finally(function () {
            ModelStateRuntime.write(oController, "view", "/attachmentBusy", false);
        });
    }

    function openAttachment(oController, oEvent) {
        var oCtx = oEvent && oEvent.getSource && oEvent.getSource().getBindingContext("selected");
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

    function onUploaderChange(oController, oEvent, mRuntime) {
        if (mRuntime && typeof mRuntime.onUploaderChange === "function") {
            return mRuntime.onUploaderChange(oController, oEvent);
        }
        return undefined;
    }

    return {
        deleteAttachment: deleteAttachment,
        toggleHistory: toggleHistory,
        openAttachment: openAttachment,
        onUploaderChange: onUploaderChange
    };
});
