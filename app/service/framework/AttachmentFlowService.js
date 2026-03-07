sap.ui.define([], function () {
    "use strict";

    function read(oController, sModelName, sPath, vFallback) {
        var oModel = oController && oController.getModel && oController.getModel(sModelName);
        if (!oModel || typeof oModel.getProperty !== "function") {
            return vFallback;
        }
        return oModel.getProperty(sPath);
    }

    function write(oController, sModelName, sPath, vValue) {
        var oModel = oController && oController.getModel && oController.getModel(sModelName);
        if (!oModel || typeof oModel.setProperty !== "function") {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        return true;
    }

    function resolveCurrentRootId(oController) {
        return String((oController && oController._currentRootId && oController._currentRootId()) || "").trim();
    }

    function deleteAttachment(oController, oEvent) {
        var oCtx = oEvent && oEvent.getSource && oEvent.getSource().getBindingContext("selected");
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        if (!sAttachmentId) {
            return Promise.resolve(false);
        }
        write(oController, "view", "/attachmentBusy", true);
        return Promise.resolve(oController._run("attachmentDelete", {
            rootId: resolveCurrentRootId(oController),
            attachmentId: sAttachmentId,
            attachment: oRow || null
        })).finally(function () {
            write(oController, "view", "/attachmentBusy", false);
        });
    }

    function toggleHistory(oController, mRuntime) {
        var bExpanded = !!read(oController, "view", "/attachmentsExpanded", false);
        var bLoaded = !!read(oController, "view", "/attachmentsLoaded", false);
        if (bExpanded) {
            write(oController, "view", "/attachmentsExpanded", false);
            if (mRuntime && typeof mRuntime.unbindDropZone === "function") {
                mRuntime.unbindDropZone(oController);
            }
            return Promise.resolve({ collapsed: true });
        }
        write(oController, "view", "/attachmentsExpanded", true);
        if (mRuntime && typeof mRuntime.bindDropZone === "function") {
            mRuntime.bindDropZone(oController);
        } else if (oController && typeof oController._scheduleAttachmentDropZoneBind === "function") {
            oController._scheduleAttachmentDropZoneBind();
        }
        if (bLoaded) {
            return Promise.resolve({ expanded: true, loaded: true });
        }
        write(oController, "view", "/attachmentBusy", true);
        return Promise.resolve(oController._run("attachmentLoad", {
            rootId: resolveCurrentRootId(oController)
        })).finally(function () {
            write(oController, "view", "/attachmentBusy", false);
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
