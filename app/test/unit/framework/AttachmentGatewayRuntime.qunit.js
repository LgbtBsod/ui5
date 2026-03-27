sap.ui.define([
    "sap/ui/unified/FileUploaderParameter",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/AttachmentGatewayRuntime"
], function (FileUploaderParameter, AttachmentGatewayRuntime) {
    "use strict";

    QUnit.module("framework/AttachmentGatewayRuntime");

    QUnit.test("gateway uploader posts pending attachments through FileUploader media upload", function (assert) {
        var done = assert.async();
        var sUploadUrl = "";
        var bSendXHR = false;
        var bUseMultipart = true;
        var bMultiple = true;
        var aHeaderParameters = [];
        var fnComplete = null;
        var bUploadCalled = false;
        var bCleared = false;
        var oUploader = {
            setUploadUrl: function (sValue) {
                sUploadUrl = sValue;
            },
            setSendXHR: function (bValue) {
                bSendXHR = bValue;
            },
            setUseMultipart: function (bValue) {
                bUseMultipart = bValue;
            },
            setMultiple: function (bValue) {
                bMultiple = bValue;
            },
            setSameFilenameAllowed: function () {},
            removeAllHeaderParameters: function () {
                aHeaderParameters = [];
            },
            addHeaderParameter: function (oParameter) {
                aHeaderParameters.push(oParameter);
            },
            attachUploadComplete: function (fnHandler) {
                fnComplete = fnHandler;
            },
            detachUploadComplete: function (fnHandler) {
                if (fnComplete === fnHandler) {
                    fnComplete = null;
                }
            },
            clear: function () {
                bCleared = true;
            },
            upload: function () {
                bUploadCalled = true;
                fnComplete({
                    getParameter: function (sName) {
                        if (sName === "status") {
                            return 201;
                        }
                        if (sName === "responseRaw" || sName === "response") {
                            return "{\"AttachmentKey\":\"ATT-1\"}";
                        }
                        return null;
                    }
                });
            }
        };
        var oController = {
            byId: function (sId) {
                return sId === "attachmentUploader" ? oUploader : null;
            },
            getModel: function (sName) {
                if (sName !== "mainService") {
                    return null;
                }
                return {
                    sServiceUrl: "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/",
                    getSecurityToken: function () {
                        return "TOKEN-1";
                    }
                };
            }
        };

        AttachmentGatewayRuntime.uploadPendingAttachments(oController, {
            rootId: "001122",
            attachments: [{
                file: {
                    name: "evidence.txt",
                    type: "text/plain"
                },
                fileName: "evidence.txt",
                categoryKey: "GEN",
                description: "desc"
            }]
        }).then(function () {
            var mHeaders = {};

            aHeaderParameters.forEach(function (oParameter) {
                if (oParameter instanceof FileUploaderParameter) {
                    mHeaders[oParameter.getName()] = oParameter.getValue();
                }
            });

            assert.strictEqual(sUploadUrl, "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/AttachmentSet", "upload url targets attachment entity set");
            assert.strictEqual(bSendXHR, true, "xhr mode is enabled");
            assert.strictEqual(bUseMultipart, false, "gateway media upload uses raw body");
            assert.strictEqual(bMultiple, false, "uploader is constrained to a single file");
            assert.strictEqual(bUploadCalled, true, "upload is triggered");
            assert.strictEqual(mHeaders["X-CSRF-Token"], "TOKEN-1", "csrf token is forwarded");
            assert.strictEqual(mHeaders["X-DB-Key"], "001122", "root key is forwarded");
            assert.strictEqual(mHeaders["X-Parent-Key"], "001122", "parent key stays canonical");
            assert.strictEqual(mHeaders["X-Category-Key"], "GEN", "category header is forwarded");
            assert.strictEqual(mHeaders["X-Description"], "desc", "description header is forwarded");
            assert.strictEqual(mHeaders["X-File-Name"], "evidence.txt", "filename header is forwarded");
            assert.strictEqual(mHeaders.Slug, "evidence.txt", "slug carries the file name");
            assert.strictEqual(bCleared, true, "uploader is cleared after completion");
            done();
        }).catch(function (oError) {
            assert.ok(false, oError && oError.message || String(oError));
            done();
        });
    });
});
