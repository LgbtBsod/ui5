sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayAdapterSupport",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/AttachmentRepoSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/AccessPayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayAdapterSupport, ChecklistSnapshotMapper, AttachmentRepoSupport, AccessPayload, DetailRuntimePayload, CreateSentinel, GatewayClient) {
    "use strict";
    function rootId(mArgs) { return DetailRuntimePayload.rootId(mArgs); }
    function normalizeRootKey(sRootId) { return DetailRuntimePayload.normalizeRootKey(sRootId); }
    function resolveServerRootId(oPayload, sFallbackRootId) {
        var oData = oPayload || {};
        return String(
            oData.RootKey ||
            oData.rootKey ||
            oData.Key ||
            oData.key ||
            oData.pcct_uuid ||
            (oData.root && (oData.root.pcct_uuid || oData.root.RootKey || oData.root.Key)) ||
            (oData.root && oData.root.id) ||
            sFallbackRootId ||
            ""
        ).trim();
    }

    function normalizeSavePayload(sRootId, oPayload) {
        var oIn = oPayload || {};
        if (Object.prototype.hasOwnProperty.call(oIn, "root") || Object.prototype.hasOwnProperty.call(oIn, "checks") || Object.prototype.hasOwnProperty.call(oIn, "barriers")) {
            return Object.assign({}, oIn, {
                root: Object.assign({}, oIn.root || {}, {
                    pcct_uuid: normalizeRootKey((oIn.root && oIn.root.pcct_uuid) || sRootId)
                }),
                client_version: Number(oIn.client_version || ((oIn.root || {}).version_number) || 0) || 0,
                SessionGuid: oIn.SessionGuid || oIn.session_guid || null
            });
        }
        return { RootKey: normalizeRootKey(sRootId), ClientAggChangedOn: (oIn.meta && oIn.meta.aggChangedOn) || null, FullPayload: { root: oIn.root || {}, basic: oIn.basic || {}, checks: oIn.checks || [], barriers: oIn.barriers || [] } };
    }

    function firstRow(vData) {
        var oUnwrapped = GatewayAdapterSupport.unwrap(vData);
        if (Array.isArray(oUnwrapped)) { return oUnwrapped[0] || {}; }
        if (oUnwrapped && Array.isArray(oUnwrapped.results)) { return oUnwrapped.results[0] || {}; }
        return oUnwrapped || {};
    }
    function pad2(iValue) {
        var s = String(iValue);
        return s.length >= 2 ? s : "0" + s;
    }
    function formatUtcDate(oDate) {
        if (!(oDate instanceof Date) || Number.isNaN(oDate.getTime())) {
            return "";
        }
        return [
            oDate.getUTCFullYear(),
            pad2(oDate.getUTCMonth() + 1),
            pad2(oDate.getUTCDate())
        ].join("-");
    }
    function toYmd(vDateCheck) {
        var s;
        var m;
        var oDate;
        if (vDateCheck instanceof Date) {
            return formatUtcDate(vDateCheck);
        }
        if (typeof vDateCheck === "number" && Number.isFinite(vDateCheck)) {
            return formatUtcDate(new Date(vDateCheck));
        }
        s = String(vDateCheck || "").trim();
        if (!s) {
            return "";
        }
        m = /\/(?:Date\()?([0-9]{10,13})/.exec(s);
        if (m) {
            return formatUtcDate(new Date(Number(m[1])));
        }
        if (/^\d{4}-\d{2}-\d{2}/.test(s)) {
            return s.slice(0, 10);
        }
        oDate = new Date(s);
        return formatUtcDate(oDate);
    }
    function mapBasic(oBasic) {
        var o = oBasic || {};
        var sBrowserTimezone = (Intl && Intl.DateTimeFormat && Intl.DateTimeFormat().resolvedOptions && Intl.DateTimeFormat().resolvedOptions().timeZone) || "UTC";
        return {
            checklist_id: o.ChecklistId || o.ChecklistID || o.CHECKLIST_ID || o.checklist_id || "",
            date: toYmd(o.DateCheck || o.date),
            time: String(o.TimeCheck || o.time || "").slice(0, 5),
            timezone: o.TimeZone || o.timezone || sBrowserTimezone,
            equipment: o.EquipName || o.equipment || "",
            OBSERVER_FULLNAME: o.ObserverFullname || o.OBSERVER_FULLNAME || "",
            OBSERVER_PERNER: o.ObserverPernr || o.OBSERVER_PERNER || "",
            OBSERVER_POSITION: o.ObserverPosition || o.OBSERVER_POSITION || "",
            OBSERVER_ORGUNIT: o.ObserverOrgUnit || o.OBSERVER_ORGUNIT || "",
            OBSERVED_FULLNAME: o.ObservedFullname || o.OBSERVED_FULLNAME || "",
            OBSERVED_PERNER: o.ObservedPernr || o.OBSERVED_PERNER || "",
            OBSERVED_POSITION: o.ObservedPosition || o.OBSERVED_POSITION || "",
            OBSERVED_ORGUNIT: o.ObservedOrgUnit || o.OBSERVED_ORGUNIT || "",
            LOCATION_KEY: o.LocationKey || o.LOCATION_KEY || "",
            LOCATION_NAME: o.LocationName || o.LOCATION_NAME || "",
            LOCATION_TEXT: o.LocationText || o.LOCATION_TEXT || "",
            LPC_KEY: o.Lpc || o.LPC_KEY || "",
            LPC_TEXT: o.LpcText || o.LPC_TEXT || o.Lpc || "",
            PROF_KEY: o.Profession || o.PROF_KEY || "",
            PROF_TEXT: o.ProfessionText || o.PROF_TEXT || o.Profession || ""
        };
    }
    function mapResult(oRoot, oBasic, oChecks, oBarriers) {
        var oRootRow = firstRow(oRoot);
        var oBasicRow = firstRow(oBasic);
        var oMappedBasic = mapBasic(oBasicRow);
        var sAggChangedOn = oRootRow.ChangedOn || oRootRow.AggChangedOn || "";
        var iVersionNumber = Number(oRootRow.VersionNumber || oRootRow.version_number || 0) || 0;
        var sRootId = String(oRootRow.Key || oRootRow.RootKey || oRootRow.pcct_uuid || "").trim();
        if (sAggChangedOn && !oRootRow.server_changed_on) {
            oRootRow.server_changed_on = sAggChangedOn;
        }
        if (sRootId && !oRootRow.id) {
            oRootRow.id = sRootId;
        }
        if (sRootId && !oRootRow.pcct_uuid) {
            oRootRow.pcct_uuid = sRootId;
        }
        if (oMappedBasic.checklist_id && !oRootRow.checklist_id) {
            oRootRow.checklist_id = oMappedBasic.checklist_id;
        }
        if (iVersionNumber) {
            oRootRow.version_number = iVersionNumber;
            oRootRow.VersionNumber = iVersionNumber;
        }
        return {
            root: oRootRow,
            basic: oMappedBasic,
            checks: GatewayAdapterSupport.asArray(oChecks).map(ChecklistSnapshotMapper.mapCheckRow),
            barriers: GatewayAdapterSupport.asArray(oBarriers).map(ChecklistSnapshotMapper.mapBarrierRow),
            attachments: [],
            meta: { source: "GatewayODataClient", aggChangedOn: sAggChangedOn, versionNumber: iVersionNumber }
        };
    }
    function enrichServerSnapshot(oServerPayload, sFallbackRootId) {
        var sResolvedRootId = resolveServerRootId(oServerPayload, sFallbackRootId);
        if (!sResolvedRootId || CreateSentinel.isCreateId(sResolvedRootId)) {
            return Promise.resolve(oServerPayload || {});
        }
        return loadDetailSnapshot({ rootId: sResolvedRootId }).then(function (oSnapshot) {
            var oResolvedSnapshot = oSnapshot || {};
            var oMeta = Object.assign({}, oResolvedSnapshot.meta || {});
            var oRoot = Object.assign({}, oResolvedSnapshot.root || {});
            var sAggChangedOn = (oServerPayload && (oServerPayload.AggChangedOn || oServerPayload.ChangedOn || oServerPayload.changed_on)) || oMeta.aggChangedOn || oRoot.server_changed_on || "";
            var iVersionNumber = Number((oServerPayload && (oServerPayload.version_number || oServerPayload.VersionNumber)) || oRoot.version_number || oMeta.versionNumber || 0) || 0;
            if (sAggChangedOn) {
                oMeta.aggChangedOn = sAggChangedOn;
                oRoot.server_changed_on = oRoot.server_changed_on || sAggChangedOn;
            }
            if (iVersionNumber) {
                oMeta.versionNumber = iVersionNumber;
                oRoot.version_number = iVersionNumber;
                oRoot.VersionNumber = iVersionNumber;
            }
            return Object.assign({}, oResolvedSnapshot, {
                root: Object.assign({}, oRoot, {
                    id: String(oRoot.id || sResolvedRootId).trim()
                }),
                meta: oMeta
            });
        }).catch(function () {
            return oServerPayload || {};
        });
    }
    function loadDetailSnapshot(mArgs) {
        var sRootId = rootId(mArgs);
        var pRoot = GatewayAdapterSupport.get("ChecklistRootSet('" + sRootId + "')");
        var pBasic = GatewayAdapterSupport.get("ChecklistBasicInfoSet", { "$filter": "RootKey eq '" + sRootId + "'" });
        var pChecks = GatewayAdapterSupport.get("ChecklistCheckSet", { "$filter": "RootKey eq '" + sRootId + "'" });
        var pBarriers = GatewayAdapterSupport.get("ChecklistBarrierSet", { "$filter": "RootKey eq '" + sRootId + "'" });
        return Promise.all([pRoot, pBasic, pChecks, pBarriers]).then(function (aResult) {
            var oSnapshot = mapResult(aResult[0], aResult[1], aResult[2], aResult[3]);
            oSnapshot.attachments = [];
            return oSnapshot;
        });
    }
    function setChecklistStatus(mArgs) {
        var sRootId = rootId(mArgs);
        var sStatusCode = mArgs && mArgs.statusCode;
        return GatewayAdapterSupport.postFunction("SetChecklistStatus", {
            RootKey: normalizeRootKey(sRootId), NewStatus: sStatusCode, ClientAggChangedOn: (mArgs && mArgs.clientAggChangedOn) || null
        }).then(function (oResponse) {
            return { statusCode: sStatusCode, aggChangedOn: (oResponse && oResponse.AggChangedOn) || "" };
        });
    }
    function deleteChecklist(mArgs) {
        var sRootId = normalizeRootKey(rootId(mArgs));
        return GatewayClient.deletePath("/ChecklistRootSet('" + sRootId + "')").then(function () {
            return { deleted: true, rootId: sRootId };
        });
    }

    function parseGrantedOperations(vValue) {
        return String(vValue || "").split(",").map(function (sCode) {
            return String(sCode || "").trim();
        }).filter(Boolean);
    }

    function normalizeChecklistIds(aIds) {
        var mSeen = {};
        return (aIds || []).reduce(function (aAcc, sId) {
            var sNormalized = String(sId || "").trim();
            if (!sNormalized || mSeen[sNormalized]) {
                return aAcc;
            }
            mSeen[sNormalized] = true;
            aAcc.push(sNormalized);
            return aAcc;
        }, []);
    }

    function normalizePermissionResponse(oPermission, sRootId, sActivity) {
        var aGranted = parseGrantedOperations(oPermission && oPermission.GrantedOperations);
        var bCanCreate = !!(oPermission && oPermission.CanCreate) || aGranted.indexOf("01") >= 0;
        var bCanView = !!(oPermission && oPermission.CanView) || aGranted.indexOf("03") >= 0;
        var bCanEdit = !!(oPermission && oPermission.CanEdit) || aGranted.indexOf("02") >= 0;
        var bCanDelete = !!(oPermission && oPermission.CanDelete) || aGranted.indexOf("06") >= 0;
        return AccessPayload.normalizePermission({
            rootId: sRootId,
            userId: String((oPermission && (oPermission.UserId || oPermission.userId)) || "").trim(),
            canCreate: bCanCreate,
            canView: bCanView,
            canEdit: bCanEdit,
            canDelete: bCanDelete,
            reasonCode: String((oPermission && (oPermission.ReasonCode || oPermission.reasonCode)) || "AUTHORIZED").trim() || "AUTHORIZED",
            message: String((oPermission && (oPermission.Message || oPermission.message)) || "").trim(),
            requestedActivity: sActivity
        }, sRootId, {
            requestedActivity: sActivity
        });
    }

    function checkCreatePermission(sActivity) {
        return GatewayAdapterSupport.get("ChecklistCreatePermissionSet('CURRENT')", {
            "__ts": Date.now()
        }).then(function (oResponse) {
            return normalizePermissionResponse(firstRow(oResponse), "", sActivity);
        });
    }

    function checkChecklistPermission(mArgs, mDeps) {
        var sRootId = normalizeRootKey(rootId(mArgs));
        var sActivity = String((mArgs && (mArgs.activity || mArgs.ACTVT)) || "").trim();
        if (!sRootId) {
            if (sActivity !== "01") {
                return Promise.resolve(AccessPayload.normalizePermission({
                    rootId: "",
                    userId: "",
                    canCreate: false,
                    canView: false,
                    canEdit: false,
                    canDelete: false,
                    reasonCode: "INVALID_PERMISSION_TARGET",
                    message: "RootKey is required for non-create permissions",
                    requestedActivity: sActivity
                }, "", {
                    requestedActivity: sActivity
                }));
            }
            return checkCreatePermission(sActivity);
        }
        return GatewayAdapterSupport.get("ChecklistPermissionSet('" + sRootId + "')", {
            ACTVT: sActivity
        }).then(function (oResponse) {
            return normalizePermissionResponse(firstRow(oResponse), sRootId, sActivity);
        });
    }

    function create(mDeps) {
        return {
            loadDetailSnapshot: loadDetailSnapshot,
            saveChecklist: function (mArgs) {
                var sRootId = rootId(mArgs);
                var oDelta = (mArgs && mArgs.delta) || {};
                var sSessionGuid = String((mArgs && mArgs.sessionGuid) || "").trim();
                var oRequest = normalizeSavePayload(sRootId, oDelta);
                if (sSessionGuid) {
                    oRequest.SessionGuid = sSessionGuid;
                    oRequest.session_guid = sSessionGuid;
                }
                return GatewayAdapterSupport.request({ method: "POST_ENTITY", path: "SaveChanges", body: oRequest }).then(function (oServerPayload) {
                    return enrichServerSnapshot(oServerPayload, sRootId).then(function (oServerSnapshot) {
                        return { serverSnapshot: oServerSnapshot || {}, lastChangeSet: {}, serverResponse: oServerPayload || {} };
                    });
                });
            },
            createChecklist: function (mArgs) {
                var oCurrent = (mArgs && mArgs.delta) || {};
                var oRequest = normalizeSavePayload("", oCurrent);
                return GatewayAdapterSupport.request({ method: "POST_ENTITY", path: "CreateChecklist", body: oRequest }).then(function (oServerPayload) {
                    return enrichServerSnapshot(oServerPayload, "").then(function (oServerSnapshot) {
                        return { serverSnapshot: oServerSnapshot || {}, lastChangeSet: {}, serverResponse: oServerPayload || {} };
                    });
                });
            },
            copyChecklist: function (mArgs) {
                var sRootId = rootId(mArgs);
                var sSessionGuid = String((mArgs && mArgs.sessionGuid) || "").trim();
                return GatewayAdapterSupport.postFunction("CopyChecklist", {
                    RootId: normalizeRootKey(sRootId),
                    SessionGuid: sSessionGuid
                }).then(function (oServerPayload) {
                    return enrichServerSnapshot(oServerPayload, "").then(function (oServerSnapshot) {
                        return { serverSnapshot: oServerSnapshot || {}, serverResponse: oServerPayload || {} };
                    });
                });
            },
            autosaveChecklist: function (mArgs) {
                var sRootId = rootId(mArgs);
                var oDelta = (mArgs && mArgs.delta) || {};
                var sSessionGuid = String((mArgs && mArgs.sessionGuid) || "").trim();
                var oRequest = normalizeSavePayload(sRootId, oDelta);
                if (sSessionGuid) {
                    oRequest.SessionGuid = sSessionGuid;
                    oRequest.session_guid = sSessionGuid;
                }
                return GatewayAdapterSupport.request({ method: "POST_ENTITY", path: "AutoSave", body: oRequest }).then(function (oResponse) {
                    return enrichServerSnapshot(oResponse, sRootId).then(function (oServerSnapshot) {
                        return { autosavedAt: new Date().toISOString(), serverHints: oResponse || {}, serverSnapshot: oServerSnapshot || {} };
                    });
                });
            },
            setChecklistStatus: setChecklistStatus,
            deleteChecklist: deleteChecklist,
            exportSearchResults: function (mArgs) {
                var aRootIds = normalizeChecklistIds((mArgs && (mArgs.rootIds || mArgs.RootKeys)) || []);
                var oPayload = {
                    Entity: String((mArgs && mArgs.entity) || "screen").trim() || "screen",
                    Limit: Math.max(1, Number((mArgs && mArgs.limit) || 0) || 200000),
                    SelectionMode: String((mArgs && mArgs.selectionMode) || (aRootIds.length ? "selected" : "all")).trim() || "all"
                };
                if (aRootIds.length) {
                    oPayload.RootKeys = aRootIds;
                }
                if (!aRootIds.length && mArgs && mArgs.searchContract) {
                    oPayload.SearchContract = Object.assign({}, mArgs.searchContract);
                }
                return GatewayAdapterSupport.request({
                    method: "POST_ENTITY",
                    path: "ReportExport",
                    body: oPayload
                }).then(function (oResponse) {
                    return GatewayAdapterSupport.asArray(oResponse);
                });
            },
            checkChecklistPermission: function (mArgs) {
                return checkChecklistPermission(mArgs, mDeps || {});
            },
            loadAttachments: AttachmentRepoSupport.loadAttachments,
            uploadAttachment: AttachmentRepoSupport.uploadAttachment,
            deleteAttachment: AttachmentRepoSupport.deleteAttachment
        };
    }
    return { create: create };
});
