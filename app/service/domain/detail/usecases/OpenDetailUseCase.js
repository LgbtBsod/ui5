sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseInputUtils",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (UseCase, Result, Effects, DetailAuthorizationSupport, ViewPathContracts, UseCaseInputUtils, StatePaths, CreateSentinel) {
    "use strict";

    function resolveLoadedAttachments(oUiState, sRootId) {
        var sSelectedRootId = String((oUiState && oUiState.get("selected", "/root/id")) || "").trim();
        var bAttachmentsLoaded = !!(oUiState && oUiState.get("view", ViewPathContracts.ATTACHMENTS_LOADED));
        var aAttachments = (oUiState && oUiState.get("selected", "/attachments")) || [];
        if (!sRootId || sSelectedRootId !== sRootId || !bAttachmentsLoaded || !Array.isArray(aAttachments)) {
            return [];
        }
        return aAttachments;
    }

    function OpenDetailUseCase() {
        UseCase.call(this, "OpenDetailUseCase");
    }

    OpenDetailUseCase.prototype = Object.create(UseCase.prototype);
    OpenDetailUseCase.prototype.constructor = OpenDetailUseCase;

    OpenDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oUiState = mCtx && mCtx.uiState;
        var sReadyAt = new Date().toISOString();

        if (CreateSentinel.isCreateId(sRootId)) {
            var oDraft = (oUiState && oUiState.get("selected", "/")) || {};
            return DetailAuthorizationSupport.fetchPermission(mCtx || {}, "", {
                activity: DetailAuthorizationSupport.OPERATIONS.CREATE
            }).then(function (oPermission) {
                if (!oPermission.allowed) {
                    return Result.fail({ message: "No permission to create checklist", code: "NO_CREATE_PERMISSION" }, DetailAuthorizationSupport.deniedActionEffects(oPermission, "detailCreatePermissionDenied", [
                        Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                            status: "denied",
                            ready: false,
                            readyAt: "",
                            error: "NO_CREATE_PERMISSION",
                            rootId: CreateSentinel.VALUE,
                            mode: "READ",
                            permissionKnown: true,
                            lockKnown: true
                        }),
                        Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                        Effects.modelPatch("selected", "/", {}),
                        Effects.modelPatch("snapshot", "/", {}),
                        Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false),
                        Effects.navigate("search", {}, true)
                    ]));
                }
                return Result.ok({ snapshot: oDraft || {} }, [
                    Effects.modelPatch("view", ViewPathContracts.ACCESS_STATE, {
                        denied: false,
                        rootId: CreateSentinel.VALUE,
                        userId: "",
                        canView: false,
                        canEdit: true,
                        canDelete: false,
                        reasonCode: "CREATE_DRAFT",
                        titleKey: "",
                        messageKey: "",
                        illustrationSrc: "assets/illustrations/detail-access-denied.svg"
                    }),
                    Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                        status: "ready",
                        ready: true,
                        readyAt: sReadyAt,
                        error: "",
                        rootId: CreateSentinel.VALUE,
                        mode: "CREATE",
                        permissionKnown: true,
                        lockKnown: true
                    }),
                    Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "CREATE"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                    Effects.modelPatch("snapshot", "/", oDraft || {}),
                    Effects.modelPatch("selected", "/", oDraft || {}),
                    Effects.modelPatch("selected", "/attachments", (oDraft && oDraft.attachments) || []),
                    Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false)
                ]);
            });
        }

        if (!sRootId || !oRepo || typeof oRepo.loadDetailSnapshot !== "function") {
            return Promise.resolve(Result.fail({ message: "Open detail input invalid", code: "INVALID_INPUT" }));
        }

        var oCacheValidation = mCtx && mCtx.cacheValidation;

        return DetailAuthorizationSupport.fetchPermission(mCtx || {}, sRootId, {
            activity: DetailAuthorizationSupport.OPERATIONS.DISPLAY
        }).then(function (oPermission) {
            var pValidation;
            if (!oPermission.allowed) {
                return Result.fail({ message: "No permission to open checklist", code: "NO_VIEW_PERMISSION" }, [
                    Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                        status: "denied",
                        ready: false,
                        readyAt: "",
                        error: "NO_VIEW_PERMISSION",
                        rootId: sRootId,
                        mode: "READ",
                        permissionKnown: true,
                        lockKnown: false
                    })
                ].concat(DetailAuthorizationSupport.openDeniedEffects(oPermission)));
            }
            pValidation = (oCacheValidation && typeof oCacheValidation.execute === "function")
                ? Promise.resolve(oCacheValidation.execute({ rootId: sRootId, toleranceMs: 5500 }, mCtx || {})).catch(function () { return null; })
                : Promise.resolve(null);
            return pValidation.then(function (oValidation) {
                var oValidationData = (oValidation && oValidation.ok && oValidation.data) ? oValidation.data : null;
                if (oValidationData && oValidationData.valid && oValidationData.snapshot) {
                    return {
                        snapshot: oValidationData.snapshot,
                        permission: oPermission
                    };
                }
                return Promise.resolve(oRepo.loadDetailSnapshot({ rootId: sRootId })).then(function (oSnapshot) {
                    var oCacheWrite = mCtx && mCtx.cacheWrite;
                    if (oCacheWrite && typeof oCacheWrite.execute === "function") {
                        return Promise.resolve(oCacheWrite.execute({ rootId: sRootId, snapshot: oSnapshot }, mCtx || {})).catch(function () {
                            return null;
                        }).then(function () {
                            return {
                                snapshot: oSnapshot,
                                permission: oPermission
                            };
                        });
                    }
                    return {
                        snapshot: oSnapshot,
                        permission: oPermission
                    };
                });
            });
        }).then(function (oResolved) {
            if (oResolved && oResolved.ok === false) {
                return oResolved;
            }
            var oSnapshot = oResolved && oResolved.snapshot;
            var oPermission = oResolved && oResolved.permission;
            var aLoadedAttachments = resolveLoadedAttachments(oUiState, sRootId);
            return Result.ok({ snapshot: oSnapshot || {} }, DetailAuthorizationSupport.contentAccessEffects(oPermission).concat([
                Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                    status: "ready",
                    ready: true,
                    readyAt: sReadyAt,
                    error: "",
                    rootId: sRootId,
                    mode: "READ",
                    permissionKnown: true,
                    lockKnown: true
                }),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "READ_ONLY"),
                Effects.modelPatch("snapshot", "/", oSnapshot || {}),
                Effects.modelPatch("selected", "/", oSnapshot || {}),
                Effects.modelPatch("selected", "/attachments", aLoadedAttachments),
                Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false)
            ]));
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                    status: "error",
                    ready: false,
                    readyAt: "",
                    error: String((oError && oError.message) || "detail_open_failed"),
                    rootId: sRootId,
                    mode: "READ",
                    permissionKnown: false,
                    lockKnown: false
                }),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false)
            ]);
        });
    };

    return OpenDetailUseCase;
});
