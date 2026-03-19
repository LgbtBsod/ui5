sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/UiAssetPaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts"
], function (UseCase, Result, Effects, DetailAuthorizationRuntime, ViewPathContracts, UseCaseValue, StatePaths, CreateSentinel, WorkflowContracts, UiAssetPaths, NavigationContracts) {
    "use strict";

    function resetTransientDetailIncidentEffects() {
        return [
            Effects.modelPatch("state", "/isKilled", false),
            Effects.modelPatch("state", "/hasConflict", false),
            Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_LOST_REASON, ""),
            Effects.modelPatch("state", StatePaths.PENDING_NAVIGATION_INTENT, null),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
            Effects.modelPatch("state", StatePaths.PERSISTENCE, {
                state: "idle",
                messageKey: "persistenceIdle",
                lastSavedAt: null,
                lastSaveError: null,
                taxonomy: "",
                currentWriteRequestId: "",
                isManualSaveInFlight: false,
                isAutoSaveInFlight: false,
                hasValidLock: false,
                lockOwnerSessionMatches: false,
                lastLockRefreshAt: null,
                nextHeartbeatAt: null
            }),
            Effects.modelPatch("uiState", "/lock", {
                ok: false,
                reason: "FREE",
                isKilled: false
            }),
            Effects.modelPatch("state", StatePaths.TAB_CONFLICT_STATE, {
                active: false,
                source: "",
                at: ""
            })
        ];
    }

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
        var sRootId = UseCaseValue.rootId(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oUiState = mCtx && mCtx.uiState;
        var sReadyAt = new Date().toISOString();

        if (CreateSentinel.isCreateId(sRootId)) {
            var oDraft = (oUiState && oUiState.get("selected", "/")) || {};
        return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, "", {
            activity: DetailAuthorizationRuntime.OPERATIONS.CREATE
            }).then(function (oPermission) {
                if (!oPermission.allowed) {
            return Result.fail({ message: "No permission to create checklist", code: "NO_CREATE_PERMISSION" }, DetailAuthorizationRuntime.deniedActionEffects(oPermission, "detailCreatePermissionDenied", [
                        Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                            status: "denied",
                            ready: false,
                            readyAt: "",
                            error: "NO_CREATE_PERMISSION",
                            rootId: CreateSentinel.VALUE,
                            mode: WorkflowContracts.EDIT_MODES.READ,
                            permissionKnown: true,
                            lockKnown: true
                        }),
                        Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                        Effects.modelPatch("selected", "/", {}),
                        Effects.modelPatch("snapshot", "/", {}),
                        Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false),
                        Effects.navigate(NavigationContracts.ROUTES.SEARCH, {}, true)
                    ]));
                }
                return Result.ok({ snapshot: oDraft || {} }, resetTransientDetailIncidentEffects().concat([
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
                        illustrationSrc: UiAssetPaths.resolveDetailAccessDeniedIllustration()
                    }),
                    Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                        status: "ready",
                        ready: true,
                        readyAt: sReadyAt,
                        error: "",
                        rootId: CreateSentinel.VALUE,
                        mode: WorkflowContracts.EDIT_MODES.CREATE,
                        permissionKnown: true,
                        lockKnown: true
                    }),
                    Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.CREATE),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.IDLE),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                    Effects.modelPatch("snapshot", "/", oDraft || {}),
                    Effects.modelPatch("selected", "/", oDraft || {}),
                    Effects.modelPatch("selected", "/attachments", (oDraft && oDraft.attachments) || []),
                    Effects.modelPatch("view", ViewPathContracts.SESSION_ATTACHMENTS, (oDraft && oDraft.attachments) || []),
                    Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false)
                ]));
            });
        }

        if (!sRootId || !oRepo || typeof oRepo.loadDetailSnapshot !== "function") {
            return Promise.resolve(Result.fail({ message: "Open detail input invalid", code: "INVALID_INPUT" }));
        }

        var oCacheValidation = mCtx && mCtx.cacheValidation;

        return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, sRootId, {
            activity: DetailAuthorizationRuntime.OPERATIONS.DISPLAY
        }).then(function (oPermission) {
            var pValidation;
            if (!oPermission.allowed) {
                return Result.fail({ message: "No permission to open checklist", code: "NO_VIEW_PERMISSION" }, resetTransientDetailIncidentEffects().concat([
                    Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                        status: "denied",
                        ready: false,
                        readyAt: "",
                        error: "NO_VIEW_PERMISSION",
                        rootId: sRootId,
                        mode: WorkflowContracts.EDIT_MODES.READ,
                        permissionKnown: true,
                        lockKnown: false
                    })
                ]).concat(DetailAuthorizationRuntime.openDeniedEffects(oPermission)));
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
            return Result.ok({ snapshot: oSnapshot || {} }, resetTransientDetailIncidentEffects().concat(DetailAuthorizationRuntime.contentAccessEffects(oPermission)).concat([
                Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                    status: "ready",
                    ready: true,
                    readyAt: sReadyAt,
                    error: "",
                    rootId: sRootId,
                    mode: WorkflowContracts.EDIT_MODES.READ,
                    permissionKnown: true,
                    lockKnown: true
                }),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY),
                Effects.modelPatch("snapshot", "/", oSnapshot || {}),
                Effects.modelPatch("selected", "/", oSnapshot || {}),
                Effects.modelPatch("selected", "/attachments", aLoadedAttachments),
                Effects.modelPatch("view", ViewPathContracts.SESSION_ATTACHMENTS, aLoadedAttachments),
                Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false)
            ])));
        }).catch(function (oError) {
            return Result.fail(oError, resetTransientDetailIncidentEffects().concat([
                Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                    status: "error",
                    ready: false,
                    readyAt: "",
                    error: String((oError && oError.message) || "detail_open_failed"),
                    rootId: sRootId,
                    mode: WorkflowContracts.EDIT_MODES.READ,
                    permissionKnown: false,
                    lockKnown: false
                }),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false)
            ]));
        });
    };

    return OpenDetailUseCase;
});
