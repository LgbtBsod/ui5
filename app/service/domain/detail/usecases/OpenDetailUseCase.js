sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiAssetPaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (Result, Effects, DetailAuthorizationRuntime, ViewPathContracts, UseCaseValue, StatePaths, CreateSentinel, WorkflowContracts, UiAssetPaths, NavigationContracts, WorkflowRuntimeConstants, DetailPersistenceConstants, ModelPathContracts, CloneUtil, DetailSaveRuntime, ModelContracts, DetailUseCaseConstants, DetailMessageKeyConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var DETAIL_MODEL = MODELS.DETAIL;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_ACCESS_REASON_CODES = DetailUseCaseConstants.ACCESS_REASON_CODES;
    var DETAIL_CODES = DetailUseCaseConstants.CODES;
    var DETAIL_MESSAGE_KEYS = DetailMessageKeyConstants;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function cloneSnapshot(oSnapshot) {
        return CloneUtil.clone(oSnapshot || {}, {});
    }

    function resolveCanonicalRootId(oRepo, sRootId) {
        if (!oRepo || typeof oRepo.resolveRootId !== "function" || !sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(sRootId);
        }
        return Promise.resolve(oRepo.resolveRootId({ rootId: sRootId })).then(function (sResolvedRootId) {
            return String(sResolvedRootId || sRootId).trim() || sRootId;
        }).catch(function () {
            return sRootId;
        });
    }

    function resetTransientDetailIncidentEffects() {
        return [
            Effects.modelPatch(STATE_MODEL, "/isKilled", false),
            Effects.modelPatch(STATE_MODEL, "/hasConflict", false),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_LOCK_LOST_REASON, ""),
            Effects.modelPatch(STATE_MODEL, StatePaths.PENDING_NAVIGATION_INTENT, null),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
            Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE, {
                state: DetailPersistenceConstants.STATES.IDLE,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_IDLE,
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
            Effects.modelPatch(MODELS.SHELL, MODEL_PATHS.SHELL_LOCK, {
                ok: false,
                reason: WorkflowContracts.REASONS.FREE,
                isKilled: false
            }),
            Effects.modelPatch(STATE_MODEL, StatePaths.TAB_CONFLICT_STATE, {
                active: false,
                source: "",
                at: ""
            })
        ];
    }

    function resolveLoadedAttachments(oUiState, sRootId) {
        var sSelectedRootId = String((oUiState && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT_ID)) || "").trim();
        var bAttachmentsLoaded = !!(oUiState && oUiState.get(VIEW_MODEL, ViewPathContracts.ATTACHMENTS_LOADED));
        var aAttachments = (oUiState && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.ATTACHMENTS)) || [];
        if (!sRootId || sSelectedRootId !== sRootId || !bAttachmentsLoaded || !Array.isArray(aAttachments)) {
            return [];
        }
        return aAttachments;
    }

    function resolveEditableOpenState(oUiState, sRootId) {
        var sHydratedRootId = String((oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID)) || "").trim();
        var sActiveRootId = String((oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID)) || "").trim();
        var sEditMode = WorkflowContracts.normalizeEditMode(oUiState && oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        var sLockState = WorkflowContracts.normalizeLockState(oUiState && oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE));
        var bMatchesRoot = !!sRootId && (sHydratedRootId === sRootId || sActiveRootId === sRootId);
        var bEditable = bMatchesRoot && WorkflowContracts.isEditLocked(sEditMode, sLockState);

        return {
            editMode: bEditable ? WorkflowContracts.EDIT_MODES.EDIT : WorkflowContracts.EDIT_MODES.READ,
            lockState: bEditable ? WorkflowContracts.LOCK_STATES.EDIT_LOCKED : WorkflowContracts.LOCK_STATES.READ_ONLY,
            autosaveEnabled: bEditable
        };
    }

    function resolveSameRootSnapshot(oUiState, sRootId, sPath) {
        var oSnapshot = (oUiState && oUiState.get(DETAIL_MODEL, sPath)) || null;
        var sSnapshotRootId = String((oSnapshot && oSnapshot.root && oSnapshot.root.id) || "").trim();
        if (!sRootId || !sSnapshotRootId || sSnapshotRootId !== sRootId) {
            return null;
        }
        return oSnapshot;
    }

    function OpenDetailUseCase() {
        return {
            execute: execute
        };
    }

    function execute(mInput, mCtx) {
        var sRootId = UseCaseValue.rootId(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oUiState = mCtx && mCtx.uiState;
        var sReadyAt = new Date().toISOString();

        if (CreateSentinel.isCreateId(sRootId)) {
            var oDraft = (oUiState && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT)) || {};
            var oDraftSnapshot = cloneSnapshot(oDraft);
            var oDraftSelected = cloneSnapshot(oDraft);
            return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, "", {
                activity: DetailAuthorizationRuntime.OPERATIONS.CREATE
            }).then(function (oPermission) {
                if (!oPermission.allowed) {
                    return Result.fail({ message: "No permission to create checklist", code: DETAIL_CODES.NO_CREATE_PERMISSION }, DetailAuthorizationRuntime.deniedActionEffects(oPermission, DETAIL_MESSAGE_KEYS.DETAIL_CREATE_PERMISSION_DENIED, [
                        Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_DETAIL, {
                            status: WorkflowRuntimeConstants.READINESS_STATUS.DENIED,
                            ready: false,
                            readyAt: "",
                            error: DETAIL_CODES.NO_CREATE_PERMISSION,
                            rootId: CreateSentinel.VALUE,
                            mode: WorkflowContracts.EDIT_MODES.READ,
                            permissionKnown: true,
                            lockKnown: true
                        }),
                        Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                        Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT, {}),
                        Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE, {}),
                        Effects.modelPatch(VIEW_MODEL, ViewPathContracts.DETAIL_SKELETON_BUSY, false),
                        Effects.navigate(NavigationContracts.ROUTES.SEARCH, {}, true)
                    ]));
                }
                return Result.ok({ snapshot: oDraftSnapshot }, resetTransientDetailIncidentEffects().concat([
                    Effects.modelPatch(VIEW_MODEL, ViewPathContracts.ACCESS_STATE, {
                        denied: false,
                        rootId: CreateSentinel.VALUE,
                        userId: "",
                        canView: false,
                        canEdit: true,
                        canDelete: false,
                        reasonCode: DETAIL_ACCESS_REASON_CODES.CREATE_DRAFT,
                        titleKey: "",
                        messageKey: "",
                        illustrationSrc: UiAssetPaths.resolveDetailAccessDeniedIllustration()
                    }),
                    Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_DETAIL, {
                        status: WorkflowRuntimeConstants.READINESS_STATUS.READY,
                        ready: true,
                        readyAt: sReadyAt,
                        error: "",
                        rootId: CreateSentinel.VALUE,
                        mode: WorkflowContracts.EDIT_MODES.CREATE,
                        permissionKnown: true,
                        lockKnown: true
                    }),
                    Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.CREATE),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.IDLE),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                    Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE, oDraftSnapshot),
                    Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT, oDraftSelected),
                    Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ATTACHMENTS, (oDraftSelected && oDraftSelected.attachments) || []),
                    Effects.modelPatch(VIEW_MODEL, ViewPathContracts.SESSION_ATTACHMENTS, (oDraftSelected && oDraftSelected.attachments) || []),
                    Effects.modelPatch(VIEW_MODEL, ViewPathContracts.DETAIL_SKELETON_BUSY, false)
                ]));
            });
        }

        if (!sRootId || !oRepo || typeof oRepo.loadDetailSnapshot !== "function") {
            return Promise.resolve(Result.fail({ message: "Open detail input invalid", code: DETAIL_CODES.INVALID_INPUT }));
        }

        var oCacheValidation = mCtx && mCtx.cacheValidation;

        return resolveCanonicalRootId(oRepo, sRootId).then(function (sCanonicalRootId) {
            return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, sCanonicalRootId, {
                activity: DetailAuthorizationRuntime.OPERATIONS.DISPLAY
            }).then(function (oPermission) {
            var pValidation;
            if (!oPermission.allowed) {
                return Result.fail({ message: "No permission to open checklist", code: DETAIL_CODES.NO_VIEW_PERMISSION }, resetTransientDetailIncidentEffects().concat([
                    Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_DETAIL, {
                        status: WorkflowRuntimeConstants.READINESS_STATUS.DENIED,
                        ready: false,
                        readyAt: "",
                        error: DETAIL_CODES.NO_VIEW_PERMISSION,
                        rootId: sCanonicalRootId,
                        mode: WorkflowContracts.EDIT_MODES.READ,
                        permissionKnown: true,
                        lockKnown: false
                    })
                ]).concat(DetailAuthorizationRuntime.openDeniedEffects(oPermission)));
            }
            pValidation = (oCacheValidation && typeof oCacheValidation.execute === "function")
                ? Promise.resolve(oCacheValidation.execute({ rootId: sCanonicalRootId, toleranceMs: 5500 }, mCtx || {})).catch(function () { return null; })
                : Promise.resolve(null);
            return pValidation.then(function (oValidation) {
                var oValidationData = (oValidation && oValidation.ok && oValidation.data) ? oValidation.data : null;
                if (oValidationData && oValidationData.valid && oValidationData.snapshot) {
                    return {
                        snapshot: oValidationData.snapshot,
                        permission: oPermission,
                        rootId: sCanonicalRootId
                    };
                }
                return Promise.resolve(oRepo.loadDetailSnapshot({ rootId: sCanonicalRootId })).then(function (oSnapshot) {
                    var oCacheWrite = mCtx && mCtx.cacheWrite;
                    if (oCacheWrite && typeof oCacheWrite.execute === "function") {
                        return Promise.resolve(oCacheWrite.execute({ rootId: sCanonicalRootId, snapshot: oSnapshot }, mCtx || {})).catch(function () {
                            return null;
                        }).then(function () {
                            return {
                                snapshot: oSnapshot,
                                permission: oPermission,
                                rootId: sCanonicalRootId
                            };
                        });
                    }
                    return {
                        snapshot: oSnapshot,
                        permission: oPermission,
                        rootId: sCanonicalRootId
                    };
                });
            });
        });
        }).then(function (oResolved) {
            if (oResolved && oResolved.ok === false) {
                return oResolved;
            }
            var oSnapshot = oResolved && oResolved.snapshot;
            var oPermission = oResolved && oResolved.permission;
            var sCanonicalRootId = String((oResolved && oResolved.rootId) || sRootId).trim() || sRootId;
            var oCurrentSelected = resolveSameRootSnapshot(oUiState, sCanonicalRootId, DETAIL_MODEL_PATHS.ROOT);
            var oCurrentSnapshot = resolveSameRootSnapshot(oUiState, sCanonicalRootId, DETAIL_MODEL_PATHS.BASE);
            var aLoadedAttachments = resolveLoadedAttachments(oUiState, sCanonicalRootId);
            var aSnapshotAttachments = Array.isArray(oSnapshot && oSnapshot.attachments) ? oSnapshot.attachments : [];
            var aEffectiveAttachments = aLoadedAttachments.length ? aLoadedAttachments : aSnapshotAttachments;
            var oEditState = resolveEditableOpenState(oUiState, sCanonicalRootId);
            var oNormalizedSnapshot = DetailSaveRuntime.normalizeOverallResult(
                DetailSaveRuntime.preserveBasicFields(oSnapshot, oCurrentSelected, oCurrentSnapshot)
            );
            var oBaseSnapshot = cloneSnapshot(oNormalizedSnapshot);
            var oSelectedSnapshot = cloneSnapshot(oNormalizedSnapshot);
            return Result.ok({ snapshot: oBaseSnapshot }, resetTransientDetailIncidentEffects().concat(DetailAuthorizationRuntime.contentAccessEffects(oPermission)).concat([
                Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_DETAIL, {
                    status: WorkflowRuntimeConstants.READINESS_STATUS.READY,
                    ready: true,
                    readyAt: sReadyAt,
                    error: "",
                    rootId: sCanonicalRootId,
                    mode: oEditState.editMode,
                    permissionKnown: true,
                    lockKnown: true
                }),
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, sCanonicalRootId),
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.SELECTED_ID, sCanonicalRootId),
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, sCanonicalRootId),
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, oEditState.editMode),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, oEditState.lockState),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, oEditState.autosaveEnabled),
                Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE, oBaseSnapshot),
                Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT, oSelectedSnapshot),
                Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ATTACHMENTS, aEffectiveAttachments),
                Effects.modelPatch(VIEW_MODEL, ViewPathContracts.SESSION_ATTACHMENTS, aEffectiveAttachments),
                Effects.modelPatch(VIEW_MODEL, ViewPathContracts.DETAIL_SKELETON_BUSY, false)
            ]));
        }).catch(function (oError) {
            return Result.fail(oError, resetTransientDetailIncidentEffects().concat([
                Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_DETAIL, {
                    status: WorkflowRuntimeConstants.READINESS_STATUS.ERROR,
                    ready: false,
                    readyAt: "",
                    error: String((oError && oError.message) || "detail_open_failed"),
                    rootId: sRootId,
                    mode: WorkflowContracts.EDIT_MODES.READ,
                    permissionKnown: false,
                    lockKnown: false
                }),
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch(VIEW_MODEL, ViewPathContracts.DETAIL_SKELETON_BUSY, false)
            ]));
        });
    }

    return OpenDetailUseCase;
});
