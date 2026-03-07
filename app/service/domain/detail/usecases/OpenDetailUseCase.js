sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/service/domain/detail/DetailAuthorizationSupport",
    "checklist/app/service/domain/shared/UseCaseInputUtils",
    "checklist/app/service/domain/shared/StatePaths",
    "checklist/app/util/CreateSentinel"
], function (UseCase, Result, Effects, DetailAuthorizationSupport, UseCaseInputUtils, StatePaths, CreateSentinel) {
    "use strict";

    function OpenDetailUseCase() {
        UseCase.call(this, "OpenDetailUseCase");
    }

    OpenDetailUseCase.prototype = Object.create(UseCase.prototype);
    OpenDetailUseCase.prototype.constructor = OpenDetailUseCase;

    OpenDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oUiState = mCtx && mCtx.uiState;

        if (CreateSentinel.isCreateId(sRootId)) {
            var oDraft = (oUiState && oUiState.get("selected", "/")) || {};
            return Promise.resolve(Result.ok({ snapshot: oDraft || {} }, [
                Effects.modelPatch("view", "/accessState", {
                    denied: false,
                    rootId: CreateSentinel.VALUE,
                    userId: "",
                    canView: true,
                    canEdit: true,
                    canDelete: false,
                    reasonCode: "CREATE_DRAFT",
                    titleKey: "",
                    messageKey: "",
                    illustrationSrc: "assets/illustrations/detail-access-denied.svg"
                }),
                Effects.modelPatch("state", StatePaths.UI_BUSY_GLOBAL, false),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "CREATE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                Effects.modelPatch("uiState", "/_detailSnapshot", oDraft || {}),
                Effects.modelPatch("uiState", "/_detailCurrent", oDraft || {}),
                Effects.modelPatch("selected", "/", oDraft || {}),
                Effects.modelPatch("selected", "/attachments", (oDraft && oDraft.attachments) || []),
                Effects.modelPatch("view", "/detailSkeletonBusy", false)
            ]));
        }

        if (!sRootId || !oRepo || typeof oRepo.loadDetailSnapshot !== "function") {
            return Promise.resolve(Result.fail({ message: "Open detail input invalid", code: "INVALID_INPUT" }));
        }

        var oCacheValidation = mCtx && mCtx.cacheValidation;
        var pValidation = (oCacheValidation && typeof oCacheValidation.execute === "function")
            ? Promise.resolve(oCacheValidation.execute({ rootId: sRootId, toleranceMs: 5500 }, mCtx || {})).catch(function () { return null; })
            : Promise.resolve(null);
        var pPermission = DetailAuthorizationSupport.fetchPermission(mCtx || {}, sRootId);

        return Promise.all([pValidation, pPermission]).then(function (aResolved) {
            var oValidation = aResolved[0];
            var oPermission = aResolved[1];
            var oValidationData = (oValidation && oValidation.ok && oValidation.data) ? oValidation.data : null;
            if (!oPermission.canView) {
                return Result.fail({ message: "No permission to open checklist", code: "NO_VIEW_PERMISSION" }, DetailAuthorizationSupport.openDeniedEffects(oPermission));
            }
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
        }).then(function (oResolved) {
            if (oResolved && oResolved.ok === false) {
                return oResolved;
            }
            var oSnapshot = oResolved && oResolved.snapshot;
            var oPermission = oResolved && oResolved.permission;
            return Result.ok({ snapshot: oSnapshot || {} }, DetailAuthorizationSupport.contentAccessEffects(oPermission).concat([
                Effects.modelPatch("state", StatePaths.UI_BUSY_GLOBAL, false),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ"),
                Effects.modelPatch("uiState", "/_detailSnapshot", oSnapshot || {}),
                Effects.modelPatch("uiState", "/_detailCurrent", oSnapshot || {}),
                Effects.modelPatch("selected", "/", oSnapshot || {}),
                Effects.modelPatch("selected", "/attachments", []),
                Effects.modelPatch("view", "/detailSkeletonBusy", false)
            ]));
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                Effects.modelPatch("view", "/detailSkeletonBusy", false)
            ]);
        });
    };

    return OpenDetailUseCase;
});