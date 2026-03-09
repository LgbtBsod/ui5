sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseInputUtils",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (UseCase, DetailAuthorizationSupport, UseCaseInputUtils, CreateSentinel) {
    "use strict";

    function ResolveDetailRouteUseCase() {
        UseCase.call(this, "ResolveDetailRouteUseCase");
    }

    ResolveDetailRouteUseCase.prototype = Object.create(UseCase.prototype);
    ResolveDetailRouteUseCase.prototype.constructor = ResolveDetailRouteUseCase;

    ResolveDetailRouteUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var sRouteName = String((mInput && mInput.routeName) || "detail").trim() || "detail";
        var mRouteArgs = (mInput && mInput.routeArgs) || {};

        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve({
                allowed: true,
                routeName: sRouteName,
                routeArgs: mRouteArgs,
                permission: {
                    rootId: sRootId,
                    canView: true,
                    canEdit: true,
                    canDelete: false,
                    reasonCode: "CREATE_DRAFT",
                    message: ""
                }
            });
        }

        return DetailAuthorizationSupport.fetchPermission(mCtx || {}, sRootId).then(function (oPermission) {
            var bAllowed = !!(oPermission && oPermission.canView);

            return {
                allowed: bAllowed,
                routeName: bAllowed ? sRouteName : "accessDenied",
                routeArgs: bAllowed ? mRouteArgs : { id: sRootId },
                permission: oPermission || {}
            };
        });
    };

    return ResolveDetailRouteUseCase;
});
