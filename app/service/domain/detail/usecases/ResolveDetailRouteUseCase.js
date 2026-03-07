sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/domain/detail/DetailAuthorizationSupport",
    "sap_ui5/util/CreateSentinel"
], function (UseCase, DetailAuthorizationSupport, CreateSentinel) {
    "use strict";

    function ResolveDetailRouteUseCase() {
        UseCase.call(this, "ResolveDetailRouteUseCase");
    }

    ResolveDetailRouteUseCase.prototype = Object.create(UseCase.prototype);
    ResolveDetailRouteUseCase.prototype.constructor = ResolveDetailRouteUseCase;

    ResolveDetailRouteUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = String((mInput && (mInput.rootId || mInput.id)) || "").trim();
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
