sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts"
], function (UseCase, DetailAuthorizationRuntime, UseCaseValue, CreateSentinel, NavigationContracts) {
    "use strict";

    function ResolveDetailRouteUseCase() {
        UseCase.call(this, "ResolveDetailRouteUseCase");
    }

    ResolveDetailRouteUseCase.prototype = Object.create(UseCase.prototype);
    ResolveDetailRouteUseCase.prototype.constructor = ResolveDetailRouteUseCase;

    ResolveDetailRouteUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = UseCaseValue.rootId(mInput);
        var sRouteName = String((mInput && mInput.routeName) || NavigationContracts.ROUTES.DETAIL).trim() || NavigationContracts.ROUTES.DETAIL;
        var mRouteArgs = (mInput && mInput.routeArgs) || {};

        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, "", {
                activity: DetailAuthorizationRuntime.OPERATIONS.CREATE
            }).then(function (oPermission) {
                return {
                    allowed: !!(oPermission && oPermission.allowed),
                    routeName: oPermission && oPermission.allowed ? sRouteName : NavigationContracts.ROUTES.SEARCH,
                    routeArgs: oPermission && oPermission.allowed ? mRouteArgs : {},
                    permission: oPermission || {}
                };
            });
        }

        return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, sRootId, {
            activity: DetailAuthorizationRuntime.OPERATIONS.DISPLAY
        }).then(function (oPermission) {
            var bAllowed = !!(oPermission && oPermission.allowed);

            return {
                allowed: bAllowed,
                routeName: bAllowed ? sRouteName : NavigationContracts.ROUTES.DETAIL,
                routeArgs: bAllowed ? mRouteArgs : { id: sRootId },
                permission: oPermission || {}
            };
        });
    };

    return ResolveDetailRouteUseCase;
});
