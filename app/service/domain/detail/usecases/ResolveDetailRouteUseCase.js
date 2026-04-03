sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts"
], function (DetailAuthorizationRuntime, UseCaseValue, CreateSentinel, NavigationContracts) {
    "use strict";

    function resolveCanonicalDbKey(oRepo, sDbKey) {
        if (!oRepo || typeof oRepo.resolveDbKey !== "function" || !sDbKey || CreateSentinel.isCreateId(sDbKey)) {
            return Promise.resolve(sDbKey);
        }
        return Promise.resolve(oRepo.resolveDbKey({ dbKey: sDbKey })).then(function (sResolvedDbKey) {
            return String(sResolvedDbKey || sDbKey).trim() || sDbKey;
        }).catch(function () {
            return sDbKey;
        });
    }

    function ResolveDetailRouteUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

function execute(mInput, mCtx) {
        var sDbKey = UseCaseValue.dbKey(mInput);
        var sRouteName = String((mInput && mInput.routeName) || NavigationContracts.ROUTES.DETAIL).trim() || NavigationContracts.ROUTES.DETAIL;
        var mRouteArgs = (mInput && mInput.routeArgs) || {};
        var oRepo = mCtx && mCtx.repo;

        if (!sDbKey || CreateSentinel.isCreateId(sDbKey)) {
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

        return resolveCanonicalDbKey(oRepo, sDbKey).then(function (sCanonicalDbKey) {
            return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, sCanonicalDbKey, {
                activity: DetailAuthorizationRuntime.OPERATIONS.DISPLAY
            }).then(function (oPermission) {
            var bAllowed = !!(oPermission && oPermission.allowed);

            return {
                allowed: bAllowed,
                routeName: bAllowed ? sRouteName : NavigationContracts.ROUTES.DETAIL,
                routeArgs: bAllowed ? Object.assign({}, mRouteArgs, { id: sCanonicalDbKey }) : { id: sCanonicalDbKey },
                permission: oPermission || {}
            };
        });
        });
    }

    return ResolveDetailRouteUseCase;
});
