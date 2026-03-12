sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/contracts/NavigationContracts"
], function (LayoutStateRuntime, ModelStateRuntime, NavigationContracts) {
    "use strict";

    function normalizeId(vId) {
        var sId = String(vId || "").trim();
        return sId || null;
    }

    function resolveSelectedId(sLayout, sRouteName, mArgs, oStateModel) {
        var sRoute = String(sRouteName || "");
        var sArgId = normalizeId(mArgs && mArgs.id);
        if (sLayout === NavigationContracts.LAYOUTS.ONE_COLUMN || sRoute === NavigationContracts.ROUTES.SEARCH) {
            return null;
        }
        if (sRoute === NavigationContracts.ROUTES.ANALYTICS) {
            return normalizeId(
                ModelStateRuntime.readOnModel(oStateModel, "/selectedId", "") ||
                ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "")
            );
        }
        if (NavigationContracts.isDetailRoute(sRoute) && sArgId) {
            return sArgId;
        }
        return normalizeId(ModelStateRuntime.readOnModel(oStateModel, "/selectedId", ""));
    }

    function syncRouteState(oStateModel, sNextLayout, sRouteName, mArgs) {
        var sLayout;
        var sPrevLayout;
        var sPrevRouteName;
        var sNextSelectedId;
        var sPrevSelectedId;
        var sNextRouteName;
        var bChanged = false;
        if (!oStateModel || typeof oStateModel.getProperty !== "function" || typeof oStateModel.setProperty !== "function") {
            return null;
        }
        sLayout = LayoutStateRuntime.normalizeLayout(sNextLayout);
        if (!sLayout) {
            return null;
        }
        sPrevLayout = LayoutStateRuntime.readLayout(oStateModel, NavigationContracts.LAYOUTS.ONE_COLUMN);
        sPrevSelectedId = normalizeId(ModelStateRuntime.readOnModel(oStateModel, "/selectedId", ""));
        sPrevRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        sNextSelectedId = resolveSelectedId(sLayout, sRouteName, mArgs, oStateModel);
        sNextRouteName = String(sRouteName || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;

        if (sPrevRouteName !== sNextRouteName) {
            ModelStateRuntime.writeOnModel(oStateModel, "/currentRouteName", sNextRouteName);
            bChanged = true;
        }
        if (sPrevSelectedId !== sNextSelectedId) {
            ModelStateRuntime.writeOnModel(oStateModel, "/selectedId", sNextSelectedId);
            bChanged = true;
        }
        if (sPrevLayout !== sLayout) {
            ModelStateRuntime.writeOnModel(oStateModel, "/layout", sLayout);
            bChanged = true;
        }
        return bChanged ? {
            layout: sLayout,
            selectedId: sNextSelectedId,
            currentRouteName: sNextRouteName
        } : null;
    }

    return {
        syncRouteState: syncRouteState
    };
});
