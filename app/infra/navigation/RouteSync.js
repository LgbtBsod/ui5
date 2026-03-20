sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (LayoutStateRuntime, ModelStateRuntime, NavigationContracts, ModelPathContracts) {
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
                ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, "") ||
                ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "")
            );
        }
        if (NavigationContracts.isDetailRoute(sRoute) && sArgId) {
            return sArgId;
        }
        return normalizeId(ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, ""));
    }

    function resolveActiveObjectId(sLayout, sRouteName, mArgs, oStateModel) {
        var sRoute = String(sRouteName || "");
        var sArgId = normalizeId(mArgs && mArgs.id);
        if (sLayout === NavigationContracts.LAYOUTS.ONE_COLUMN || sRoute === NavigationContracts.ROUTES.SEARCH) {
            return null;
        }
        if (NavigationContracts.isDetailRoute(sRoute) && sArgId) {
            return sArgId;
        }
        return normalizeId(
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "") ||
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, "")
        );
    }

    function syncRouteState(oStateModel, sNextLayout, sRouteName, mArgs) {
        var sLayout;
        var sPrevLayout;
        var sPrevRouteName;
        var sNextSelectedId;
        var sNextActiveObjectId;
        var sPrevSelectedId;
        var sPrevActiveObjectId;
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
        sPrevSelectedId = normalizeId(ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, ""));
        sPrevActiveObjectId = normalizeId(ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, ""));
        sPrevRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        sNextSelectedId = resolveSelectedId(sLayout, sRouteName, mArgs, oStateModel);
        sNextActiveObjectId = resolveActiveObjectId(sLayout, sRouteName, mArgs, oStateModel);
        sNextRouteName = String(sRouteName || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;

        if (sPrevRouteName !== sNextRouteName) {
            ModelStateRuntime.writeOnModel(oStateModel, "/currentRouteName", sNextRouteName);
            bChanged = true;
        }
        if (sPrevSelectedId !== sNextSelectedId) {
            ModelStateRuntime.writeOnModel(oStateModel, ModelPathContracts.SELECTED_ID, sNextSelectedId);
            bChanged = true;
        }
        if (sPrevActiveObjectId !== sNextActiveObjectId) {
            ModelStateRuntime.writeOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, sNextActiveObjectId);
            bChanged = true;
        }
        if (sPrevLayout !== sLayout) {
            ModelStateRuntime.writeOnModel(oStateModel, "/layout", sLayout);
            bChanged = true;
        }
        return bChanged ? {
            activeObjectId: sNextActiveObjectId,
            layout: sLayout,
            selectedId: sNextSelectedId,
            currentRouteName: sNextRouteName
        } : null;
    }

    return {
        syncRouteState: syncRouteState
    };
});
