sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (LayoutStateRuntime, ModelStateRuntime) {
    "use strict";

    function normalizeId(vId) {
        var sId = String(vId || "").trim();
        return sId || null;
    }

    function resolveSelectedId(sLayout, sRouteName, mArgs, oStateModel) {
        var sRoute = String(sRouteName || "");
        var sArgId = normalizeId(mArgs && mArgs.id);
        if (sLayout === "OneColumn" || sRoute === "search") {
            return null;
        }
        if (sRoute === "analytics") {
            return normalizeId(
                ModelStateRuntime.readOnModel(oStateModel, "/selectedId", "") ||
                ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "")
            );
        }
        if ((sRoute === "detail" || sRoute === "detailLayout") && sArgId) {
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
        sPrevLayout = LayoutStateRuntime.readLayout(oStateModel, "OneColumn");
        sPrevSelectedId = normalizeId(ModelStateRuntime.readOnModel(oStateModel, "/selectedId", ""));
        sPrevRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", "search") || "search").trim() || "search";
        sNextSelectedId = resolveSelectedId(sLayout, sRouteName, mArgs, oStateModel);
        sNextRouteName = String(sRouteName || "search").trim() || "search";

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
