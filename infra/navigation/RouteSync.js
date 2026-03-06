sap.ui.define([], function () {
    "use strict";

    function normalizeLayout(sLayout) {
        var sValue = String(sLayout || "").trim();
        if (sValue === "MidColumnFullScreen") {
            return "MidColumnFullScreen";
        }
        if (sValue === "TwoColumnsMidExpanded" || sValue === "TwoColumnsBeginExpanded") {
            return "TwoColumnsMidExpanded";
        }
        if (sValue === "OneColumn") {
            return "OneColumn";
        }
        return null;
    }

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
        if ((sRoute === "detail" || sRoute === "detailLayout") && sArgId) {
            return sArgId;
        }
        return normalizeId(oStateModel.getProperty("/selectedId"));
    }

    function syncRouteState(oStateModel, sNextLayout, sRouteName, mArgs) {
        var sLayout;
        var sPrevLayout;
        var sNextSelectedId;
        var sPrevSelectedId;
        var bChanged = false;
        if (!oStateModel || typeof oStateModel.getProperty !== "function" || typeof oStateModel.setProperty !== "function") {
            return null;
        }
        sLayout = normalizeLayout(sNextLayout);
        if (!sLayout) {
            return null;
        }
        sPrevLayout = normalizeLayout(oStateModel.getProperty("/layout")) || "OneColumn";
        sPrevSelectedId = normalizeId(oStateModel.getProperty("/selectedId"));
        sNextSelectedId = resolveSelectedId(sLayout, sRouteName, mArgs, oStateModel);

        if (sPrevLayout !== sLayout) {
            oStateModel.setProperty("/layout", sLayout);
            bChanged = true;
        }
        if (sPrevSelectedId !== sNextSelectedId) {
            oStateModel.setProperty("/selectedId", sNextSelectedId);
            bChanged = true;
        }
        return bChanged ? {
            layout: sLayout,
            selectedId: sNextSelectedId
        } : null;
    }

    return {
        syncRouteState: syncRouteState
    };
});
