sap.ui.define([
    "sap_ui5/service/usecase/SearchActionUseCase"
], function (SearchActionUseCase) {
    "use strict";

    function buildCreateIntent() {
        return {
            objectAction: "CREATE",
            layout: "TwoColumnsMidExpanded",
            route: "detail",
            routeParams: { id: "__create" }
        };
    }

    function buildCopyIntent(mArgs) {
        var sId = (mArgs && mArgs.selectedId) || "";
        if (!sId) {
            return null;
        }
        return {
            objectAction: "COPY",
            layout: "TwoColumnsMidExpanded",
            route: "detail",
            routeParams: { id: sId }
        };
    }

    function buildOpenDetailIntent(mArgs) {
        var sId = (mArgs && mArgs.id) || "";
        if (!sId) {
            return null;
        }
        return {
            objectAction: "",
            layout: "TwoColumnsMidExpanded",
            route: "detail",
            routeParams: { id: sId }
        };
    }

    function resolveSelectedId(mArgs) {
        var oSelectedModel = mArgs && mArgs.selectedModel;
        var oSelected = oSelectedModel && typeof oSelectedModel.getData === "function"
            ? (oSelectedModel.getData() || {})
            : {};
        return SearchActionUseCase.extractSelectedChecklistId(oSelected);
    }

    function applyIntent(mArgs) {
        var mIntent = mArgs && mArgs.intent;
        var oStateModel = mArgs && mArgs.stateModel;
        var fnNavTo = mArgs && mArgs.navTo;
        if (!mIntent || !oStateModel || typeof oStateModel.setProperty !== "function" || typeof fnNavTo !== "function") {
            return false;
        }

        oStateModel.setProperty("/layout", mIntent.layout || "TwoColumnsMidExpanded");
        if (mIntent.objectAction) {
            oStateModel.setProperty("/objectAction", mIntent.objectAction);
        }
        fnNavTo(mIntent.route, mIntent.routeParams || {});
        return true;
    }

    return {
        buildCreateIntent: buildCreateIntent,
        buildCopyIntent: buildCopyIntent,
        buildOpenDetailIntent: buildOpenDetailIntent,
        resolveSelectedId: resolveSelectedId,
        applyIntent: applyIntent
    };
});
