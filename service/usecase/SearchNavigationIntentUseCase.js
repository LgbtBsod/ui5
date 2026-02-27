sap.ui.define([
    "sap_ui5/service/usecase/SearchActionUseCase"
], function (SearchActionUseCase) {
    "use strict";

    function normalizeLayout(sLayout) {
        var sValue = String(sLayout || "").trim();
        var aAllowed = [
            "OneColumn",
            "TwoColumnsBeginExpanded",
            "TwoColumnsMidExpanded",
            "ThreeColumnsMidExpanded",
            "ThreeColumnsEndExpanded"
        ];
        return aAllowed.indexOf(sValue) >= 0 ? sValue : "TwoColumnsMidExpanded";
    }

    function resolveDetailLayout(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;
        var sPreferred = oStateModel && typeof oStateModel.getProperty === "function"
            ? (oStateModel.getProperty("/preferredDetailLayout") || oStateModel.getProperty("/layout"))
            : "";
        return normalizeLayout(sPreferred);
    }

    function buildCreateIntent(mArgs) {
        return {
            objectAction: "CREATE",
            layout: resolveDetailLayout(mArgs),
            route: "detailLayout",
            routeParams: { id: "__create", layout: resolveDetailLayout(mArgs) }
        };
    }

    function buildCopyIntent(mArgs) {
        var sId = (mArgs && mArgs.selectedId) || "";
        if (!sId) {
            return null;
        }
        return {
            objectAction: "COPY",
            layout: resolveDetailLayout(mArgs),
            route: "detailLayout",
            routeParams: { id: sId, layout: resolveDetailLayout(mArgs) }
        };
    }

    function buildOpenDetailIntent(mArgs) {
        var sId = (mArgs && mArgs.id) || "";
        if (!sId) {
            return null;
        }
        return {
            objectAction: "",
            layout: resolveDetailLayout(mArgs),
            route: "detailLayout",
            routeParams: { id: sId, layout: resolveDetailLayout(mArgs) }
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
