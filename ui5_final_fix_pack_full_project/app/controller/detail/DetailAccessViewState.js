sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/UiAssetPaths"
], function (UiAssetPaths) {
    "use strict";

    function createDefaultState(sRootId) {
        return {
            denied: false,
            rootId: String(sRootId || "").trim(),
            userId: "",
            canView: true,
            canEdit: true,
            canDelete: true,
            reasonCode: "AUTHORIZED",
            titleKey: "",
            messageKey: "",
            illustrationSrc: UiAssetPaths.resolveDetailAccessDeniedIllustration()
        };
    }

    return {
        createDefaultState: createDefaultState
    };
});
