sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiAssetPaths"
], function (UiAssetPaths) {
    "use strict";

    function createDefaultState(sDbKey) {
        var sCanonicalDbKey = String(sDbKey || "").trim();
        return {
            denied: false,
            dbKey: sCanonicalDbKey,
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
