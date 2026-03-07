sap.ui.define([], function () {
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
            illustrationSrc: "assets/illustrations/detail-access-denied.svg"
        };
    }

    return {
        createDefaultState: createDefaultState
    };
});
