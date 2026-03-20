sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants"
], function (NavigationContracts) {
    "use strict";

    return {
        layout: NavigationContracts.LAYOUTS.ONE_COLUMN,
        currentRouteName: "search",
        selectedId: null,
        sessionId: null,
        tabSessionId: null,
        activeObjectId: null,
        copySourceId: null,
        navGuardBypass: false,
        analyticsNavReturn: {
            hash: "",
            rootId: "",
            restoreEdit: false
        },
        analyticsReturnRestoreEdit: null,
        detailAccessGuard: {
            rootId: "",
            userId: "",
            canView: true,
            canEdit: false,
            canDelete: false,
            reasonCode: "AUTHORIZED",
            message: "",
            checkedAt: ""
        }
    };
});
