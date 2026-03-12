sap.ui.define([], function () {
    "use strict";

    return {
        layout: "OneColumn",
        currentRouteName: "search",
        selectedId: null,
        sessionId: null,
        tabSessionId: null,
        activeObjectId: null,
        copySourceId: null,
        navGuardBypass: false,
        analyticsNavReturn: {
            routeName: "search",
            routeArgs: {},
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
