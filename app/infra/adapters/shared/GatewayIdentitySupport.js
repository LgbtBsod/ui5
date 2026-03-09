sap.ui.define([], function () {
    "use strict";

    function resolveUserName() {
        return "";
    }

    function withUserName(oPayload) {
        return Object.assign({}, oPayload || {});
    }

    return {
        resolveUserName: resolveUserName,
        withUserName: withUserName
    };
});
