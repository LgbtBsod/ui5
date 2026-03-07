sap.ui.define([], function () {
    "use strict";

    function resolveUserName(mDeps) {
        return "";
    }

    function withUserName(oPayload, mDeps) {
        return Object.assign({}, oPayload || {});
    }

    return {
        resolveUserName: resolveUserName,
        withUserName: withUserName
    };
});
