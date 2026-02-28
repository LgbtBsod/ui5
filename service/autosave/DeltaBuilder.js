sap.ui.define([], function () {
    "use strict";

    return {
        diff: function (viewModelData, cacheSnapshot) {
            if (JSON.stringify(viewModelData || {}) === JSON.stringify(cacheSnapshot || {})) {
                return [];
            }
            return [{ Entity: "ROOT", Key: (viewModelData && viewModelData.Key) || "", ParentKey: "", Fields: viewModelData || {} }];
        }
    };
});
