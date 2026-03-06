sap.ui.define([
    "sap_ui5/util/DraftChecklistFactory"
], function (DraftChecklistFactory) {
    "use strict";

    return {
        createEmptyDraft: function () {
            return DraftChecklistFactory.createEmptyDraft();
        },
        createTempKey: function () {
            return DraftChecklistFactory.createTempKey();
        }
    };
});
