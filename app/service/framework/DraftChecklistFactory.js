sap.ui.define([
    "checklist/app/util/DraftChecklistFactory"
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
