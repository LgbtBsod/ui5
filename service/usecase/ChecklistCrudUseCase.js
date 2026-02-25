sap.ui.define([
    "sap_ui5/service/backend/BackendAdapter"
], function (BackendAdapter) {
    "use strict";

    return {
        getChecklistRoot: function (sId) {
            return BackendAdapter.getChecklistRoot(sId);
        },

        getChecklistRows: function (sId, sSection, mPaging) {
            if (sSection === "barriers") {
                return BackendAdapter.getChecklistBarriers(sId, mPaging);
            }
            return BackendAdapter.getChecklistChecks(sId, mPaging);
        },

        saveChecklist: function (sId, oPayload, bCreateMode, mOptions) {
            if (bCreateMode) {
                return BackendAdapter.createCheckList(oPayload || {});
            }
            return BackendAdapter.updateCheckList(sId, oPayload || {}, mOptions || {});
        },

        forceUpdateChecklist: function (sId, oPayload) {
            return BackendAdapter.updateCheckList(sId, oPayload || {}, { force: true });
        },

        getChecklistCollection: function () {
            return BackendAdapter.getCheckLists();
        }
    };
});
