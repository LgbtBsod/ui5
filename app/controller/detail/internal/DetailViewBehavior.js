sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailInfoCardFactory"
], function (DetailLayoutRuntime, DetailInfoCardFactory) {
    "use strict";

    return {
        applyLayoutState: DetailLayoutRuntime.applyLayoutState,
        buildInfoCard: DetailInfoCardFactory.buildInfoCard
    };
});
