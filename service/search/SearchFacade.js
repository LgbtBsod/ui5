sap.ui.define([
    "sap_ui5/util/search/SearchFilterBuilder",
    "sap_ui5/util/search/TableOrderExtractor"
], function (SearchFilterBuilder, TableOrderExtractor) {
    "use strict";

    function _smartRefs(oView) {
        return {
            sfb: oView.byId("searchSmartFilterBar"),
            st: oView.byId("searchSmartTable")
        };
    }

    var _ctx = null;

    return {
        init: function (view, models, gatewayClient) {
            _ctx = { view: view, models: models || {}, gatewayClient: gatewayClient };
        },

        applySearch: function () {
            if (!_ctx) { return; }
            var oRefs = _smartRefs(_ctx.view);
            if (oRefs.st && oRefs.st.rebindTable) {
                oRefs.st.rebindTable();
            }
        },

        onFailSegmentChanged: function (key) {
            if (!_ctx || !_ctx.models.state) { return; }
            _ctx.models.state.setProperty("/search/failSegment", String(key || "ALL").toUpperCase());
            this.applySearch();
        },

        onBarrierFailSegmentChanged: function (key) {
            if (!_ctx || !_ctx.models.state) { return; }
            _ctx.models.state.setProperty("/search/barrierFailSegment", String(key || "ALL").toUpperCase());
            this.applySearch();
        },

        getDisplayedRootKeysInOrder: function () {
            if (!_ctx) { return []; }
            var oRefs = _smartRefs(_ctx.view);
            var oTable = oRefs.st && oRefs.st.getTable && oRefs.st.getTable();
            return TableOrderExtractor.getDisplayedRootKeysInOrder(oTable);
        },

        buildMergedFilter: function (aSfbFilters, sMode, sSegment, sBarrierSegment) {
            return SearchFilterBuilder.mergeSmartFilterBarFilters(
                aSfbFilters,
                [
                    SearchFilterBuilder.buildFailSegmentFilter(sSegment),
                    SearchFilterBuilder.buildBarrierFailSegmentFilter(sBarrierSegment)
                ],
                sMode
            );
        }
    };
});
