sap.ui.define([
    "sap_ui5/controller/support/DetailValidationSummarySupport",
    "sap_ui5/controller/support/DetailActionConstants",
    "sap_ui5/service/framework/FacadeCommandContract",
    "sap_ui5/controller/support/DetailActionViewportSupport",
    "sap_ui5/controller/support/DetailActionPinnedRailSupport",
    "sap_ui5/controller/support/DetailActionDialogSupport",
    "sap_ui5/controller/support/DetailChecklistCoreSupport",
    "sap_ui5/controller/support/DetailChecklistStateActions",
    "sap_ui5/controller/support/DetailChecklistRowActions",
    "sap_ui5/controller/support/DetailAttachmentLocationActions"
], function (
    DetailValidationSummarySupport,
    DetailActionConstants,
    FacadeCommandContract,
    DetailActionViewportSupport,
    DetailActionPinnedRailSupport,
    DetailActionDialogSupport,
    DetailChecklistCoreSupport,
    DetailChecklistStateActions,
    DetailChecklistRowActions,
    DetailAttachmentLocationActions
) {
    "use strict";

    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    return Object.assign({},
        DetailActionViewportSupport,
        DetailActionPinnedRailSupport,
        DetailActionDialogSupport,
        DetailChecklistCoreSupport,
        DetailChecklistStateActions,
        DetailChecklistRowActions,
        DetailAttachmentLocationActions,
        {
            _computeValidationSummary: function () {
                return DetailValidationSummarySupport.compute(this);
            },

            _recomputeValidationSummary: function (sSource, bShowValidation) {
                return DetailValidationSummarySupport.recompute(this, sSource, bShowValidation, STATE_PATHS);
            },

            _focusFirstInvalidField: function () {
                return DetailValidationSummarySupport.focusFirstInvalidField(this, STATE_PATHS);
            },

            _onSelectedChecklistChanged: function (oEvent) {
                DetailValidationSummarySupport.onSelectedChecklistChanged(this, oEvent, STATE_PATHS);
            },

            _run: function (sMethod, mInput) {
                var sCommand = FacadeCommandContract.normalizeDetailMethod(sMethod);
                var oPayload = FacadeCommandContract.normalizeDetailPayload(sCommand, mInput);
                return this.executeFacadeMethod(this._facade, sCommand, oPayload, this._ctx());
            }
        }
    );
});
