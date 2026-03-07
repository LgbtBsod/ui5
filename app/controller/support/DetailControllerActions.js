sap.ui.define([
    "checklist/app/controller/support/DetailValidationSummarySupport",
    "checklist/app/controller/support/DetailActionConstants",
    "checklist/app/controller/support/DetailActionViewportSupport",
    "checklist/app/controller/support/DetailActionPinnedRailSupport",
    "checklist/app/controller/support/DetailActionDialogSupport",
    "checklist/app/controller/support/DetailChecklistCoreSupport",
    "checklist/app/controller/support/DetailChecklistStateActions",
    "checklist/app/controller/support/DetailChecklistRowActions",
    "checklist/app/controller/support/DetailAttachmentLocationActions"
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
            }
        }
    );
});
