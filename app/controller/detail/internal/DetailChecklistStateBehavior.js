sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailStateActionRuntime",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (DetailActionConstants, DetailCommandPolicy, ModelStateRuntime, RootIdRuntime, DetailStateActionRuntime, CreateSentinel, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    return {
        onToggleEdit: function (oEvent) {
            return DetailStateActionRuntime.toggleEdit(this, oEvent, {
                enterEdit: function (mInput) {
                    return DetailCommandPolicy.enterEdit(this, RootIdRuntime.withCurrentRootId(this, mInput));
                }.bind(this)
            });
        },

        onSaveDetail: function () {
            return DetailStateActionRuntime.save(this, {
                saveDetail: function () {
                    return DetailCommandPolicy.save(this, RootIdRuntime.withCurrentRootId(this));
                }.bind(this)
            }, {
                saveInFlightPath: STATE_PATHS.SAVE_IN_FLIGHT
            });
        },

        onCloseDetail: function () {
            return DetailStateActionRuntime.close(this, {
                closeDetail: function (mInput) {
                    return DetailCommandPolicy.close(this, RootIdRuntime.withCurrentRootId(this, mInput));
                }.bind(this),
                saveDetail: function () {
                    return DetailStateActionRuntime.save(this, {
                        saveDetail: function () {
                            return DetailCommandPolicy.save(this, RootIdRuntime.withCurrentRootId(this));
                        }.bind(this)
                    }, {
                        saveInFlightPath: STATE_PATHS.SAVE_IN_FLIGHT
                    });
                }.bind(this)
            });
        },

        onArmDeleteChecklist: function () {
            return DetailStateActionRuntime.armDelete(this);
        },

        onConfirmDeleteChecklist: function () {
            return DetailStateActionRuntime.confirmDelete(this, {
                deleteChecklist: function () {
                    return DetailCommandPolicy.deleteChecklist(this, RootIdRuntime.withCurrentRootId(this));
                }.bind(this)
            });
        },

        onDeleteChecklist: function () {
            return this.onArmDeleteChecklist();
        },

        onCopyDetailLink: function () {
            return DetailStateActionRuntime.copyDetailLink(this, {
                isCreateId: CreateSentinel.isCreateId,
                showToast: this._showToast.bind(this)
            });
        },

        onToggleDetailFullscreen: function () {
            return DetailStateActionRuntime.toggleFullscreen(this, {
                applyLayoutState: this._applyLayoutState.bind(this)
            });
        },

        onJumpToDetailSection: function (oEvent) {
            return DetailStateActionRuntime.jumpToDetailSection(this, oEvent);
        },

        onCancelEditFromDetail: function () {
            DetailCommandPolicy.discardChanges(this, RootIdRuntime.withCurrentRootId(this));
        },

        onValidateChecklist: function () {
            this._recomputeValidationSummary("manualValidate", true);
            return DetailCommandPolicy.validate(this, RootIdRuntime.withCurrentRootId(this)).then(function (oResult) {
                this._recomputeValidationSummary("validateResult", true);
                if (ModelStateRuntime.read(this, STATE_MODEL, STATE_PATHS.VALIDATION_SUMMARY + "/hasErrors", false)) {
                    this._focusFirstInvalidField();
                }
                return oResult;
            }.bind(this));
        },

        onFocusFirstInvalid: function () {
            this._recomputeValidationSummary("summaryFocus", true);
            this._focusFirstInvalidField();
        },

        onChangeChecklistStatus: function (oEvent) {
            var oSrc = oEvent && oEvent.getSource && oEvent.getSource();
            var sStatus = String((oSrc && (oSrc.data("status") || oSrc.data("targetStatus"))) || "").trim().toUpperCase();
            var oSelectedModel = this.getModel && this.getModel(ModelContracts.MODELS.SELECTED);
            var sCurrentStatus = String(ModelStateRuntime.readOnModel(oSelectedModel, "/root/status", "") || "").trim().toUpperCase();

            if (!sStatus) {
                return Promise.resolve(false);
            }

            return DetailCommandPolicy.validate(this, RootIdRuntime.withCurrentRootId(this)).then(function () {
                this._recomputeValidationSummary("statusChange", true);
                if (ModelStateRuntime.read(this, STATE_MODEL, STATE_PATHS.VALIDATION_SUMMARY + "/hasErrors", false)) {
                    this._focusFirstInvalidField();
                    return false;
                }
                if (sCurrentStatus !== sStatus) {
                    ModelStateRuntime.writeOnModel(oSelectedModel, "/root/status", sStatus);
                    ModelStateRuntime.writeOnModel(oSelectedModel, "/root/Status", sStatus);
                    ModelStateRuntime.write(this, STATE_MODEL, "/isDirty", true);
                }
                return DetailStateActionRuntime.save(this, {
                    saveDetail: function () {
                        return DetailCommandPolicy.save(this, RootIdRuntime.withCurrentRootId(this));
                    }.bind(this)
                }, {
                    saveInFlightPath: STATE_PATHS.SAVE_IN_FLIGHT
                });
            }.bind(this));
        }
    };
});
