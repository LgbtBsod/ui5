sap.ui.define([
    "sap_ui5/controller/support/DetailDialogSupport",
    "sap_ui5/controller/support/DetailViewSupport",
    "sap_ui5/controller/support/DetailAccessViewState",
    "sap_ui5/controller/support/DetailActionConstants",
    "sap_ui5/controller/support/DetailCommandPolicy",
    "sap_ui5/controller/support/DetailInfoCardLayoutSupport",
    "sap_ui5/util/CreateSentinel",
    "sap_ui5/controller/support/ControllerModelWriteSupport"
], function (DetailDialogSupport, DetailViewSupport, DetailAccessViewState, DetailActionConstants, DetailCommandPolicy, DetailInfoCardLayoutSupport, CreateSentinel, ControllerModelWriteSupport) {
    "use strict";

    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    return {
        ensureEffectDialog: function (sId) {
            return DetailDialogSupport.ensureEffectDialog(this, sId);
        },

        infoCardFactory: function (sId, oContext) {
            return DetailViewSupport.buildInfoCard(this, sId, oContext);
        },

        _focusInfoCardByKey: function (sKey) {
            DetailInfoCardLayoutSupport.focusCardByKey(this, sKey);
        },

        _isEditMode: function () {
            return ControllerModelWriteSupport.get(this, "state", "/mode") === "EDIT";
        },

        _showToast: function (sTextKey) {
            return this.applyUseCaseEffects({
                effects: [{
                    type: "toast",
                    textKey: sTextKey,
                    level: "info"
                }]
            });
        },

        _applyLayoutState: function (sLayout, mOptions) {
            DetailViewSupport.applyLayoutState(this, sLayout, mOptions);
        },

        _onDetailMatched: function (oEvent) {
            var mArgs = oEvent.getParameter("arguments") || {};
            var sRouteName = String((oEvent.getParameter("name") || (mArgs.layout ? "detailLayout" : "detail")) || "detail").trim() || "detail";
            var sId = mArgs.id;
            var sLayoutArg = String(mArgs.layout || "").toLowerCase();
            var bCreate = CreateSentinel.isCreateId(sId);
            var sRouteLayout = sLayoutArg === "midcolumnfullscreen" ? "MidColumnFullScreen" : "TwoColumnsMidExpanded";
            var mStatePatch = {
                "/activeObjectId": bCreate ? CreateSentinel.VALUE : sId,
                "/selectedId": bCreate ? CreateSentinel.VALUE : sId,
                "/currentRouteName": sRouteName
            };
            var mViewPatch = {
                "/detailSkeletonBusy": !bCreate,
                "/validationShown": false,
                "/validationMissing": {},
                "/deleteChecklistConfirmArmed": false,
                "/observerSuggestions": [],
                "/observedSuggestions": [],
                "/personSuggestHint": "",
                "/accessState": DetailAccessViewState.createDefaultState(sId)
            };
            var sPostOpenHydratedRootId = String(ControllerModelWriteSupport.get(this, "state", "/postOpenHydratedRootId") || "").trim();
            var oSelected = this.getModel("selected");
            var oSelectedData = (oSelected && oSelected.getData && oSelected.getData()) || {};
            var sSelectedRootId = String((oSelectedData && oSelectedData.root && oSelectedData.root.id) || "").trim();

            if (bCreate) {
                mStatePatch["/mode"] = "CREATE";
                mStatePatch["/lockOperationState"] = "IDLE";
                mStatePatch["/autosaveEnabled"] = false;
                mStatePatch["/isDirty"] = false;
            }
            ControllerModelWriteSupport.setMany(this, "state", mStatePatch);
            this._applyLayoutState(sRouteLayout, { syncRoute: false });
            ControllerModelWriteSupport.setMany(this, "view", mViewPatch);
            DetailInfoCardLayoutSupport.writeCards(this, ControllerModelWriteSupport.get(this, "view", "/infoCards", []));
            ControllerModelWriteSupport.set(this, "state", STATE_PATHS.VALIDATION_SUMMARY, {
                hasErrors: false,
                missingPaths: [],
                missingKeys: [],
                source: "detailMatched",
                firstMissingPath: "",
                firstMissingKey: ""
            });
            this._scheduleAttachmentDropZoneBind();

            if (bCreate) {
                if (oSelected && oSelected.setData) {
                    oSelected.setData(DetailDialogSupport.createEmptyDraft());
                }
                DetailCommandPolicy.open(this, { id: CreateSentinel.VALUE, rootId: CreateSentinel.VALUE });
                return;
            }
            if (sPostOpenHydratedRootId && sPostOpenHydratedRootId === sId && sSelectedRootId === sId) {
                ControllerModelWriteSupport.set(this, "state", "/postOpenHydratedRootId", "");
                ControllerModelWriteSupport.set(this, "view", "/detailSkeletonBusy", false);
                return;
            }
            ControllerModelWriteSupport.set(this, "state", "/activeObjectId", sId);
            DetailCommandPolicy.open(this, { id: sId, rootId: sId }).then(function (oResult) {
                var oAccessState;
                if (!oResult || oResult.ok !== false || !oResult.error || oResult.error.code !== "NO_VIEW_PERMISSION") {
                    return oResult;
                }
                oAccessState = ControllerModelWriteSupport.get(this, "view", "/accessState", {}) || {};
                ControllerModelWriteSupport.set(this, "state", "/detailAccessGuard", {
                    rootId: String(oAccessState.rootId || sId || "").trim(),
                    userId: String(oAccessState.userId || "").trim(),
                    canView: false,
                    canEdit: !!oAccessState.canEdit,
                    canDelete: !!oAccessState.canDelete,
                    reasonCode: String(oAccessState.reasonCode || "NO_VIEW_PERMISSION").trim(),
                    message: String(oAccessState.message || "").trim(),
                    checkedAt: new Date().toISOString()
                });
                this.getRouter().navTo("accessDenied", { id: sId }, false);
                return oResult;
            }.bind(this));
        },

        _currentRootId: function () {
            return ControllerModelWriteSupport.get(this, "state", "/activeObjectId")
                || ControllerModelWriteSupport.get(this, "state", "/selectedId")
                || "";
        },

        _resolveRowInput: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var oCursor = oSource;
            var oCtx;
            var oRow;
            var sPath = "";
            while (oCursor) {
                if (oCursor.getBindingContext) {
                    oCtx = oCursor.getBindingContext("selected") || oCursor.getBindingContext();
                    if (oCtx && oCtx.getPath) {
                        sPath = String(oCtx.getPath() || "");
                        oRow = oCtx.getObject && oCtx.getObject();
                        if (sPath) {
                            break;
                        }
                    }
                }
                oCursor = oCursor.getParent && oCursor.getParent();
            }
            return {
                rowPath: sPath,
                rowId: String((oRow && (oRow.client_row_id || oRow.Key || oRow.id)) || "").trim()
            };
        }
    };
});
