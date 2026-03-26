sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DraftChecklistFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (DraftChecklistFactory, ModelPathContracts, ViewPathContracts, StatePaths, ControllerViewStateRuntime, ModelStateRuntime, CreateSentinel, NavigationContracts, WorkflowContracts, ModelContracts, OperationSourceContracts, DetailUseCaseConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var DETAIL_MODEL = MODELS.DETAIL;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;
    var APP_SOURCES = OperationSourceContracts.APP;

    function createMatchedStatePatch(sId, sRouteName, bCreate) {
        var mStatePatch = {};

        mStatePatch[ModelPathContracts.ACTIVE_OBJECT_ID] = bCreate ? CreateSentinel.VALUE : sId;
        mStatePatch[ModelPathContracts.SELECTED_ID] = bCreate ? CreateSentinel.VALUE : sId;
        mStatePatch[ModelPathContracts.CURRENT_ROUTE_NAME] = sRouteName;
        if (bCreate) {
            mStatePatch[StatePaths.WORKFLOW_DETAIL_EDIT_MODE] = WorkflowContracts.EDIT_MODES.CREATE;
            mStatePatch[StatePaths.WORKFLOW_DETAIL_LOCK_STATE] = WorkflowContracts.LOCK_STATES.IDLE;
            mStatePatch[ModelPathContracts.AUTOSAVE_ENABLED] = false;
            mStatePatch[ModelPathContracts.IS_DIRTY] = false;
        }
        return mStatePatch;
    }

    function createMatchedViewPatch(sId, bCreate, mHooks) {
        var mViewPatch = {};

        mViewPatch["/isEditMode"] = bCreate;
        mViewPatch["/isCreateMode"] = bCreate;
        mViewPatch["/hasPersistedObject"] = !bCreate && !CreateSentinel.isCreateId(sId);
        mViewPatch[ViewPathContracts.DETAIL_SKELETON_BUSY] = !bCreate;
        mViewPatch[ViewPathContracts.VALIDATION_SHOWN] = false;
        mViewPatch[ViewPathContracts.VALIDATION_MISSING] = {};
        mViewPatch["/deleteChecklistConfirmArmed"] = false;
        mViewPatch["/attachmentsExpanded"] = false;
        mViewPatch[ViewPathContracts.ATTACHMENTS_LOADED] = false;
        mViewPatch[ViewPathContracts.SESSION_ATTACHMENTS] = [];
        mViewPatch["/attachmentBusy"] = false;
        mViewPatch["/observerSuggestions"] = [];
        mViewPatch["/observedSuggestions"] = [];
        mViewPatch["/personSuggestHint"] = "";
        mViewPatch[ViewPathContracts.ACCESS_STATE] = mHooks.createAccessState(sId);
        return mViewPatch;
    }

    function buildValidationSummary() {
        return {
            hasErrors: false,
            missingPaths: [],
            missingKeys: [],
            source: APP_SOURCES.DETAIL_MATCHED,
            firstMissingPath: "",
            firstMissingKey: ""
        };
    }

    function isLayoutOnlyTransition(mInput) {
        return !mInput.bCreate &&
            mInput.sCurrentRootId === mInput.sId &&
            mInput.sSelectedRootId === mInput.sId &&
            NavigationContracts.isDetailRoute(mInput.sCurrentRouteName) &&
            NavigationContracts.isDetailRoute(mInput.sRouteName);
    }

    function prepareMatchedContext(oController, oEvent) {
        var mArgs = oEvent.getParameter("arguments") || {};
        var sRouteName = String((oEvent.getParameter("name") || NavigationContracts.ROUTES.DETAIL) || NavigationContracts.ROUTES.DETAIL).trim() || NavigationContracts.ROUTES.DETAIL;
        var sId = mArgs.id;
        var sLayoutArg = String(mArgs.layout || "").toLowerCase();
        var bCreate = CreateSentinel.isCreateId(sId);
        var sRouteLayout = sLayoutArg === "midcolumnfullscreen" ? NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN : NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED;
        var sCurrentRouteName = String(ModelStateRuntime.read(oController, STATE_MODEL, ModelPathContracts.CURRENT_ROUTE_NAME, NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        var sCurrentRootId = String(ModelStateRuntime.read(oController, STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, "") || "").trim();
        var sPostOpenHydratedRootId = String(ModelStateRuntime.read(oController, STATE_MODEL, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "") || "").trim();
        var oDetailModel = oController.getModel(DETAIL_MODEL);
        var oDetailData = (oDetailModel && oDetailModel.getData && oDetailModel.getData()) || {};
        var sSelectedRootId = String((oDetailData && oDetailData.current && oDetailData.current.root && oDetailData.current.root.id) || "").trim();

        return {
            bCreate: bCreate,
            oDetailModel: oDetailModel,
            sCurrentRootId: sCurrentRootId,
            sCurrentRouteName: sCurrentRouteName,
            sId: sId,
            sPostOpenHydratedRootId: sPostOpenHydratedRootId,
            sRouteLayout: sRouteLayout,
            sRouteName: sRouteName,
            sSelectedRootId: sSelectedRootId
        };
    }

    function applyMatchedState(oController, mContext, mHooks) {
        var mStatePatch = createMatchedStatePatch(mContext.sId, mContext.sRouteName, mContext.bCreate);

        if (isLayoutOnlyTransition(mContext)) {
            ModelStateRuntime.setMany(oController, STATE_MODEL, mStatePatch);
            mHooks.applyLayoutState(mContext.sRouteLayout, { syncRoute: false });
            return "layoutOnly";
        }

        ModelStateRuntime.setMany(oController, STATE_MODEL, mStatePatch);
        mHooks.applyLayoutState(mContext.sRouteLayout, { syncRoute: false });
        ControllerViewStateRuntime.setMany(oController, createMatchedViewPatch(mContext.sId, mContext.bCreate, mHooks));
        mHooks.writeInfoCards(ControllerViewStateRuntime.get(oController, "/infoCards", []));
        ModelStateRuntime.write(oController, STATE_MODEL, mHooks.validationSummaryPath, buildValidationSummary());
        mHooks.scheduleAttachmentDropZoneBind();
        return "full";
    }

    function openCreateDraft(oController, mContext, mHooks) {
        if (mContext.oDetailModel && mContext.oDetailModel.setData) {
            mContext.oDetailModel.setData({
                current: DraftChecklistFactory.createEmptyDraft(),
                base: {}
            });
            ModelStateRuntime.writeOnModel(mContext.oDetailModel, DETAIL_MODEL_PATHS.ATTACHMENTS, []);
        }
        return mHooks.openChecklist({ id: CreateSentinel.VALUE, rootId: CreateSentinel.VALUE });
    }

    return {
        prepareMatchedContext: prepareMatchedContext,
        applyMatchedState: applyMatchedState,
        openCreateDraft: openCreateDraft
    };
});
