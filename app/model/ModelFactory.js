sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/model/StateSchema",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/AttachmentUploadPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutPersonalizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (JSONModel, StateSchema, AttachmentUploadPolicy, CloneUtil, LayoutPersonalizationRuntime, ThemeContracts, WorkflowContracts) {
    "use strict";

    function clone(v) {
        return CloneUtil.clone(v, {});
    }

    function createModel(vData) {
        return new JSONModel(vData);
    }

    function createTimers() {
        return clone(StateSchema.createTimers());
    }

    function createMasterDataDefaults() {
        return {
            dict: {},
            runtime: { timers: {}, requiredFields: [], uploadPolicy: AttachmentUploadPolicy.DEFAULT_UPLOAD_POLICY },
            persons: [],
            lpc: [],
            professions: [],
            checksNumbers: [],
            barriersNumbers: [],
            statuses: ["SUCCESS", "WARNING", "CRITICAL"],
            resultTypes: ["PASS", "FAIL"],
            types: [],
            timezones: [],
            attachmentTypes: []
        };
    }

    function createLayoutDefaults() {
        var oPersonalization = LayoutPersonalizationRuntime.readAll();
        return {
            smartFilter: { useCustomSmartFilters: true, variant: "default", fields: [] },
            smartTable: { useCustomSmartTable: true, variant: "default", columns: [], selectionMode: "MultiSelect" },
            personalization: {
                compactRows: !!oPersonalization.compactRows,
                showHints: oPersonalization.showHints !== false,
                infoCardLayout: Array.isArray(oPersonalization.infoCardLayout) ? oPersonalization.infoCardLayout : []
            }
        };
    }

    function createShellDefaults() {
        var oLayoutDefaults = createLayoutDefaults();

        return {
            animationEnabled: ThemeContracts.DEFAULTS.ANIMATION_ENABLED,
            compactDensity: false,
            invertedBlockScheme: false,
            themeMode: ThemeContracts.MODES.MORNING,
            layout: WorkflowContracts.LAYOUTS ? WorkflowContracts.LAYOUTS.ONE_COLUMN : "OneColumn",
            isPhoneViewport: false,
            isTabletViewport: false,
            viewportWidth: 0,
            busy: false,
            currentRootKey: "",
            sessionGuid: "",
            lock: { ok: false, reason: WorkflowContracts.REASONS.FREE, isKilled: false },
            timers: createTimers(),
            activity: { lastActiveAt: "", idleUntil: "" },
            smartFilter: oLayoutDefaults.smartFilter,
            smartTable: oLayoutDefaults.smartTable,
            personalization: oLayoutDefaults.personalization,
            shell: {
                eyebrow: "",
                productName: "",
                routeLabel: "",
                contextSubtitle: "",
                userLabel: "",
                userMeta: "",
                userSessionLabel: "",
                userSessionState: "Information",
                userActionVisible: false,
                userActionText: "",
                userActionIcon: "sap-icon://employee-lookup",
                userActionType: "Transparent",
                userActionKind: "",
                userActionHint: "",
                userActionPassiveText: "",
                userTooltip: "",
                userIcon: "sap-icon://employee",
                userSummaryText: "",
                userPermissions: [],
                userLoginLabel: "",
                userEnvironmentLabel: "",
                userRefreshBusy: false,
                showHints: oLayoutDefaults.personalization.showHints
            }
        };
    }

    return {
        createViewModel: function () {
            return createModel({
                root: {},
                basicInfo: {},
                checks: { items: [] },
                barriers: { items: [] },
                attachments: { items: [] },
                meta: { aggChangedOn: "" },
                sessionAttachments: [],
                showSessionAttachments: true,
                attachmentsExpanded: false,
                attachmentsLoaded: false,
                attachmentBusy: false,
                attachmentActionsEnabled: false,
                attachmentMetaEditable: false,
                attachmentDesktopColumnsVisible: true,
                attachmentActionsColumnWidth: "14rem",
                attachmentCategoryKey: ""
            });
        },

        createDetailModel: function () {
            return createModel({
                current: {},
                base: {}
            });
        },

        createMasterDataModel: function () {
            return createModel(createMasterDataDefaults());
        },

        createStateModel: function () {
            return createModel(StateSchema.createStateDefaults());
        },

        createShellModel: function () {
            return createModel(createShellDefaults());
        }
    };
});
