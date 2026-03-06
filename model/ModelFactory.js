sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "sap_ui5/model/StateSchema",
    "sap_ui5/util/AttachmentUploadPolicy",
    "sap_ui5/service/framework/LayoutPersonalizationRuntime"
], function (JSONModel, StateSchema, AttachmentUploadPolicy, LayoutPersonalizationRuntime) {
    "use strict";

    function clone(v) {
        return JSON.parse(JSON.stringify(v));
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
            smartTable: { useCustomSmartTable: true, variant: "default", columns: [], selectionMode: "SingleSelectMaster" },
            personalization: {
                compactRows: !!oPersonalization.compactRows,
                showHints: oPersonalization.showHints !== false,
                infoCardLayout: Array.isArray(oPersonalization.infoCardLayout) ? oPersonalization.infoCardLayout : []
            }
        };
    }

    return {
        createDataModel: function () {
            return createModel({ checkLists: [], visibleCheckLists: [], selectedChecklist: null });
        },

        createUiStateModel: function () {
            return createModel({
                mode: "READ",
                busy: false,
                currentRootKey: "",
                sessionGuid: "",
                lock: { ok: false, reason: "FREE", isKilled: false },
                timers: createTimers(),
                activity: { lastActiveAt: "", idleUntil: "" }
            });
        },

        createViewModel: function () {
            return createModel({
                root: {}, basicInfo: {}, checks: { items: [] },
                barriers: { items: [] }, attachments: { items: [] }, meta: { aggChangedOn: "" }
            });
        },

        createCacheModel: function () {
            return createModel({ byRootKey: {}, pristineSnapshot: null, keyMapping: {}, lastServerState: null });
        },

        createMasterDataModel: function () {
            return createModel(createMasterDataDefaults());
        },

        createHierarchyModel: function () {
            return createModel({ byDate: {} });
        },

        createStateModel: function () {
            return createModel(StateSchema.createStateDefaults());
        },

        createLayoutModel: function () {
            return createModel(createLayoutDefaults());
        },

        createMplModel: function () {
            return createModel({ locations: [] });
        },

        createEnvModel: function () {
            return createModel({ source: "gateway", loadedAt: "", variables: {}, timers: createTimers() });
        }
    };
});
