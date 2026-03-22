sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ClientKeyGenerator",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (ClientKeyGenerator, WorkflowContracts) {
    "use strict";

    var CHECKLIST_STATUSES = WorkflowContracts.CHECKLIST_STATUSES;

    function nowParts() {
        var oNow = new Date();
        return {
            date: [
                String(oNow.getFullYear()),
                String(oNow.getMonth() + 1).padStart(2, "0"),
                String(oNow.getDate()).padStart(2, "0")
            ].join("-"),
            time: [String(oNow.getHours()).padStart(2, "0"), String(oNow.getMinutes()).padStart(2, "0")].join(":"),
            timezone: (Intl && Intl.DateTimeFormat && Intl.DateTimeFormat().resolvedOptions && Intl.DateTimeFormat().resolvedOptions().timeZone) || "UTC"
        };
    }

    function tempKey() {
        return ClientKeyGenerator.createHex32();
    }

    function rootNode(oSource, sRootKey) {
        var o = oSource || {};
        var sStatus = String(o.status || o.Status || CHECKLIST_STATUSES.DRAFT).toUpperCase() || CHECKLIST_STATUSES.DRAFT;
        return {
            id: sRootKey,
            Key: sRootKey,
            RootKey: sRootKey,
            client_row_id: sRootKey,
            status: sStatus,
            Status: sStatus,
            overall_result: null,
            version_number: 0,
            VersionNumber: 0,
            server_changed_on: "",
            ChangedOn: "",
            CreatedOn: "",
            RequestId: "",
            Id: ""
        };
    }

    function basicNode(oSource, sRootKey) {
        var oParts = nowParts();
        var o = oSource || {};
        var sBasicKey = tempKey();
        return Object.assign({}, o, {
            id: sBasicKey,
            Key: sBasicKey,
            RootKey: sRootKey,
            client_row_id: sBasicKey,
            date: o.date || oParts.date,
            time: o.time || oParts.time,
            timezone: o.timezone || oParts.timezone,
            equipment: o.equipment || "",
            OBSERVER_FULLNAME: o.OBSERVER_FULLNAME || "",
            OBSERVER_PERNER: o.OBSERVER_PERNER || "",
            OBSERVER_POSITION: o.OBSERVER_POSITION || "",
            OBSERVER_ORGUNIT: o.OBSERVER_ORGUNIT || "",
            OBSERVED_FULLNAME: o.OBSERVED_FULLNAME || "",
            OBSERVED_PERNER: o.OBSERVED_PERNER || "",
            OBSERVED_POSITION: o.OBSERVED_POSITION || "",
            OBSERVED_ORGUNIT: o.OBSERVED_ORGUNIT || "",
            LOCATION_NAME: o.LOCATION_NAME || "",
            LOCATION_TEXT: o.LOCATION_TEXT || "",
            LOCATION_KEY: o.LOCATION_KEY || "",
            LPC_TEXT: o.LPC_TEXT || "",
            PROF_TEXT: o.PROF_TEXT || "",
            LPC_KEY: o.LPC_KEY || "",
            PROF_KEY: o.PROF_KEY || "",
            CHECKS_NUMBER: o.CHECKS_NUMBER || "",
            CHECKS_NUMBER_TEXT: o.CHECKS_NUMBER_TEXT || "",
            BARRIERS_NUMBER: o.BARRIERS_NUMBER || "",
            BARRIERS_NUMBER_TEXT: o.BARRIERS_NUMBER_TEXT || ""
        });
    }

    function createEmptyDraft() {
        var sRootKey = tempKey();
        return {
            root: rootNode({}, sRootKey),
            basic: basicNode({}, sRootKey),
            checks: [],
            barriers: [],
            participants: [],
            attachments: [],
            meta: {
                aggChangedOn: "",
                versionNumber: 0,
                draftRootKey: sRootKey
            }
        };
    }

    return {
        createEmptyDraft: createEmptyDraft,
        createTempKey: tempKey
    };
});
