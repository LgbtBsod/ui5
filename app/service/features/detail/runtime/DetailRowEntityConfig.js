sap.ui.define([], function () {
    "use strict";

    var ENTITY_CONFIG = Object.freeze({
        check: Object.freeze({
            key: "check",
            kind: "check",
            rowsPath: "/checks",
            numberField: "ChecksNum",
            rowBusyPath: "/checksBusy",
            dialogBusyPath: "/checksExpandedBusy",
            dialogId: "checksExpanded",
            labelKeys: Object.freeze({
                number: "checksNumberLabel",
                text: "checkLabel",
                status: "statusLabel",
                comment: "commentLabel",
                actions: "rowActionsLabel"
            }),
            titleKey: "checksTitle",
            emptyTitleKey: "detailEmptyChecksTitle",
            emptyTextKey: "detailEmptyChecksText",
            actionIcon: "sap-icon://task",
            emptyIcon: "sap-icon://complete",
            deleteActionKind: "check",
            resultSwitchClass: "detailResultSwitch detailChecksResultSwitch",
            desktopVisibleRowCount: 7,
            desktopTableClass: "flatEditorGridTable detailGridTable",
            dialogDesktopTableClass: "dialogDataTable detailGridTable",
            mobileTableClass: "flatEditorTable",
            dialogMobileTableClass: "flatEditorTable dialogDataTable",
            noDataKey: "noChecksData"
        }),
        barrier: Object.freeze({
            key: "barrier",
            kind: "barrier",
            rowsPath: "/barriers",
            numberField: "BarriersNum",
            rowBusyPath: "/barriersBusy",
            dialogBusyPath: "/barriersExpandedBusy",
            dialogId: "barriersExpanded",
            labelKeys: Object.freeze({
                number: "barriersNumberLabel",
                text: "barrierLabel",
                status: "statusLabel",
                comment: "commentLabel",
                actions: "rowActionsLabel"
            }),
            titleKey: "barriersTitle",
            emptyTitleKey: "detailEmptyBarriersTitle",
            emptyTextKey: "detailEmptyBarriersText",
            actionIcon: "sap-icon://quality-issue",
            emptyIcon: "sap-icon://alert",
            deleteActionKind: "barrier",
            resultSwitchClass: "detailResultSwitch detailBarriersResultSwitch",
            desktopVisibleRowCount: 8,
            desktopTableClass: "flatEditorGridTable detailGridTable detailBarriersGridTable",
            dialogDesktopTableClass: "dialogDataTable detailGridTable",
            mobileTableClass: "flatEditorTable",
            dialogMobileTableClass: "flatEditorTable dialogDataTable",
            noDataKey: "noBarriersData"
        })
    });

    function get(sEntity) {
        return ENTITY_CONFIG[String(sEntity || "").trim()] || ENTITY_CONFIG.check;
    }

    return Object.freeze({
        all: ENTITY_CONFIG,
        get: get
    });
});
