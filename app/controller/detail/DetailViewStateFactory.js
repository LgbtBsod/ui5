sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/DetailRuntimeContracts"
], function (ControllerTextRuntime, DetailAccessViewState, DetailInfoCardLayoutRuntime, DetailRuntimeContracts) {
    "use strict";

    var INFO_CARD_KEYS = DetailRuntimeContracts.INFO_CARD_KEYS;
    var INFO_CARD_TEXT_KEYS = DetailRuntimeContracts.INFO_CARD_TEXT_KEYS;
    var INFO_CARD_TEXT_FALLBACKS = DetailRuntimeContracts.INFO_CARD_TEXT_FALLBACKS;
    var VIEW_DEFAULTS = DetailRuntimeContracts.VIEW_DEFAULTS;

    function resolveText(oController, sKey, sFallback) {
        return ControllerTextRuntime.getText(oController, sKey, [], sFallback || sKey);
    }

    function create(oController) {
        return {
            detailSkeletonBusy: false,
            attachmentBusy: false,
            attachmentsExpanded: false,
            attachmentsLoaded: false,
            sessionAttachments: [],
            checksBusy: false,
            barriersBusy: false,
            checksExpandedBusy: false,
            barriersExpandedBusy: false,
            locationVhBusy: false,
            attachmentCategoryKey: VIEW_DEFAULTS.ATTACHMENT_CATEGORY_KEY,
            observerSuggestions: [],
            observedSuggestions: [],
            observerInputValue: "",
            observedInputValue: "",
            personSuggestHint: "",
            locationVhHint: "",
            narrowDetailViewport: false,
            deleteChecklistConfirmArmed: false,
            detailSections: {
                checks: {
                    kind: "check",
                    titleKey: "checksTitle",
                    emptyTitleKey: "detailEmptyChecksTitle",
                    emptyTextKey: "detailEmptyChecksText",
                    actionIcon: "sap-icon://task",
                    emptyIcon: "sap-icon://complete"
                },
                barriers: {
                    kind: "barrier",
                    titleKey: "barriersTitle",
                    emptyTitleKey: "detailEmptyBarriersTitle",
                    emptyTextKey: "detailEmptyBarriersText",
                    actionIcon: "sap-icon://quality-issue",
                    emptyIcon: "sap-icon://alert"
                }
            },
            accessState: DetailAccessViewState.createDefaultState(""),
            validationShown: false,
            validationMissing: {},
            infoCards: DetailInfoCardLayoutRuntime.resolveCards(oController, [
                { key: INFO_CARD_KEYS.DATETIME, title: resolveText(oController, INFO_CARD_TEXT_KEYS.DATETIME, INFO_CARD_TEXT_FALLBACKS.DATETIME), pinned: true },
                { key: INFO_CARD_KEYS.LOCATION, title: resolveText(oController, INFO_CARD_TEXT_KEYS.LOCATION, INFO_CARD_TEXT_FALLBACKS.LOCATION), pinned: true },
                { key: INFO_CARD_KEYS.EQUIPMENT, title: resolveText(oController, INFO_CARD_TEXT_KEYS.EQUIPMENT, INFO_CARD_TEXT_FALLBACKS.EQUIPMENT), pinned: true },
                { key: INFO_CARD_KEYS.OBSERVER, title: resolveText(oController, INFO_CARD_TEXT_KEYS.OBSERVER, INFO_CARD_TEXT_FALLBACKS.OBSERVER), pinned: true },
                { key: INFO_CARD_KEYS.OBSERVED, title: resolveText(oController, INFO_CARD_TEXT_KEYS.OBSERVED, INFO_CARD_TEXT_FALLBACKS.OBSERVED), pinned: true },
                { key: INFO_CARD_KEYS.PROFESSION, title: resolveText(oController, INFO_CARD_TEXT_KEYS.PROFESSION, INFO_CARD_TEXT_FALLBACKS.PROFESSION), pinned: false },
                { key: INFO_CARD_KEYS.LPC, title: resolveText(oController, INFO_CARD_TEXT_KEYS.LPC, INFO_CARD_TEXT_FALLBACKS.LPC), pinned: false }
            ])
        };
    }

    return {
        create: create
    };
});
