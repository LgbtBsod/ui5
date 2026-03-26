sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowEntityConfig",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (ControllerTextRuntime, DetailAccessViewState, DetailInfoCardLayoutRuntime, DetailRuntimeContracts, DetailRowEntityConfig, WorkflowContracts) {
    "use strict";

    /* Этот блок фиксирует contracts для построения view-model detail-экрана.
     * Результат: фабрика собирает состояние из одних и тех же канонических ключей. */
    var INFO_CARD_KEYS = DetailRuntimeContracts.INFO_CARD_KEYS;
    var INFO_CARD_TEXT_KEYS = DetailRuntimeContracts.INFO_CARD_TEXT_KEYS;
    var VIEW_DEFAULTS = DetailRuntimeContracts.VIEW_DEFAULTS;

    /* Этот блок берет локализованный текст для view state.
     * Результат: фабрика не размазывает прямой доступ к bundle по всему объекту состояния. */
    function resolveText(oController, sKey, sFallback) {
        return ControllerTextRuntime.getText(oController, sKey, [], sFallback || sKey);
    }

    /* Этот блок создает базовое состояние секции checks/barriers.
     * Результат: обе секции строятся по одной модели и не расходятся по структуре. */
    function createSectionState(oConfig) {
        return {
            kind: oConfig.kind,
            titleKey: oConfig.titleKey,
            emptyTitleKey: oConfig.emptyTitleKey,
            emptyTextKey: oConfig.emptyTextKey,
            actionIcon: oConfig.actionIcon,
            emptyIcon: oConfig.emptyIcon,
            tableSpec: {
                kind: oConfig.kind,
                rowsPath: oConfig.rowsPath,
                numberField: oConfig.numberField,
                labelKeys: oConfig.labelKeys,
                deleteActionKind: oConfig.deleteActionKind,
                resultSwitchClass: oConfig.resultSwitchClass,
                desktopVisibleRowCount: oConfig.desktopVisibleRowCount,
                desktopTableClass: oConfig.desktopTableClass,
                mobileTableClass: oConfig.mobileTableClass,
                noDataKey: oConfig.noDataKey,
                ignoreNarrowViewport: false,
                expanded: false
            }
        };
    }

    /* Этот блок создает состояние expanded-dialog для табличных секций.
     * Результат: полноэкранный режим использует тот же owner-contract, что и обычная секция. */
    function createExpandedDialogState(oConfig) {
        return {
            kind: oConfig.kind,
            titleKey: oConfig.kind === "barrier" ? "barriersExpandedTitle" : "checksExpandedTitle",
            tableSpec: {
                kind: oConfig.kind,
                rowsPath: oConfig.rowsPath,
                numberField: oConfig.numberField,
                labelKeys: oConfig.labelKeys,
                deleteActionKind: oConfig.deleteActionKind,
                resultSwitchClass: oConfig.resultSwitchClass,
                desktopVisibleRowCount: 8,
                desktopTableClass: oConfig.dialogDesktopTableClass,
                mobileTableClass: oConfig.dialogMobileTableClass,
                noDataKey: oConfig.noDataKey,
                ignoreNarrowViewport: true,
                expanded: true
            }
        };
    }

    /* Этот блок собирает полный начальный view state detail-экрана.
     * Результат: контроллер получает один согласованный объект для runtime-флагов, секций и карточек. */
    function create(oController) {
        var oCheckConfig = DetailRowEntityConfig.get("check");
        var oBarrierConfig = DetailRowEntityConfig.get("barrier");
        return {
            /* D-03: computed properties â€” kept in sync by bindStateValidationModel
             * in DetailControllerBehavior whenever editMode changes.
             * Initial values = READ mode on open. */
            isEditMode: false,
            isCreateMode: false,
            hasPersistedObject: false,
            detailSkeletonBusy: false,
            attachmentBusy: false,
            attachmentsExpanded: false,
            attachmentsLoaded: false,
            sessionAttachments: [],
            attachmentActionsEnabled: false,
            attachmentMetaEditable: false,
            showSessionAttachments: true,
            attachmentDesktopColumnsVisible: true,
            attachmentActionsColumnWidth: "14rem",
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
            statusTargets: WorkflowContracts.CHECKLIST_STATUSES,
            personSuggestHint: "",
            locationVhHint: "",
            narrowDetailViewport: false,
            deleteChecklistConfirmArmed: false,
            detailSections: {
                checks: createSectionState(oCheckConfig),
                barriers: createSectionState(oBarrierConfig)
            },
            detailExpandedDialogs: {
                checks: createExpandedDialogState(oCheckConfig),
                barriers: createExpandedDialogState(oBarrierConfig)
            },
            activeExpandedDialog: createExpandedDialogState(oCheckConfig),
            accessState: DetailAccessViewState.createDefaultState(""),
            validationShown: false,
            validationMissing: {},
            infoCards: DetailInfoCardLayoutRuntime.resolveCards(oController, [
                { key: INFO_CARD_KEYS.DATETIME, title: resolveText(oController, INFO_CARD_TEXT_KEYS.DATETIME), pinned: true },
                { key: INFO_CARD_KEYS.LOCATION, title: resolveText(oController, INFO_CARD_TEXT_KEYS.LOCATION), pinned: true },
                { key: INFO_CARD_KEYS.EQUIPMENT, title: resolveText(oController, INFO_CARD_TEXT_KEYS.EQUIPMENT), pinned: true },
                { key: INFO_CARD_KEYS.OBSERVER, title: resolveText(oController, INFO_CARD_TEXT_KEYS.OBSERVER), pinned: true },
                { key: INFO_CARD_KEYS.OBSERVED, title: resolveText(oController, INFO_CARD_TEXT_KEYS.OBSERVED), pinned: true },
                { key: INFO_CARD_KEYS.PROFESSION, title: resolveText(oController, INFO_CARD_TEXT_KEYS.PROFESSION), pinned: false },
                { key: INFO_CARD_KEYS.LPC, title: resolveText(oController, INFO_CARD_TEXT_KEYS.LPC), pinned: false }
            ])
        };
    }

    return {
        create: create
    };
});
