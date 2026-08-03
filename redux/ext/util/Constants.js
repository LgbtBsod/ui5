sap.ui.define([], function () {
        "use strict";

        // CDS-модель: CheckRoots/CheckBasics/CheckItems/Barriers, см. abap/README.md.
        return {
                ENTITY_SETS: {
                        LOCATIONS: "Locations",
                        LOCATION_HIERARCHY: "LocationHierarchy",
                        CHECK_TYPES: "CheckTypes",
                        BARRIER_TYPES: "BarrierTypes",
                        CHECK_RESULTS: "CheckResults",
                        TIME_ZONES: "TimeZones",
                        PERSONS: "Persons"
                },

                NAV_PROPERTIES: {
                        BASIC: "to_Basic",
                        CHECK_ROWS: "to_Checks",
                        BARRIERS: "to_Barriers"
                },

                FIELDS: {
                        // CheckItem и Barrier используют одинаковые имена Code/Text —
                        // сущность/EntitySet различает смысл, не имя поля.
                        CODE: "Code",
                        RAW_TEXT: "RawText",
                        TEXT: "Text",
                        RESULT: "Result",
                        RESULT_TEXT: "ResultText",
                        COMMENT: "Comment",

                        // Справочные сущности (CheckTypes/BarrierTypes/CheckResults) имеют
                        // собственные имена code/text полей, кроме общего Category.
                        CATEGORY: "Category",
                        CHECK_TYPE_CODE: "CheckTypeCode",
                        CHECK_TYPE_TEXT: "CheckTypeText",
                        BARRIER_TYPE_CODE: "BarrierTypeCode",
                        BARRIER_TYPE_TEXT: "BarrierTypeText",
                        RESULT_CODE: "ResultCode",
                        RESULT_TEXT_REF: "ResultText",

                        PK_LEVEL: "PkLevel",
                        PK_LEVEL_TEXT: "PkLevelText",
                        PK_LEVELS: "PkLevels",

                        PROFESSION_CODE: "ProfessionCode",
                        PROFESSION_TEXT: "ProfessionText",

                        // CheckRoots — read-only pass-through + вычисляемые KPI-поля.
                        DOC_ID: "DocId",
                        DATE: "Date",
                        LPC_KEY: "LpcKey",
                        LPC_TEXT: "LpcText",
                        PROF_KEY: "ProfKey",
                        PROF_TEXT: "ProfText",
                        LOCATION_NAME_ROOT: "LocationName",
                        CHECKS_SUCCESS: "ChecksSuccess",
                        CHECKS_AMOUNT: "ChecksAmount",
                        BARRIERS_SUCCESS: "BarriersSuccess",
                        BARRIERS_AMOUNT: "BarriersAmount",
                        CHECKS_CRITICALITY: "ChecksCriticality",
                        BARRIERS_CRITICALITY: "BarriersCriticality",
                        HAS_ERROR_CHECKS: "HasErrorChecks",
                        HAS_ERROR_BARRIERS: "HasErrorBarriers",
                        HEADER_KPI_TITLE: "HeaderKpiTitle",
                        HEADER_KPI_SUBTITLE: "HeaderKpiSubtitle",
                        BARRIERS_HIDDEN: "BarriersHidden",
                        CHECKS_HIDDEN: "ChecksHidden",
                        THIS_IS_INTEGRATION_DATA: "ThisIsIntegrationData",
                        // Видимость баджей декларативна (UI.Hidden в annotations.xml);
                        // эти поля нужны только как seed-дефолты для transient-объекта (KpiSync.js).
                        INTEGRATION_BADGE_HIDDEN: "IntegrationBadgeHidden",
                        CHECKS_ERROR_BADGE_HIDDEN: "ChecksErrorBadgeHidden",
                        BARRIERS_ERROR_BADGE_HIDDEN: "BarriersErrorBadgeHidden",

                        // CheckBasics (редактируемые поля — живут на to_Basic, не на Root).
                        BASIC_DATE: "Date",
                        BASIC_TIME: "Time",
                        TIMEZONE: "Timezone",
                        TIMEZONE_TEXT: "TimezoneText",
                        BASIC_LPC_KEY: "LpcKey",
                        BASIC_PROF_KEY: "ProfKey",
                        LOCATION_KEY: "LocationKey",
                        LOCATION_NAME: "LocationName",
                        PARENT_LOCATION_UUID: "ParentLocationUuid",
                        HAS_CHILDREN: "HasChildren",
                        WITH_SUBLOCATION: "with_sub",

                        OBSERVED_FULLNAME: "ObservedFullname",
                        OBSERVED_PERNR: "ObservedPerner",
                        OBSERVER_FULLNAME: "ObserverFullname",
                        OBSERVER_PERNR: "ObserverPerner",

                        LOCATION_UUID: "LocationUuid",
                        FIO: "Fio",
                        PERNR: "Pernr",
                        ACTIVE_FROM: "ActiveFrom",
                        ACTIVE_TO: "ActiveTo"
                },

                RESULT_CODES: {
                        SATISFACTORY: "X",
                        UNSATISFACTORY: ""
                },

                // Сервер вычисляет BarriersHidden/ChecksHidden только после первого save
                // (ZI_CheckRoot.cds), поэтому для transient-объекта (до save) правило
                // дублируется здесь, иначе обе таблицы были бы видимы всё время заполнения
                // формы. Применяется только когда ODataContextUtils.isTransient(context) —
                // для сохранённых записей не используется. Значения должны совпадать с
                // serve_config.py BUSINESS_RULES (CHECKS_HIDDEN_PK_LEVEL, BARRIERS_HIDDEN_PK_LEVELS).
                TRANSIENT_UX_RULES: {
                        CHECKS_HIDDEN_PK_LEVEL: "",
                        BARRIERS_HIDDEN_PK_LEVELS: ["", "0", "1"]
                },

                UI_MODEL: {
                        NAME: "ui",
                        EDITABLE_PATH: "/editable",
                        EDITABLE_BINDING: "{ui>/editable}"
                },

                ID_SUFFIXES: {
                        ADD_ENTRY: "::addEntry",
                        DELETE_ENTRY: "::deleteEntry",
                        // Не путать с DELETE_ENTRY выше: тот — суффикс кнопок удаления строк
                        // подтаблиц (SubTableCrud), этот — кнопки "Удалить" в футере ObjectPage.
                        DELETE_ENTRY_BUTTON: "--deleteEntry",
                        SAVE_BUTTON: "--save",
                        EDIT_BUTTON: "--edit",
                        CANCEL_BUTTON: "--cancel",
                        CLOSE_COLUMN_BUTTON: "--closeColumn"
                },

                FACET_IDS: {
                        CHECK_ROWS_SECTION: "CheckRowsSection",
                        BARRIERS_SECTION: "BarriersSection"
                },

                EDM_TYPES: {
                        TIME: "Edm.Time"
                },

                // Field-контролы, которые стреляют событием change (см.
                // ObjectPageExtension._wireFieldChangeListeners).
                FIELD_CONTROL_TYPES: [
                        "sap.m.Input",
                        "sap.m.DatePicker",
                        "sap.m.DateTimePicker",
                        "sap.m.TimePicker",
                        "sap.m.Select",
                        "sap.m.ComboBox",
                        "sap.m.MultiComboBox",
                        "sap.m.CheckBox",
                        "sap.m.Switch",
                        "sap.m.StepInput",
                        "sap.m.RatingIndicator",
                        "sap.ui.comp.smartfield.SmartField"
                ],

                /**
                 * @param {sap.ui.core.Control} oControl
                 * @returns {boolean} whether oControl is one of FIELD_CONTROL_TYPES.
                 */
                isFieldControl(oControl) {
                        return this.FIELD_CONTROL_TYPES.some((sType) => oControl.isA(sType));
                },

                POLLING: {
                        SUGGEST_DEBOUNCE_MS: 300
                },

                EXPORT: {
                        ROW_LIMIT: 5000
                },

                // Точка адаптации под Fiori 3.0 breakpoints (используется в SubTableCrud._forceBlockPopin).
                RESPONSIVE: {
                        POPIN_MIN_SCREEN_WIDTH: "Tablet"
                },

                SENTINELS: {
                        // Код-заглушка для строк проверки из интеграции без сопоставления
                        // со справочником CheckTypes (см. CheckCodeCoverage.js).
                        CHECK_TYPE_RAW_TEXT_CODE: "000.000"
                },

                SUGGEST: {
                        MIN_CHARS: 3
                }
        };
});
