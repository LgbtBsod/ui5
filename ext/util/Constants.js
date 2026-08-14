sap.ui.define([], function () {
        "use strict";

        // CDS-модель: CheckRoots/CheckBasics/CheckItems/Barriers, см. abap/README.md.
        return {
                ENTITY_SETS: {
                        LOCATIONS: "Locations",
                        LOCATION_HIERARCHY: "LocationHierarchy",
                        CHECK_RESULTS: "CheckResults",
                        TIME_ZONES: "TimeZones",
                        PK_LEVELS: "PkLevels",
                        PROFESSIONS: "Professions"
                },

                NAV_PROPERTIES: {
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

                        // Справочная сущность CheckResults (см. SubTableCrud._seedDefaultResultText) -
                        // собственные имена code/text полей, отдельные от локальных Result/ResultText выше.
                        RESULT_CODE: "ResultCode",
                        RESULT_TEXT_REF: "ResultText",

                        // CheckRoots — read-only pass-through + вычисляемые KPI-поля.
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
                        LOCATION_KEY: "LocationKey",
                        LOCATION_NAME: "LocationName",
                        PARENT_LOCATION_UUID: "ParentLocationUuid",
                        HAS_CHILDREN: "HasChildren",
                        // [Fix, as-of-versioning pass] Locations/LocationHierarchy only -
                        // see metadata.xml's own comment and
                        // resolvers.latest_location_version_per_code. LocationPicker uses
                        // this to send "EffectiveDate le datetime'<check's own Date>'".
                        EFFECTIVE_DATE: "EffectiveDate",
                        WITH_SUBLOCATION: "with_sub",

                        OBSERVED_FULLNAME: "ObservedFullname",
                        OBSERVED_PERNR: "ObservedPernr",
                        OBSERVER_FULLNAME: "ObserverFullname",
                        OBSERVER_PERNR: "ObserverPernr",

                        LOCATION_UUID: "LocationUuid",
                        ACTIVE_FROM: "ActiveFrom",
                        ACTIVE_TO: "ActiveTo"
                },

                RESULT_CODES: {
                        SATISFACTORY: "X"
                },

                // Сервер вычисляет BarriersHidden/ChecksHidden только после первого save
                // (ZI_CheckRoot.cds), поэтому для нового несохранённого черновика (до save)
                // правило дублируется здесь, иначе обе таблицы были бы видимы всё время
                // заполнения формы. Применяется только когда
                // ODataContextUtils.isUnsavedNewDraft(context) — для сохранённых записей и
                // edit-черновиков существующих записей не используется. Значения должны
                // совпадать с serve_config.py BUSINESS_RULES (CHECKS_HIDDEN_PK_LEVEL,
                // BARRIERS_HIDDEN_PK_LEVELS).
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
                        // [Fix, best-practices pass] ADD_ENTRY ("::addEntry") and
                        // DELETE_ENTRY ("::deleteEntry") — the SmartTable's OWN native
                        // toolbar button suffixes — removed as dead: they only existed to
                        // support the toolbar-button-injection approach SubTableCrud.js no
                        // longer uses (see that file's top-of-file comment). Not to be
                        // confused with DELETE_ENTRY_BUTTON below, which is unrelated (the
                        // ObjectPage footer's own "Удалить" button) and is still live.
                        DELETE_ENTRY_BUTTON: "--deleteEntry",
                        SAVE_BUTTON: "--save",
                        EDIT_BUTTON: "--edit",
                        CANCEL_BUTTON: "--cancel",
                        CLOSE_COLUMN_BUTTON: "--closeColumn",
                        // [Fix, use-case pass] extensionAPI.rebind(sId) resolves sId via
                        // oController.byId(sId) and requires the SmartTable control itself
                        // (instanceof-checked internally) - the FACET's own id (e.g.
                        // "CheckRowsSection") resolves to nothing, so rebind() silently
                        // no-ops. The real local id, per StableIdHelper's
                        // {type:'ObjectPageTable', subType:'SmartTable'} naming convention,
                        // is the facet id + this suffix (confirmed live:
                        // "CheckRowsSection::Table"). Passing the bare facet id was the
                        // root cause of rows staying invisible until the next full save -
                        // rebindTable() was simply never being called.
                        SMART_TABLE: "::Table"
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
