sap.ui.define([
        "sap/ui/model/Filter",
        "sap/ui/model/FilterOperator",
        "sap/ui/model/json/JSONModel",
        "sap/ui/core/Fragment",
        "./Constants",
        "./ODataUtils",
        "./DialogLifecycle",
        "./FioriElementsDom",
        "./I18n"
], function (Filter, FilterOperator, JSONModel, Fragment, Constants, ODataUtils, DialogLifecycle, FioriElementsDom, I18n) {
        "use strict";

        const F = Constants.FIELDS;
        const ROOT_LABEL = I18n.getText("locationRootLabel");
        const ENTITY_PATH = `/${Constants.ENTITY_SETS.LOCATION_HIERARCHY}`;
        // Только поля, реально читаемые диалогом (список, навигация, выбор) —
        // см. LocationDialog.fragment.xml и _onRowPress/_onRowNavigate ниже.
        const LEVEL_SELECT = `${F.LOCATION_UUID},${F.LOCATION_NAME},${F.HAS_CHILDREN}`;

        /** @returns {boolean} whether the given row is the currently selected one. */
        function locationIsSelectedFormatter(sRowUuid, sSelectedUuid) {
                return !!(sRowUuid && sSelectedUuid && sRowUuid === sSelectedUuid);
        }

        /** @returns {"Success"|"None"} row highlight, derived from locationIsSelectedFormatter. */
        function highlightFormatter(sRowUuid, sSelectedUuid) {
                return locationIsSelectedFormatter(sRowUuid, sSelectedUuid) ? "Success" : "None";
        }

        /**
         * LocationPickerSession — управляет одним открытым диалогом выбора локации.
         * Создаётся заново при каждом открытии диалога (не singleton).
         */
        class LocationPickerSession {
                /**
                 * @param {sap.ui.core.mvc.View} oView - view, из которой открыт диалог
                 * @param {Function} fnOnPick - callback для сохранения результата выбора
                 * @param {Function} [fnAfterFieldChange] - вызывается после прямой записи
                 *   LocationName/LocationKey в модель через setProperty (header-режим);
                 *   см. использование в _chooseCurrentLevel.
                 */
                constructor(oView, fnOnPick, fnAfterFieldChange) {
                        this._oView = oView;
                        this._fnOnPick = fnOnPick;
                        this._fnAfterFieldChange = fnAfterFieldChange;
                        this._oModel = oView.getModel();
                        this._oHeaderContext = oView.getBindingContext();
                        this._bFilterBarMode = !this._oHeaderContext;
                        this._oFilterControl = this._bFilterBarMode ? LocationPickerSession._findFilterControl(oView) : null;
                        this._oState = new JSONModel({
                                breadcrumbCurrentText: ROOT_LABEL,
                                breadcrumbLinks: [],
                                levelItems: [],
                                selectedNodeId: null,
                                showSublocationSwitch: this._bFilterBarMode,
                                withSublocation: false
                        });
                        this._aPath = [];
                        this._oDialog = null;
                }

                open() {
                        if (!this._oHeaderContext && !this._oFilterControl) {
                                return;
                        }
                        Fragment.load({
                                name: "sap.pc_lite.check.ext.fragment.LocationDialog",
                                controller: this._buildFragmentController()
                        }).then((oControl) => {
                                this._oDialog = oControl;
                                this._oDialog.setModel(this._oState, "locationModel");
                                this._oDialog.setModel(I18n.getModel(), "i18n");
                                this._oDialog.attachAfterClose(() => {
                                        clearTimeout(this._iSearchDebounceId);
                                        DialogLifecycle.destroyWithModels(this._oDialog, [this._oState]);
                                });
                                this._oView.addDependent(this._oDialog);
                                this._oDialog.open();
                                this._loadCurrentLevel();
                        });
                }

                _buildFragmentController() {
                        return {
                                formatter: {
                                        locationRowHighlight: highlightFormatter,
                                        locationIsSelected: locationIsSelectedFormatter
                                },
                                onLocSearch: (oEvent) => this._onSearch(oEvent),
                                onLocationRowPress: (oEvent) => this._onRowPress(oEvent),
                                onLocationRowNavigate: (oEvent) => this._onRowNavigate(oEvent),
                                onBreadcrumbPress: (oEvent) => this._navigateTo(oEvent.getSource().data("uuid"), ""),
                                onLocationChooseCurrentLevel: () => this._chooseCurrentLevel(),
                                onCloseLocationDialog: () => this._oDialog.close()
                        };
                }

                _loadChildren(sParentUuid) {
                        return ODataUtils.readEntitySet(this._oModel, ENTITY_PATH, {
                                filters: [new Filter(F.PARENT_LOCATION_UUID, FilterOperator.EQ, sParentUuid || "")],
                                urlParameters: { "$select": LEVEL_SELECT }
                        });
                }

                _loadSearch(sQuery) {
                        return ODataUtils.readEntitySet(this._oModel, ENTITY_PATH, {
                                filters: [new Filter(F.LOCATION_NAME, FilterOperator.Contains, sQuery)],
                                urlParameters: { "$select": LEVEL_SELECT }
                        });
                }

                _loadCurrentLevel() {
                        const sParentUuid = this._aPath.length ? this._aPath[this._aPath.length - 1].uuid : "";
                        return this._loadChildren(sParentUuid).then((aItems) => {
                                this._oState.setProperty("/levelItems", aItems);
                        });
                }

                _renderBreadcrumb() {
                        const aLinks = this._aPath.length
                                ? [{ text: ROOT_LABEL, uuid: "" }].concat(this._aPath.slice(0, -1).map((o) => ({ text: o.name, uuid: o.uuid })))
                                : [];
                        this._oState.setProperty("/breadcrumbLinks", aLinks);
                        this._oState.setProperty("/breadcrumbCurrentText",
                                this._aPath.length ? this._aPath[this._aPath.length - 1].name : ROOT_LABEL);
                }

                _navigateTo(sUuid, sName) {
                        if (!sUuid) {
                                this._aPath = [];
                        } else {
                                const iExisting = this._aPath.findIndex((o) => o.uuid === sUuid);
                                this._aPath = iExisting !== -1
                                        ? this._aPath.slice(0, iExisting + 1)
                                        : this._aPath.concat([{ uuid: sUuid, name: sName }]);
                        }
                        this._oState.setProperty("/selectedNodeId", null);
                        this._renderBreadcrumb();
                        this._loadCurrentLevel();
                }

                _onSearch(oEvent) {
                        const sQuery = oEvent.getParameter("newValue");
                        clearTimeout(this._iSearchDebounceId);
                        this._iSearchDebounceId = setTimeout(() => {
                                if (!sQuery) {
                                        this._loadCurrentLevel();
                                        return;
                                }
                                this._loadSearch(sQuery).then((aItems) => this._oState.setProperty("/levelItems", aItems));
                        }, Constants.POLLING.SUGGEST_DEBOUNCE_MS);
                }

                _onRowPress(oEvent) {
                        // Клик на название всегда ВЫБИРАЕТ строку, даже если у локации есть
                        // дочерние элементы — навигация вниз только через отдельную стрелочку
                        // (onLocationRowNavigate / _onRowNavigate).
                        const oRow = oEvent.getSource().getBindingContext("locationModel").getObject();
                        this._oState.setProperty("/selectedNodeId", oRow[F.LOCATION_UUID]);
                }

                _onRowNavigate(oEvent) {
                        const oRow = oEvent.getSource().getBindingContext("locationModel").getObject();
                        this._navigateTo(oRow[F.LOCATION_UUID], oRow[F.LOCATION_NAME]);
                }

                _chooseCurrentLevel() {
                        const sUuid = this._oState.getProperty("/selectedNodeId");
                        const oSelected = this._oState.getProperty("/levelItems").find((o) => o[F.LOCATION_UUID] === sUuid);
                        if (!oSelected) {
                                return;
                        }
                        const oPickResult = {
                                value: oSelected[F.LOCATION_NAME],
                                withSublocation: this._oState.getProperty("/withSublocation")
                        };

                        if (this._bFilterBarMode) {
                                LocationPickerSession._applyFilterValue(this._oFilterControl, oPickResult.value);
                        } else {
                                // LocationKey/LocationName проецируются с to_Basic прямо на Root.
                                const sPath = this._oHeaderContext.getPath();
                                this._oModel.setProperty(`${sPath}/${F.LOCATION_NAME}`, oPickResult.value);
                                this._oModel.setProperty(`${sPath}/${F.LOCATION_KEY}`, oSelected[F.LOCATION_UUID]);

                                // setProperty не эмитит native DOM 'change', на который в остальных
                                // случаях подписан ObjectPageExtension._scheduleTick — без явного
                                // вызова RequiredFieldsRule.refresh не пересчитается, и поле
                                // "Местоположение" останется с valueState=Error. Раньше здесь
                                // диспатчился синтетический DOM 'change' на весь view (скрытый канал
                                // связи с контроллером); теперь — прямой вызов переданного callback'а.
                                if (this._fnAfterFieldChange) {
                                        this._fnAfterFieldChange();
                                }
                        }

                        if (this._fnOnPick) {
                                this._fnOnPick(oPickResult);
                        }

                        this._oDialog.close();
                }

                static _findFilterControl(oView) {
                        const oFilterBar = FioriElementsDom.findElement("sap.ui.comp.smartfilterbar.SmartFilterBar", () => true, oView);
                        return (oFilterBar && oFilterBar.getControlByKey) ? oFilterBar.getControlByKey(F.LOCATION_NAME_ROOT) : null;
                }

                static _applyFilterValue(oControl, sValue) {
                        if (!oControl || !oControl.setValue) {
                                return;
                        }
                        oControl.setValue(sValue);
                        if (oControl.fireChange) {
                                oControl.fireChange({ value: sValue });
                        }
                }
        }

        class LocationPicker {
                // fnAfterFieldChange приходит из ValueHelpAutoApply._onNodeAdded (3-й
                // параметр fnSpecialSetup) — см. её JSDoc.
                static replace(oValueHelp, oView, fnAfterFieldChange) {
                        oValueHelp.attachAfterClose(() => oValueHelp.destroy());
                        oValueHelp.close();
                        new LocationPickerSession(oView, (oResult) => {
                                LocationPicker._oLastSublocationPick = oResult;
                        }, fnAfterFieldChange).open();
                }

                static getLastSublocationPick() {
                        return LocationPicker._oLastSublocationPick;
                }

                // Единственное место, где решается "это value help локации?"
                // (используется и в ListReportExtension, и в ObjectPageExtension).
                static pickHandler(sEntitySet) {
                        return sEntitySet === Constants.ENTITY_SETS.LOCATIONS ? this.replace : null;
                }
        }

        LocationPicker._oLastSublocationPick = null;

        return LocationPicker;
});
