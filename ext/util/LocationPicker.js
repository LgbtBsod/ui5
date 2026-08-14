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
        // [Fix, use-case pass #5] ParentLocationUuid добавлен - нужен
        // _buildAncestorPath ниже, чтобы честно восстановить цепочку предков
        // при переходе из результата ПОИСКА (который ищет по всей иерархии,
        // не по текущей ветке - узел может оказаться где угодно в дереве).
        const LEVEL_SELECT = `${F.LOCATION_UUID},${F.LOCATION_NAME},${F.HAS_CHILDREN},${F.PARENT_LOCATION_UUID}`;

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
                        // [Fix, use-case pass #5] Monotonic token guarding /levelItems
                        // writes - _loadCurrentLevel and _onSearch both write the same
                        // property, and nothing previously stopped an earlier-fired-but-
                        // slower response (e.g. a broad search match) from overwriting a
                        // later, faster one (e.g. the user already navigated away) once it
                        // finally resolved. Every load bumps this and stamps its own
                        // closure with the value at fire time; a response only applies if
                        // its stamp still matches the CURRENT counter when it resolves.
                        this._iLoadSeq = 0;
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

                /**
                 * [Fix, as-of-versioning pass] "search date >= max(EffectiveDate)" -
                 * see metadata.xml's LocationHierarchy.EffectiveDate comment and
                 * resolvers.latest_location_version_per_code. Anchors on the open
                 * check's own header Date field ("as of when the check happened"),
                 * so LocationPicker resolves whichever LocationCode version was
                 * effective back then - not today's latest rename. Only meaningful
                 * in header/object-page mode, where _oHeaderContext is a real
                 * CheckRoot: filterBar mode (List Report's own quick-filter) has no
                 * single check to anchor on, so it deliberately sends no cutoff and
                 * the generic backend post-filter just returns each code's latest
                 * version, same as before this feature existed. A brand-new check
                 * with Date not filled in yet also falls through to "no cutoff" -
                 * showing today's latest names is the sane default until the user
                 * picks a date.
                 * @returns {sap.ui.model.Filter|null}
                 */
                _effectiveDateFilter() {
                        if (this._bFilterBarMode) {
                                return null;
                        }
                        const oCheckDate = this._oHeaderContext.getProperty(F.BASIC_DATE);
                        if (!oCheckDate) {
                                return null;
                        }
                        return new Filter(F.EFFECTIVE_DATE, FilterOperator.LE, oCheckDate);
                }

                _loadChildren(sParentUuid) {
                        const aFilters = [new Filter(F.PARENT_LOCATION_UUID, FilterOperator.EQ, sParentUuid || "")];
                        const oDateFilter = this._effectiveDateFilter();
                        if (oDateFilter) {
                                aFilters.push(oDateFilter);
                        }
                        return ODataUtils.readEntitySet(this._oModel, ENTITY_PATH, {
                                filters: aFilters,
                                urlParameters: { "$select": LEVEL_SELECT }
                        });
                }

                _loadSearch(sQuery) {
                        const aFilters = [new Filter(F.LOCATION_NAME, FilterOperator.Contains, sQuery)];
                        const oDateFilter = this._effectiveDateFilter();
                        if (oDateFilter) {
                                aFilters.push(oDateFilter);
                        }
                        return ODataUtils.readEntitySet(this._oModel, ENTITY_PATH, {
                                filters: aFilters,
                                urlParameters: { "$select": LEVEL_SELECT }
                        });
                }

                /** @returns {number} a fresh load token - stamp the response handler with it, apply only if still current. */
                _startLoad() {
                        this._iLoadSeq += 1;
                        return this._iLoadSeq;
                }

                _loadCurrentLevel() {
                        const sParentUuid = this._aPath.length ? this._aPath[this._aPath.length - 1].uuid : "";
                        const iToken = this._startLoad();
                        return this._loadChildren(sParentUuid).then((aItems) => {
                                if (iToken !== this._iLoadSeq) {
                                        return; // superseded by a newer navigate/search before this resolved
                                }
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

                _afterPathChange() {
                        this._oState.setProperty("/selectedNodeId", null);
                        this._renderBreadcrumb();
                        this._loadCurrentLevel();
                }

                /**
                 * [Fix, use-case pass #5] sParentUuid is now required for a node not
                 * already in _aPath - previously this blindly APPENDED the target to
                 * whatever path the user happened to be on, which is only correct for
                 * a genuine drill-down click (target's real parent === current level).
                 * Navigating into a SEARCH result (search spans the whole hierarchy,
                 * not the current branch - see _loadSearch) could land on a node from
                 * a completely different branch, silently corrupting the breadcrumb
                 * into a fabricated, non-ancestral path. Confirmed live before this
                 * fix: search a nested location's name, drill into it via the arrow,
                 * and the breadcrumb showed it as a child of wherever browsing had
                 * last been, not its real parent.
                 */
                _navigateTo(sUuid, sName, sParentUuid) {
                        if (!sUuid) {
                                this._aPath = [];
                                this._afterPathChange();
                                return;
                        }
                        const iExisting = this._aPath.findIndex((o) => o.uuid === sUuid);
                        if (iExisting !== -1) {
                                // Clicked an already-visited breadcrumb link - just roll back to it.
                                this._aPath = this._aPath.slice(0, iExisting + 1);
                                this._afterPathChange();
                                return;
                        }
                        const sCurrentUuid = this._aPath.length ? this._aPath[this._aPath.length - 1].uuid : "";
                        if ((sParentUuid || "") === sCurrentUuid) {
                                // Genuine drill-down from the level currently being browsed - cheap append, no extra fetch.
                                this._aPath = this._aPath.concat([{ uuid: sUuid, name: sName }]);
                                this._afterPathChange();
                                return;
                        }
                        // Reached from outside the current branch (a search result) -
                        // rebuild the TRUE ancestor chain from LocationHierarchy's own
                        // ParentLocationUuid links instead of guessing.
                        this._buildAncestorPath(sParentUuid).then((aAncestors) => {
                                this._aPath = aAncestors.concat([{ uuid: sUuid, name: sName }]);
                                this._afterPathChange();
                        });
                }

                /**
                 * Walks LocationHierarchy's own ParentLocationUuid chain upward from
                 * sStartParentUuid to the root, one fetch per level - only called when
                 * a node's ancestry isn't already known from browsing (see _navigateTo).
                 * Hierarchy depth here is small in practice (a handful of levels), so a
                 * sequential walk is fine; a real deep/wide hierarchy would want a
                 * dedicated server-side "ancestors of X" endpoint instead.
                 *
                 * [Fix, as-of-versioning pass] Deliberately no _effectiveDateFilter()
                 * here, unlike _loadChildren/_loadSearch: each step filters by an
                 * exact LocationUuid, which already identifies one specific
                 * version-row unambiguously (LocationCode is what's ambiguous across
                 * versions, not LocationUuid - see serve_config.py's LOC-003 rename,
                 * which minted a brand-new LocationUuid for the renamed row). Adding
                 * a cutoff here would add nothing and risks a false "dangling
                 * parent" if it were ever computed wrong.
                 */
                _buildAncestorPath(sStartParentUuid) {
                        const aChain = [];
                        const fnStep = (sUuid) => {
                                if (!sUuid) {
                                        return Promise.resolve(aChain);
                                }
                                return ODataUtils.readEntitySet(this._oModel, ENTITY_PATH, {
                                        filters: [new Filter(F.LOCATION_UUID, FilterOperator.EQ, sUuid)],
                                        urlParameters: { "$select": LEVEL_SELECT }
                                }).then((aResults) => {
                                        const oNode = aResults[0];
                                        if (!oNode) {
                                                return aChain; // dangling parent reference - stop, don't fail the navigation
                                        }
                                        aChain.unshift({ uuid: oNode[F.LOCATION_UUID], name: oNode[F.LOCATION_NAME] });
                                        return fnStep(oNode[F.PARENT_LOCATION_UUID]);
                                });
                        };
                        return fnStep(sStartParentUuid);
                }

                _onSearch(oEvent) {
                        const sQuery = oEvent.getParameter("newValue");
                        clearTimeout(this._iSearchDebounceId);
                        this._iSearchDebounceId = setTimeout(() => {
                                if (!sQuery) {
                                        this._loadCurrentLevel();
                                        return;
                                }
                                const iToken = this._startLoad();
                                this._loadSearch(sQuery).then((aItems) => {
                                        if (iToken !== this._iLoadSeq) {
                                                return; // superseded by a newer search/navigate before this resolved
                                        }
                                        this._oState.setProperty("/levelItems", aItems);
                                });
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
                        this._navigateTo(oRow[F.LOCATION_UUID], oRow[F.LOCATION_NAME], oRow[F.PARENT_LOCATION_UUID]);
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
