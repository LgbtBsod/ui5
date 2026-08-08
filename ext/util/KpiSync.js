sap.ui.define(["./Constants", "./I18n"], function (Constants, I18n) {
        "use strict";

        const F = Constants.FIELDS;

        const KPI_TITLE_FORMAT = (sPkText, sProfText) =>
                I18n.getText("kpiTitleFormat", [sPkText, sProfText]);
        const KPI_SUBTITLE_FORMAT = (iChecks, iBarriers) =>
                I18n.getText("kpiSubtitleFormat", [iChecks, iBarriers]);

        // Для сохранённых записей заголовок/подзаголовок и BarriersHidden/ChecksHidden
        // приходят с сервера как push-down вычисляемые поля (ZI_CheckRoot.ddls.asddls) —
        // единственный источник истины. Этот класс пересчитывает их на клиенте
        // ТОЛЬКО пока объект transient (сервер до первого save ничего не вычисляет),
        // используя Constants.TRANSIENT_UX_RULES как временный fallback.
        class KpiSync {
                static sync(oView, fnIsTransient) {
                        const oContext = oView.getBindingContext();
                        if (!oContext || !fnIsTransient(oContext)) {
                                return;
                        }
                        const oModel = oView.getModel();

                        KpiSync._syncTitles(oContext, oModel);
                        KpiSync._syncHiddenFlags(oContext, oModel);
                        KpiSync._seedBadgeDefaults(oContext, oModel);
                }

                static _syncTitles(oContext, oModel) {
                        const sPath = oContext.getPath();
                        const sPkText = oContext.getProperty(F.LPC_TEXT) || "—";
                        const sProfText = oContext.getProperty(F.PROF_TEXT) || "—";
                        const iChecks = oContext.getProperty(F.CHECKS_SUCCESS) || 0;
                        const iBarriers = oContext.getProperty(F.BARRIERS_AMOUNT) || 0;
                        const sTitle = KPI_TITLE_FORMAT(sPkText, sProfText);
                        const sSubtitle = KPI_SUBTITLE_FORMAT(iChecks, iBarriers);

                        if (oContext.getProperty(F.HEADER_KPI_TITLE) !== sTitle) {
                                oModel.setProperty(`${sPath}/${F.HEADER_KPI_TITLE}`, sTitle);
                        }
                        if (oContext.getProperty(F.HEADER_KPI_SUBTITLE) !== sSubtitle) {
                                oModel.setProperty(`${sPath}/${F.HEADER_KPI_SUBTITLE}`, sSubtitle);
                        }
                }

                static _syncHiddenFlags(oContext, oModel) {
                        const sPath = oContext.getPath();
                        const sLpcKey = oContext.getProperty(F.LPC_KEY) || "";
                        const oRules = Constants.TRANSIENT_UX_RULES;
                        const bChecksHidden = sLpcKey === oRules.CHECKS_HIDDEN_PK_LEVEL;
                        const bBarriersHidden = oRules.BARRIERS_HIDDEN_PK_LEVELS.indexOf(sLpcKey) !== -1;

                        const _syncBooleanField = (sField, bValue) => {
                                if (typeof bValue === "boolean") {
                                        if (oContext.getProperty(sField) !== bValue) {
                                                oModel.setProperty(`${sPath}/${sField}`, bValue);
                                        }
                                }
                        };

                        _syncBooleanField(F.BARRIERS_HIDDEN, bBarriersHidden);
                        _syncBooleanField(F.CHECKS_HIDDEN, bChecksHidden);
                }

                // Для transient-объекта сервер не отдаёт badge-hidden поля вовсе;
                // без seed-дефолта undefined трактуется UI.Hidden как "false" (не скрыто),
                // и новый пустой объект на мгновение показал бы все три баджа.
                static _seedBadgeDefaults(oContext, oModel) {
                        const sPath = oContext.getPath();
                        [F.INTEGRATION_BADGE_HIDDEN, F.CHECKS_ERROR_BADGE_HIDDEN, F.BARRIERS_ERROR_BADGE_HIDDEN]
                                .forEach((sField) => {
                                        if (oContext.getProperty(sField) === undefined) {
                                                oModel.setProperty(`${sPath}/${sField}`, true);
                                        }
                                });
                }
        }

        return KpiSync;
});
