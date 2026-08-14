// ObserverFullname/ObservedFullname/*Pernr проецируются с to_Basic на Root;
// редактируемый свободный текст (не read-only), см. abap/README.md.
sap.ui.define(["./Constants", "./I18n"], function (Constants, I18n) {
	"use strict";

	const F = Constants.FIELDS;
	const TOOLTIP = I18n.getText("pernrValidationTooltip");
	// Конфликт интересов: инспектор не может проверять сам себя. Сравниваем
	// Pernr (табельный номер), а не текст ФИО — тексты могут визуально
	// совпадать у разных людей (омонимы), а Pernr уникален.
	const CONFLICT_TOOLTIP = I18n.getText("pernrConflictOfInterestTooltip");

	function checkPair(oContext, sFioField, sPernrField) {
		const sFio = oContext.getProperty(sFioField);
		const sPernr = oContext.getProperty(sPernrField);
		if (sFio && !sPernr) {
			return { target: `${oContext.getPath()}/${sFioField}`, message: TOOLTIP };
		}
		return null;
	}

	// Сообщение вешаем на оба поля ФИО — независимо от того, в каком поле
	// пользователь только что что-то изменил, ошибка должна быть видна
	// сразу под обоими (симметричный конфликт, не "чья вина").
	function checkConflictOfInterest(oContext) {
		const sObservedPernr = oContext.getProperty(F.OBSERVED_PERNR);
		const sObserverPernr = oContext.getProperty(F.OBSERVER_PERNR);
		if (!sObservedPernr || !sObserverPernr || sObservedPernr !== sObserverPernr) {
			return [];
		}
		return [
			{ target: `${oContext.getPath()}/${F.OBSERVED_FULLNAME}`, message: CONFLICT_TOOLTIP },
			{ target: `${oContext.getPath()}/${F.OBSERVER_FULLNAME}`, message: CONFLICT_TOOLTIP }
		];
	}

	class PernrRule {
		static getIssues(oView) {
			const oContext = oView.getBindingContext();
			// [Fix, use-case pass #4] Integration-sourced records are view-only
			// now (see IntegrationEditGuard.js / metadata.xml sap:updatable-path)
			// - "ФИО без табельного номера" was a way to ask the user to fix
			// their OWN unmatched free-text entry via F4; on a record they can
			// never edit there's nothing to fix, so surfacing it here would
			// just be a dangling error the inspector can't act on. RawText
			// display fallback (resolvers.py) already handles showing
			// unmatched integration names correctly - this rule only needs to
			// gate SAVE-time input on records someone can actually edit.
			if (!oContext || oContext.getProperty(F.THIS_IS_INTEGRATION_DATA)) {
				return [];
			}
			return [
				checkPair(oContext, F.OBSERVED_FULLNAME, F.OBSERVED_PERNR),
				checkPair(oContext, F.OBSERVER_FULLNAME, F.OBSERVER_PERNR)
			].filter(Boolean).concat(checkConflictOfInterest(oContext));
		}
	}

	return PernrRule;
});
