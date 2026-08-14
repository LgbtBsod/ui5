sap.ui.define(["./Constants"], function (Constants) {
	"use strict";

	const F = Constants.FIELDS;

	// [Fix, use-case pass #3 follow-up] UI.DataField's IconUrl property
	// (annotation/check-root.xml, FieldGroup#StatusBadges) is confirmed
	// reaching the client's parsed OData metamodel correctly - live check:
	//   oMetaModel.getODataEntityType("ZCheckService.CheckRoot")
	//     ["com.sap.vocabularies.UI.v1.FieldGroup#StatusBadges"]
	//   ...carries IconUrl: {String: "sap-icon://alert"} on both error-badge
	//   DataField records - but this specific SAPUI5 1.71.84 classic-template
	// header-facet rendering pipeline never reads it when building the
	// sap.m.ObjectStatus controls (confirmed live: .getIcon() still reports
	// the Criticality-auto-derived "sap-icon://status-negative", never
	// "sap-icon://alert"). Same class of framework gap as Criticality's
	// constant-EnumMember-vs-Path behaviour documented elsewhere in this
	// codebase - the annotation is spec-valid and parsed, just not acted
	// upon by this build. Applied here as a small, targeted post-render
	// override instead of waiting on a Fiori Elements fix.
	//
	// Keyed off the bound OData property name (oControl.getBindingPath
	// ("text"), confirmed live to resolve to "HasErrorChecks"/
	// "HasErrorBarriers"/"ThisIsIntegrationData") rather than a hardcoded
	// control id - the real id carries an auto-generated facet/view id
	// segment (...StatusBadgesHeaderFacet::HasErrorChecks::Field-objStatus)
	// that's stable in practice but not a documented contract, whereas the
	// bound property name is the same annotation-driven fact this whole
	// badge already keys off everywhere else (KpiSync, CSS badge-text hide).
	// [Fix, use-case pass #4] Integration badge added - unlike the two error
	// badges this isn't overriding a Criticality-implied shape (Integration-
	// Criticality stays hardcoded to 0/Neutral in resolvers.py, so it was
	// never going to reach the Error triangle by accident) - it's fixing a
	// plain semantic mismatch: the default Neutral-state icon is
	// sap-icon://status-inactive (a flat dash, reads as "off"/"nothing"),
	// not sap-icon://synchronize (sync arrows, the standard Fiori glyph for
	// "this record came from an external integration" - matches the badge's
	// own label "Создано интеграцией" far better).
	const ICON_BY_BOUND_PROPERTY = {
		[F.THIS_IS_INTEGRATION_DATA]: "sap-icon://synchronize",
		[F.HAS_ERROR_CHECKS]: "sap-icon://alert",
		[F.HAS_ERROR_BARRIERS]: "sap-icon://alert"
	};

	class BadgeIconOverride {
		static apply(oView) {
			oView.findAggregatedObjects(true, (oControl) => {
				if (!oControl.isA || !oControl.isA("sap.m.ObjectStatus")) {
					return false;
				}
				if (oControl.getId().indexOf("StatusBadgesHeaderFacet") === -1) {
					return false;
				}
				const sBoundProperty = oControl.getBindingPath("text");
				const sWantedIcon = ICON_BY_BOUND_PROPERTY[sBoundProperty];
				if (sWantedIcon && oControl.getIcon() !== sWantedIcon) {
					oControl.setIcon(sWantedIcon);
				}
				return false;
			});
		}
	}

	return BadgeIconOverride;
});
