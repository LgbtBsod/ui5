sap.ui.define([], function () {
    "use strict";

    function normalize(vValue, fnNormalize) {
        if (typeof fnNormalize === "function") {
            return fnNormalize(vValue);
        }
        return String(vValue || "").trim().toLowerCase();
    }

    function getField(oPerson, aKeys) {
        for (var i = 0; i < aKeys.length; i += 1) {
            var sKey = aKeys[i];
            if (oPerson && oPerson[sKey] !== undefined && oPerson[sKey] !== null) {
                return oPerson[sKey];
            }
        }
        return "";
    }

    function normalizePerson(oPerson) {
        return {
            perner: getField(oPerson, ["perner", "PERNER", "personnel_number"]),
            fullName: getField(oPerson, ["fullName", "full_name", "FULL_NAME", "name"]),
            position: getField(oPerson, ["position", "POSITION"]),
            orgUnit: getField(oPerson, ["orgUnit", "org_unit", "ORG_UNIT"]),
            integrationName: getField(oPerson, ["integrationName", "integration_name", "INTEGRATION_NAME"])
        };
    }

    function resolveSuggestionTargetPath(sTarget) {
        return sTarget === "observed" ? "/observedSuggestions" : "/observerSuggestions";
    }

    function filterSuggestions(mArgs) {
        var sQuery = normalize(mArgs && mArgs.query, mArgs && mArgs.normalizeText);
        var aPersons = (mArgs && mArgs.persons) || [];
        var iLimit = (mArgs && mArgs.limit) || 10;

        var aFiltered = aPersons.map(normalizePerson).filter(function (oPerson) {
            var sName = normalize(oPerson && oPerson.fullName, mArgs && mArgs.normalizeText);
            var sPernr = normalize(oPerson && oPerson.perner, mArgs && mArgs.normalizeText);
            var sPosition = normalize(oPerson && oPerson.position, mArgs && mArgs.normalizeText);
            if (!sQuery) {
                return true;
            }
            return sName.indexOf(sQuery) >= 0 || sPernr.indexOf(sQuery) >= 0 || sPosition.indexOf(sQuery) >= 0;
        });

        return dedupeSuggestions(aFiltered).slice(0, iLimit);
    }

    function dedupeSuggestions(aSuggestions) {
        var mSeen = {};
        return (aSuggestions || []).filter(function (oPerson) {
            var oNormalized = normalizePerson(oPerson);
            var sKey = String((oNormalized && oNormalized.perner) || "") + "|" + String((oNormalized && oNormalized.fullName) || "") + "|" + String((oNormalized && oNormalized.position) || "");
            if (mSeen[sKey]) {
                return false;
            }
            mSeen[sKey] = true;
            return true;
        });
    }

    function shouldFetchRemoteSuggestions(mArgs) {
        var sQuery = String((mArgs && mArgs.query) || "").trim();
        return !!sQuery;
    }

    function applySuggestionsToViewModel(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        if (!oViewModel || typeof oViewModel.setProperty !== "function") {
            return false;
        }
        oViewModel.setProperty(resolveSuggestionTargetPath(mArgs && mArgs.target), ((mArgs && mArgs.suggestions) || []).map(normalizePerson));
        return true;
    }

    function resolvePersonFromSuggestionEvent(oEvent) {
        var oItem = oEvent && oEvent.getParameter ? oEvent.getParameter("selectedItem") : null;
        var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
        var sTarget = oSource && typeof oSource.data === "function" ? oSource.data("target") : "observer";
        if (!oItem || typeof oItem.getBindingContext !== "function") {
            return { target: sTarget, person: null };
        }
        var oCtx = oItem.getBindingContext("view");
        return {
            target: sTarget,
            person: normalizePerson(oCtx && oCtx.getObject ? oCtx.getObject() : null)
        };
    }

    function applyPersonSelection(mArgs) {
        var oPerson = normalizePerson(mArgs && mArgs.person);
        var oSelectedModel = mArgs && mArgs.selectedModel;
        if (!oPerson || !oSelectedModel || typeof oSelectedModel.setProperty !== "function") {
            return false;
        }

        var sPrefix = (mArgs && mArgs.target) === "observed" ? "OBSERVED_" : "OBSERVER_";
        oSelectedModel.setProperty("/basic/" + sPrefix + "PERNER", oPerson.perner || "");
        oSelectedModel.setProperty("/basic/" + sPrefix + "FULLNAME", oPerson.fullName || "");
        oSelectedModel.setProperty("/basic/" + sPrefix + "POSITION", oPerson.position || "");
        oSelectedModel.setProperty("/basic/" + sPrefix + "ORGUNIT", oPerson.orgUnit || "");
        oSelectedModel.setProperty("/basic/" + sPrefix + "INTEGRATION_NAME", oPerson.integrationName || "");
        return true;
    }

    return {
        resolveSuggestionTargetPath: resolveSuggestionTargetPath,
        filterSuggestions: filterSuggestions,
        shouldFetchRemoteSuggestions: shouldFetchRemoteSuggestions,
        applySuggestionsToViewModel: applySuggestionsToViewModel,
        resolvePersonFromSuggestionEvent: resolvePersonFromSuggestionEvent,
        applyPersonSelection: applyPersonSelection,
        dedupeSuggestions: dedupeSuggestions,
        normalizePerson: normalizePerson
    };
});
