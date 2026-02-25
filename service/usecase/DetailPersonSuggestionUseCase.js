sap.ui.define([], function () {
    "use strict";

    function normalize(vValue, fnNormalize) {
        if (typeof fnNormalize === "function") {
            return fnNormalize(vValue);
        }
        return String(vValue || "").trim().toLowerCase();
    }

    function resolveSuggestionTargetPath(sTarget) {
        return sTarget === "observed" ? "/observedSuggestions" : "/observerSuggestions";
    }

    function filterSuggestions(mArgs) {
        var sQuery = normalize(mArgs && mArgs.query, mArgs && mArgs.normalizeText);
        var aPersons = (mArgs && mArgs.persons) || [];
        var iLimit = (mArgs && mArgs.limit) || 10;

        var aFiltered = aPersons.filter(function (oPerson) {
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
            var sKey = String((oPerson && oPerson.perner) || "") + "|" + String((oPerson && oPerson.fullName) || "") + "|" + String((oPerson && oPerson.position) || "");
            if (mSeen[sKey]) {
                return false;
            }
            mSeen[sKey] = true;
            return true;
        });
    }

    function shouldFetchRemoteSuggestions(mArgs) {
        var sQuery = String((mArgs && mArgs.query) || "").trim();
        var iLocalCount = Number(mArgs && mArgs.localCount) || 0;
        var iThreshold = typeof (mArgs && mArgs.minLocalBeforeRemote) === "number" ? mArgs.minLocalBeforeRemote : 3;
        return !!sQuery && iLocalCount < iThreshold;
    }

    function applySuggestionsToViewModel(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        if (!oViewModel || typeof oViewModel.setProperty !== "function") {
            return false;
        }
        oViewModel.setProperty(resolveSuggestionTargetPath(mArgs && mArgs.target), (mArgs && mArgs.suggestions) || []);
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
            person: oCtx && oCtx.getObject ? oCtx.getObject() : null
        };
    }

    function applyPersonSelection(mArgs) {
        var oPerson = mArgs && mArgs.person;
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
        dedupeSuggestions: dedupeSuggestions
    };
});
