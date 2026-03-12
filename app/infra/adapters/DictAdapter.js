sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayAdapterSupport"
], function (GatewayAdapterSupport) {
    "use strict";

    var DOMAIN_KEYS = {
        LPC: "LPC",
        PROFESSION: "PROFESSION",
        TIME_ZONE: "TIME_ZONE",
        ATTACHMENT_TYPES: "ATF_CAT",
        CHECKS_NUMBER: "CHECKS_NUMBER",
        BARRIERS_NUMBER: "BARRIERS_NUMBER"
    };

    function readTypeList(oMasterDataModel, sType) {
        var sKey = String(sType || "");
        var aItems = oMasterDataModel.getProperty("/" + sKey);
        if (Array.isArray(aItems)) {
            return aItems;
        }
        var oDict = oMasterDataModel.getProperty("/dictionaries") || {};
        aItems = oDict[sKey];
        return Array.isArray(aItems) ? aItems : [];
    }

    function pickCode(oItem) { return String(oItem.code || oItem.Code || oItem.key || oItem.Key || oItem.value || oItem.Value || "").trim(); }
    function pickText(oItem) { return String(oItem.text || oItem.Text || oItem.description || oItem.Description || oItem.label || oItem.Label || "").trim(); }

    function normalizeDictRows(aRows) {
        return (aRows || []).map(function (oRow) {
            return { key: String(oRow.Key || oRow.key || ""), text: String(oRow.Text || oRow.text || "") };
        }).filter(function (oRow) { return !!oRow.key; });
    }

    function normalizeRowsForDomain(aRows) {
        return (aRows || []).map(function (oRow) {
            return {
                Key: String(oRow.Key || oRow.key || "").trim(),
                Text: String(oRow.Text || oRow.text || "")
            };
        }).filter(function (oRow) {
            return !!oRow.Key;
        });
    }

    function loadDomain(sDomain) {
        return GatewayAdapterSupport.get("DictionaryItemSet", {
            "$filter": "Domain eq '" + String(sDomain || "").replace(/'/g, "''") + "'",
            "$orderby": "Key asc",
            "$top": 500
        }).then(function (oData) {
            return normalizeRowsForDomain(GatewayAdapterSupport.asArray(oData));
        });
    }

    // Frontend composition seam for the SAP Gateway search-init bundle.
    // This remains search-domain only; detail/analytics boot data must not be
    // folded back into this loader even before dedicated backend bundle sets exist.
    function loadSearchBundle() {
        return Promise.all([
            loadDomain(DOMAIN_KEYS.LPC),
            loadDomain(DOMAIN_KEYS.PROFESSION),
            loadDomain(DOMAIN_KEYS.TIME_ZONE),
            loadDomain(DOMAIN_KEYS.ATTACHMENT_TYPES),
            loadDomain(DOMAIN_KEYS.CHECKS_NUMBER),
            loadDomain(DOMAIN_KEYS.BARRIERS_NUMBER)
        ]).then(function (aResult) {
            return {
                LPC: aResult[0] || [],
                PROFESSION: aResult[1] || [],
                TIME_ZONE: aResult[2] || [],
                ATF_CAT: aResult[3] || [],
                CHECKS_NUMBER: aResult[4] || [],
                BARRIERS_NUMBER: aResult[5] || []
            };
        });
    }

    function create(mArgs) {
        var oMasterDataModel = mArgs && mArgs.masterDataModel;

        return {
            ensureLoaded: function () {
                if (!oMasterDataModel) {
                    return Promise.reject(new Error("masterDataModel_missing"));
                }
                var bLoaded = !!oMasterDataModel.getProperty("/dictLoaded");
                if (bLoaded) { return Promise.resolve({ loaded: true }); }

                return loadSearchBundle().then(function (mGrouped) {
                    var aLpc = normalizeDictRows(mGrouped.LPC || []);
                    var aProf = normalizeDictRows(mGrouped.PROFESSION || []);
                    var aTz = normalizeDictRows(mGrouped.TIME_ZONE || []);
                    var aAttachmentTypes = normalizeDictRows(mGrouped.ATF_CAT || []);
                    var aChecksNumbers = normalizeDictRows(mGrouped.CHECKS_NUMBER || []);
                    var aBarriersNumbers = normalizeDictRows(mGrouped.BARRIERS_NUMBER || []);
                    oMasterDataModel.setProperty("/lpc", aLpc);
                    oMasterDataModel.setProperty("/professions", aProf);
                    if (aTz.length) { oMasterDataModel.setProperty("/timezones", aTz); }
                    if (aAttachmentTypes.length) { oMasterDataModel.setProperty("/attachmentTypes", aAttachmentTypes); }
                    if (aChecksNumbers.length) { oMasterDataModel.setProperty("/checksNumbers", aChecksNumbers); }
                    if (aBarriersNumbers.length) { oMasterDataModel.setProperty("/barriersNumbers", aBarriersNumbers); }
                    oMasterDataModel.setProperty("/dictionaries", {
                        lpc: aLpc,
                        professions: aProf,
                        timezones: aTz,
                        attachmentTypes: aAttachmentTypes,
                        checksNumbers: aChecksNumbers,
                        barriersNumbers: aBarriersNumbers,
                        rawByDomain: mGrouped
                    });
                    oMasterDataModel.setProperty("/dictLoaded", true);
                    return { loaded: true, bundle: "searchInit" };
                });
            },
            getItem: function (mLookup) {
                if (!oMasterDataModel) { return null; }
                var sType = mLookup && mLookup.type;
                var sCode = String((mLookup && mLookup.code) || "");
                var aItems = readTypeList(oMasterDataModel, sType);
                var oFound = aItems.find(function (oItem) { return pickCode(oItem) === sCode; });
                return oFound ? { text: pickText(oFound) } : null;
            },
            listByType: function (mLookup) {
                if (!oMasterDataModel) { return []; }
                return readTypeList(oMasterDataModel, mLookup && mLookup.type);
            }
        };
    }

    return { create: create };
});
