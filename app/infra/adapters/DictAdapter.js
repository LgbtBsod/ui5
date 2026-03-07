sap.ui.define([
    "checklist/app/infra/adapters/shared/GatewayAdapterSupport",
    "checklist/app/util/ValueTokenParser"
], function (GatewayAdapterSupport, ValueTokenParser) {
    "use strict";

    var WEB_REQUIRED_FIELD_DOMAIN = "WEB_REQUIRED_FIELD";
    var WEB_VARIABLE_DOMAIN = "WEB_VARIABLE";

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

    function parseVariableValue(vRaw) {
        var bBoolToken;
        var sValue = String(vRaw == null ? "" : vRaw).trim();
        if (!sValue) {
            return "";
        }
        bBoolToken = ValueTokenParser.parseBooleanToken(sValue, null);
        if (bBoolToken !== null) {
            return bBoolToken;
        }
        if (/^-?\d+(\.\d+)?$/.test(sValue)) {
            return Number(sValue);
        }
        if ((sValue.charAt(0) === "{" && sValue.charAt(sValue.length - 1) === "}") || (sValue.charAt(0) === "[" && sValue.charAt(sValue.length - 1) === "]")) {
            try {
                return JSON.parse(sValue);
            } catch (e) {
                return sValue;
            }
        }
        return sValue;
    }

    function normalizeAllRows(aRows) {
        return (aRows || []).map(function (oRow) {
            return {
                domain: String(oRow.Domain || oRow.domain || "").trim(),
                key: String(oRow.Key || oRow.key || "").trim(),
                text: String(oRow.Text || oRow.text || "")
            };
        }).filter(function (oRow) {
            return !!oRow.domain && !!oRow.key;
        });
    }

    function loadAll() {
        return GatewayAdapterSupport.get("DictionaryItemSet", { "$top": 2000, "$orderby": "Domain asc,Key asc" }).then(function (oData) {
            return normalizeAllRows(GatewayAdapterSupport.asArray(oData));
        }).catch(function () { return []; });
    }

    function create(mArgs) {
        var oMasterDataModel = mArgs && mArgs.masterDataModel;
        var oStateModel = mArgs && mArgs.stateModel;
        var oEnvModel = mArgs && mArgs.envModel;

        return {
            ensureLoaded: function () {
                if (!oMasterDataModel) { return Promise.resolve({ loaded: true }); }
                var bLoaded = !!oMasterDataModel.getProperty("/dictLoaded") || (!!oStateModel && !!oStateModel.getProperty("/dictLoaded"));
                if (bLoaded) { return Promise.resolve({ loaded: true }); }

                return loadAll().then(function (aRows) {
                    var mGrouped = {};
                    var mFrontendVariables = {};
                    var aRequiredFields = [];

                    aRows.forEach(function (oRow) {
                        if (oRow.domain === WEB_REQUIRED_FIELD_DOMAIN) {
                            aRequiredFields.push(oRow.key);
                            return;
                        }
                        if (oRow.domain === WEB_VARIABLE_DOMAIN) {
                            mFrontendVariables[oRow.key] = parseVariableValue(oRow.text);
                            return;
                        }
                        if (!mGrouped[oRow.domain]) {
                            mGrouped[oRow.domain] = [];
                        }
                        mGrouped[oRow.domain].push({ Key: oRow.key, Text: oRow.text });
                    });

                    var aLpc = normalizeDictRows(mGrouped.LPC || []);
                    var aProf = normalizeDictRows(mGrouped.PROFESSION || []);
                    var aTz = normalizeDictRows(mGrouped.TIME_ZONE || []);
                    var aAttachmentTypes = normalizeDictRows(mGrouped.ATF_CAT || []);
                    oMasterDataModel.setProperty("/lpc", aLpc);
                    oMasterDataModel.setProperty("/professions", aProf);
                    if (aTz.length) { oMasterDataModel.setProperty("/timezones", aTz); }
                    if (aAttachmentTypes.length) { oMasterDataModel.setProperty("/attachmentTypes", aAttachmentTypes); }
                    oMasterDataModel.setProperty("/dictionaries", {
                        lpc: aLpc,
                        professions: aProf,
                        timezones: aTz,
                        attachmentTypes: aAttachmentTypes,
                        rawByDomain: mGrouped
                    });
                    oMasterDataModel.setProperty("/runtime/requiredFields", aRequiredFields);
                    oMasterDataModel.setProperty("/dictLoaded", true);
                    if (oStateModel && oStateModel.setProperty) { oStateModel.setProperty("/dictLoaded", true); }
                    if (oStateModel && oStateModel.setProperty) {
                        oStateModel.setProperty("/requiredFields", aRequiredFields);
                        oStateModel.setProperty("/frontendVariables", mFrontendVariables);
                    }
                    if (oEnvModel && oEnvModel.setProperty) {
                        oEnvModel.setProperty("/variables", mFrontendVariables);
                    }
                    return { loaded: true };
                }).catch(function () { return { loaded: true }; });
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
