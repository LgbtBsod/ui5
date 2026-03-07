sap.ui.define([
    "checklist/app/util/CreateSentinel"
], function (CreateSentinel) {
    "use strict";

    function parseDateLike(vValue) {
        var sValue;
        var oDate;
        if (vValue instanceof Date && !Number.isNaN(vValue.getTime())) {
            return vValue;
        }
        sValue = String(vValue || "").trim();
        if (!sValue) {
            return null;
        }
        if (/^\d{4}-\d{2}-\d{2}$/.test(sValue)) {
            oDate = new Date(sValue + "T00:00:00Z");
            return Number.isNaN(oDate.getTime()) ? null : oDate;
        }
        oDate = new Date(sValue);
        return Number.isNaN(oDate.getTime()) ? null : oDate;
    }

    function formatHumanDate(vValue) {
        var oDate = parseDateLike(vValue);
        if (!oDate) {
            return "";
        }
        return oDate.toLocaleDateString(undefined, {
            year: "numeric",
            month: "short",
            day: "2-digit"
        });
    }

    function formatDateTimeCardValue(sDate, sTime, sTimezone) {
        var aParts = [];
        var sHumanDate = formatHumanDate(sDate);
        if (sHumanDate) {
            aParts.push(sHumanDate);
        }
        if (sTime) {
            aParts.push(String(sTime));
        }
        if (sTimezone) {
            aParts.push(String(sTimezone));
        }
        return aParts.join("\n");
    }

    function formatAutosaveTime(vValue) {
        var oDate = parseDateLike(vValue);
        var sLanguageTag = "";
        if (!oDate) {
            return vValue || "-";
        }
        try {
            sLanguageTag = sap.ui.getCore().getConfiguration().getLanguageTag().toString();
        } catch (oError) {
            sLanguageTag = "";
        }
        try {
            return oDate.toLocaleTimeString(sLanguageTag || undefined, {
                hour: "2-digit",
                minute: "2-digit"
            });
        } catch (oLocaleError) {
            return oDate.toLocaleTimeString(undefined, {
                hour: "2-digit",
                minute: "2-digit"
            });
        }
    }

    return {
        formatPassedTotal: function (aRows) {
            if (!Array.isArray(aRows) || !aRows.length) {
                return "";
            }
            return aRows.filter(function (oRow) {
                return !!(oRow && oRow.result);
            }).length + "/" + aRows.length;
        },

        formatHeaderDate: function (sDate) {
            return formatHumanDate(sDate) || sDate || "-";
        },

        formatInfoCardValue: function (sKey, sDate, sTime, sTimezone, sEquipment, _sObserver, _sObserved, sLocationName, sLocationText, sLpcText, sProfText) {
            var mByKey = {
                datetime: formatDateTimeCardValue(sDate, sTime, sTimezone),
                equipment: [sEquipment],
                location: [sLocationName || sLocationText],
                lpc: [sLpcText],
                profession: [sProfText]
            };
            if (sKey === "datetime") {
                return mByKey[sKey] || "-";
            }
            return (mByKey[sKey] || []).filter(Boolean).join(" | ") || "-";
        },

        formatPersonSuggestion: function (sName, sPosition, sPerner, sOrgUnit) {
            return [sName, sPosition, sPerner, sOrgUnit].filter(Boolean).join(" | ");
        },

        formatAttachmentSize: function (nBytes) {
            var nValue = Number(nBytes || 0) || 0;
            if (nValue < 1024) {
                return nValue + " B";
            }
            if (nValue < 1024 * 1024) {
                return (nValue / 1024).toFixed(1) + " KB";
            }
            return (nValue / (1024 * 1024)).toFixed(1) + " MB";
        },

        formatAttachmentCategoryText: function (sCategoryKey, aAttachmentTypes, sCategoryText) {
            var sKey = String(sCategoryKey || "").trim();
            var aItems = Array.isArray(aAttachmentTypes) ? aAttachmentTypes : [];
            var oFound = aItems.find(function (oItem) {
                return String((oItem && oItem.key) || "").trim() === sKey;
            });
            return (oFound && oFound.text) || sCategoryText || sKey || "-";
        },

        formatAutosaveAt: function (sAutosaveAt) {
            return formatAutosaveTime(sAutosaveAt);
        },

        formatCopyLinkVisible: function (sActiveObjectId) {
            return !!sActiveObjectId && !CreateSentinel.isCreateId(sActiveObjectId);
        },

        formatDeleteChecklistVisible: function (sMode, sActiveObjectId) {
            return String(sMode || "").toUpperCase() === "EDIT" &&
                !!sActiveObjectId &&
                !CreateSentinel.isCreateId(sActiveObjectId);
        }
    };
});
