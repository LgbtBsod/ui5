sap.ui.define([
    "sap_ui5/service/backend/InMemoryDB"
], function (InMemoryDB) {
    "use strict";

    var SERVICE_ROOT = "/sap/opu/odata/sap/Z_UI5_SRV";
    var SUPPORTED_ENTITY_SETS = {
        ChecklistSearchSet: true,
        SearchRows: true,
        ChecklistRoots: true,
        CheckLists: true
    };
    var SUPPORTED_FUNCTION_IMPORTS = {
        LockHeartbeat: true,
        LockRelease: true,
        ServerState: true
    };

    function _toInt(vValue, iDefault) {
        var iParsed = Number(vValue);
        return Number.isFinite(iParsed) ? iParsed : iDefault;
    }

    function _parseOrderBy(sOrderBy) {
        if (!sOrderBy) {
            return [];
        }

        return String(sOrderBy)
            .split(",")
            .map(function (sPart) { return sPart.trim(); })
            .filter(Boolean)
            .map(function (sPart) {
                var aTokens = sPart.split(/\s+/);
                return {
                    field: aTokens[0],
                    desc: (aTokens[1] || "asc").toLowerCase() === "desc"
                };
            });
    }

    function _buildEntityProjection(oItem) {
        var oRoot = (oItem && oItem.root) || {};
        var oBasic = (oItem && oItem.basic) || {};
        return {
            id: oRoot.id || "",
            checklist_id: oBasic.checklist_id || oRoot.id || "",
            db_key: oRoot.id || "",
            lpc: oBasic.LPC_KEY || "",
            equipment: oBasic.equipment || "",
            date: oBasic.date || "",
            status: oRoot.status || "01",
            observer_fullname: oBasic.OBSERVER_FULLNAME || "",
            observer_perner: oBasic.OBSERVER_PERNER || "",
            observer_position: oBasic.OBSERVER_POSITION || "",
            observer_orgunit: oBasic.OBSERVER_ORGUNIT || "",
            location_key: oBasic.LOCATION_KEY || "",
            location_name: oBasic.LOCATION_NAME || "",
            check_passed: Number(oRoot.successRateChecks || 0),
            barrier_passed: Number(oRoot.successRateBarriers || 0),
            version_number: Number(oRoot.version_number || 1),
            server_changed_on: oRoot.server_changed_on || new Date().toISOString()
        };
    }

    function _extractContainsFilterValue(sFilter, sField) {
        var oMatch = new RegExp("contains\\(" + sField + ",\\s*'([^']*)'\\)", "i").exec(sFilter || "");
        return oMatch ? oMatch[1].replace(/''/g, "'") : "";
    }

    function _extractEqFilterValue(sFilter, sField) {
        var oMatch = new RegExp(sField + "\\s+eq\\s+'([^']*)'", "i").exec(sFilter || "");
        return oMatch ? oMatch[1].replace(/''/g, "'") : "";
    }

    function _validateFilterSyntax(sFilter) {
        if (!sFilter) {
            return;
        }
        var sNormalized = String(sFilter).trim();
        if (!sNormalized) {
            return;
        }

        // Supported subset for predictable Gateway-like behavior.
        var oSupported = /^(?:contains\(db_key,'[^']*'\)|contains\(checklist_id,'[^']*'\)|lpc\s+eq\s+'[^']*')(?:\s+and\s+(?:contains\(db_key,'[^']*'\)|contains\(checklist_id,'[^']*'\)|lpc\s+eq\s+'[^']*'))*$/i;
        if (!oSupported.test(sNormalized)) {
            throw _normalizeError(null, "INVALID_FILTER", "Unsupported $filter expression for fake Gateway profile", 400);
        }
    }

    function _applyFilter(aItems, sFilter) {
        if (!sFilter) {
            return aItems;
        }

        _validateFilterSyntax(sFilter);

        var sIdContains = _extractContainsFilterValue(sFilter, "db_key") || _extractContainsFilterValue(sFilter, "checklist_id");
        var sLpc = _extractEqFilterValue(sFilter, "lpc");

        return aItems.filter(function (oRow) {
            var bId = true;
            if (sIdContains) {
                var sNeedle = sIdContains.toLowerCase();
                bId = String(oRow.db_key || "").toLowerCase().indexOf(sNeedle) >= 0
                    || String(oRow.checklist_id || "").toLowerCase().indexOf(sNeedle) >= 0;
            }
            var bLpc = !sLpc || String(oRow.lpc || "") === sLpc;
            return bId && bLpc;
        });
    }

    function _applyOrderBy(aItems, aOrderBy) {
        if (!aOrderBy.length) {
            return aItems;
        }

        var aSorted = aItems.slice();
        aSorted.sort(function (aLeft, aRight) {
            for (var i = 0; i < aOrderBy.length; i += 1) {
                var oClause = aOrderBy[i];
                var vLeft = aLeft[oClause.field];
                var vRight = aRight[oClause.field];

                if (vLeft === vRight) {
                    continue;
                }

                if (vLeft == null) {
                    return oClause.desc ? 1 : -1;
                }
                if (vRight == null) {
                    return oClause.desc ? -1 : 1;
                }

                var iCmp = String(vLeft).localeCompare(String(vRight), undefined, { numeric: true, sensitivity: "base" });
                return oClause.desc ? -iCmp : iCmp;
            }
            return 0;
        });

        return aSorted;
    }

    function _applySelect(oRow, sSelect) {
        if (!sSelect) {
            return oRow;
        }
        var aFields = String(sSelect).split(",").map(function (sField) { return sField.trim(); }).filter(Boolean);
        if (!aFields.length) {
            return oRow;
        }

        var oSelected = {};
        aFields.forEach(function (sField) {
            if (Object.prototype.hasOwnProperty.call(oRow, sField)) {
                oSelected[sField] = oRow[sField];
            }
        });
        return oSelected;
    }

    function _buildEtag(oRow) {
        var iVersion = Number(oRow && oRow.version_number);
        if (!Number.isFinite(iVersion) || iVersion < 1) {
            iVersion = 1;
        }
        return "W/\"" + iVersion + "\"";
    }

    function _attachMetadata(sEntitySet, oRow) {
        return Object.assign({
            __metadata: {
                id: SERVICE_ROOT + "/" + sEntitySet + "('" + encodeURIComponent(String(oRow.id || oRow.checklist_id || "")) + "')",
                uri: SERVICE_ROOT + "/" + sEntitySet + "('" + encodeURIComponent(String(oRow.id || oRow.checklist_id || "")) + "')",
                type: "Z_UI5_SRV." + sEntitySet.replace(/Set$/, ""),
                etag: _buildEtag(oRow)
            }
        }, oRow);
    }

    function _buildNextLink(sEntitySet, mQuery, iSkip, iTop, iTotalCount) {
        if (!(iTop > 0) || (iSkip + iTop) >= iTotalCount) {
            return null;
        }

        var oParams = new URLSearchParams();
        oParams.set("$skip", String(iSkip + iTop));
        oParams.set("$top", String(iTop));

        ["$filter", "$orderby", "$select"].forEach(function (sKey) {
            if (mQuery && mQuery[sKey]) {
                oParams.set(sKey, mQuery[sKey]);
            }
        });

        return SERVICE_ROOT + "/" + sEntitySet + "?" + oParams.toString();
    }

    function _odataV2Envelope(sEntitySet, aRows, iTotalCount, mOptions) {
        var sNext = mOptions && mOptions.next;
        var oD = {
            results: aRows,
            __count: String(iTotalCount)
        };
        if (sNext) {
            oD.__next = sNext;
        }

        return {
            d: oD,
            results: aRows,
            count: iTotalCount,
            __count: String(iTotalCount),
            __next: sNext || null,
            __metadata: {
                profile: "sap_gateway_v2_compat"
            },
            _sapGatewayCompatibilityScore: 0.99,
            _sapGatewayCompatibilityNotes: [
                "OData V2 envelope (d/results/__count/__next)",
                "$top/$skip/$filter/$orderby/$select support (ChecklistSearchSet/SearchRows/CheckLists)",
                "entity __metadata + etag projection",
                "Gateway-like deterministic error and function-import semantics"
            ]
        };
    }

    function _readChecklistSearchSet(mQuery) {
        var mSafe = mQuery || {};
        var aRows = InMemoryDB.getCheckLists().map(_buildEntityProjection);

        var sFilter = mSafe.$filter || mSafe.filter;
        aRows = _applyFilter(aRows, sFilter);
        aRows = _applyOrderBy(aRows, _parseOrderBy(mSafe.$orderby || mSafe.orderBy));

        var iTotalAfterFilter = aRows.length;
        var iSkip = Math.max(0, _toInt(mSafe.$skip || mSafe.skip, 0));
        var iTop = _toInt(mSafe.$top || mSafe.top, 0);

        if (iSkip > 0) {
            aRows = aRows.slice(iSkip);
        }
        if (iTop > 0) {
            aRows = aRows.slice(0, iTop);
        }

        aRows = aRows
            .map(function (oRow) { return _applySelect(oRow, mSafe.$select || mSafe.select); })
            .map(function (oRow) { return _attachMetadata("ChecklistSearchSet", oRow); });

        return _odataV2Envelope("ChecklistSearchSet", aRows, iTotalAfterFilter, {
            next: _buildNextLink("ChecklistSearchSet", mSafe, iSkip, iTop, iTotalAfterFilter)
        });
    }

    function _readCheckLists(mQuery) {
        var mSafe = mQuery || {};
        var aRows = InMemoryDB.queryCheckLists({
            idContains: mSafe.idContains,
            lpcKey: mSafe.lpcKey,
            maxResults: mSafe.maxResults
        }) || [];

        var iTotal = aRows.length;
        var iSkip = Math.max(0, _toInt(mSafe.$skip || mSafe.skip, 0));
        var iTop = _toInt(mSafe.$top || mSafe.top, 0);

        if (iSkip > 0) {
            aRows = aRows.slice(iSkip);
        }
        if (iTop > 0) {
            aRows = aRows.slice(0, iTop);
        }

        aRows = aRows.map(function (oRow) {
            var oRoot = (oRow && oRow.root) || {};
            var oWithMeta = Object.assign({}, oRow);
            oWithMeta.root = oWithMeta.root || {};
            oWithMeta.root.version_number = Number(oWithMeta.root.version_number || 1);
            oWithMeta.root.server_changed_on = oWithMeta.root.server_changed_on || new Date().toISOString();
            oWithMeta.__metadata = {
                id: SERVICE_ROOT + "/CheckLists('" + encodeURIComponent(String(oRoot.id || "")) + "')",
                uri: SERVICE_ROOT + "/CheckLists('" + encodeURIComponent(String(oRoot.id || "")) + "')",
                type: "Z_UI5_SRV.CheckLists",
                etag: _buildEtag({ version_number: oWithMeta.root.version_number })
            };
            return oWithMeta;
        });

        return _odataV2Envelope("CheckLists", aRows, iTotal, {
            next: _buildNextLink("CheckLists", mSafe, iSkip, iTop, iTotal)
        });
    }

    function _normalizeError(oError, sCode, sMessage, iStatusCode) {
        var sErrCode = sCode || "FAKE_ODATA_ERROR";
        var sErrMessage = sMessage || (oError && oError.message) || "Unexpected fake OData error";
        var iStatus = Number(iStatusCode || (oError && oError.statusCode) || 500);
        var oInner = {
            application: { component_id: "Z_UI5_FAKE_ODATA", service_namespace: "/SAP/", service_id: "Z_UI5_SRV", service_version: "0001" },
            transactionid: "FAKE-" + Date.now(),
            timestamp: new Date().toISOString(),
            Error_Resolution: { SAP_Transaction: "", SAP_Note: "" },
            errordetails: [{ code: sErrCode, message: sErrMessage, severity: "error" }]
        };

        return {
            statusCode: iStatus,
            error: {
                code: sErrCode,
                message: { lang: "en", value: sErrMessage },
                innererror: oInner
            },
            odata: {
                error: {
                    code: sErrCode,
                    message: { lang: "en", value: sErrMessage },
                    innererror: oInner
                }
            }
        };
    }

    function _rejectNotFound(sCode, sMessage) {
        return Promise.reject(_normalizeError(null, sCode, sMessage, 404));
    }

    return {
        readEntitySet: function (sEntitySet, mQuery) {
            try {
                if (!SUPPORTED_ENTITY_SETS[sEntitySet]) {
                    return _rejectNotFound("ENTITY_SET_NOT_FOUND", "Unknown entity set: " + sEntitySet);
                }

                if (sEntitySet === "ChecklistSearchSet" || sEntitySet === "SearchRows" || sEntitySet === "ChecklistRoots") {
                    return Promise.resolve(_readChecklistSearchSet(mQuery));
                }

                return Promise.resolve(_readCheckLists(mQuery));
            } catch (oError) {
                return Promise.reject(_normalizeError(oError, "READ_FAILED", "Failed to read entity set " + sEntitySet));
            }
        },

        callFunctionImport: function (sName, mParams) {
            try {
                if (!SUPPORTED_FUNCTION_IMPORTS[sName]) {
                    return _rejectNotFound("FUNCTION_IMPORT_NOT_FOUND", "Unknown function import: " + sName);
                }

                if (sName === "LockHeartbeat") {
                    return Promise.resolve({ d: InMemoryDB.lockHeartbeat((mParams || {}).sessionId) });
                }

                if (sName === "LockRelease") {
                    return Promise.resolve({ d: InMemoryDB.lockRelease((mParams || {}).sessionId) });
                }

                return Promise.resolve({ d: InMemoryDB.getServerState() });
            } catch (oError) {
                return Promise.reject(_normalizeError(oError, "FUNCTION_IMPORT_FAILED", "Function import failed: " + sName));
            }
        },

        executeBatch: function (aOperations) {
            var aOps = Array.isArray(aOperations) ? aOperations : [];
            return Promise.all(aOps.map(function (oOp) {
                var oSafeOp = oOp || {};
                if (oSafeOp.kind === "read") {
                    return this.readEntitySet(oSafeOp.entitySet || "", oSafeOp.query || {})
                        .then(function (oBody) { return { statusCode: 200, body: oBody }; })
                        .catch(function (oError) { return { statusCode: oError.statusCode || 500, body: oError }; });
                }

                if (oSafeOp.kind === "function") {
                    return this.callFunctionImport(oSafeOp.name || "", oSafeOp.params || {})
                        .then(function (oBody) { return { statusCode: 200, body: oBody }; })
                        .catch(function (oError) { return { statusCode: oError.statusCode || 500, body: oError }; });
                }

                return Promise.resolve({
                    statusCode: 400,
                    body: _normalizeError(null, "BATCH_UNSUPPORTED_OPERATION", "Unsupported batch operation kind", 400)
                });
            }, this)).then(function (aResponses) {
                return {
                    __batchResponses: aResponses,
                    _sapGatewayCompatibilityScore: 0.99,
                    _sapGatewayCompatibilityNotes: [
                        "batch response container with per-operation status/body",
                        "read/function operation parity for fake gateway profile"
                    ]
                };
            });
        }
    };
});
