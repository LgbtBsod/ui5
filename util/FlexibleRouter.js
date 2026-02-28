sap.ui.define([
    "sap/ui/core/routing/Router",
    "sap/ui/core/routing/HashChanger"
], function (Router, HashChanger) {
    "use strict";

    function _decodeUriComponentSafe(sValue) {
        try {
            return decodeURIComponent(sValue || "");
        } catch (e) {
            return String(sValue || "");
        }
    }

    function _logInfo(sMessage, oData) {
        if (window.jQuery && jQuery.sap && jQuery.sap.log && jQuery.sap.log.info) {
            jQuery.sap.log.info(sMessage, oData ? JSON.stringify(oData) : "", "sap_ui5.util.FlexibleRouter");
            return;
        }
        if (window.console && window.console.info) {
            window.console.info("[FlexibleRouter] " + sMessage, oData || {});
        }
    }

    function _parseHashParts(sHash) {
        var sSafe = String(sHash || "");
        var iQuery = sSafe.indexOf("?");
        return {
            path: iQuery >= 0 ? sSafe.slice(0, iQuery) : sSafe,
            query: iQuery >= 0 ? sSafe.slice(iQuery + 1) : ""
        };
    }

    function _readLayoutFromHash(sHash) {
        var sQuery = _parseHashParts(sHash).query;
        if (!sQuery) {
            return "";
        }
        var aParts = sQuery.split("&");
        for (var i = 0; i < aParts.length; i += 1) {
            var sPair = aParts[i] || "";
            var iEq = sPair.indexOf("=");
            var sKey = iEq >= 0 ? sPair.slice(0, iEq) : sPair;
            var sVal = iEq >= 0 ? sPair.slice(iEq + 1) : "";
            if (_decodeUriComponentSafe(sKey) === "layout") {
                return _decodeUriComponentSafe(sVal);
            }
        }
        return "";
    }

    return Router.extend("sap_ui5.util.FlexibleRouter", {

        constructor: function () {
            Router.apply(this, arguments);

            this._sStorageKey = "PCCT_FCL_LAYOUT_STACK";
            this._aLayoutStack = [];
            this._mLayoutByHash = {};
            this._sFclControlId = "fcl";
            this._mRoutePolicy = {
                search: { layout: "OneColumn", targetColumn: "begin" },
                detail: { layout: "TwoColumnsMidExpanded", targetColumn: "mid" }
            };

            this._hydrateLayoutStack();
            this._readPolicyFromManifestRoutes(arguments[0], arguments[3]);
            this.attachRouteMatched(this._onRouteMatched, this);

            this._oHashChanger = HashChanger.getInstance();
            this._fnHashChanged = this._onHashChanged.bind(this);
            this._oHashChanger.attachEvent("hashChanged", this._fnHashChanged);
        },

        destroy: function () {
            if (this._oHashChanger && this._fnHashChanged) {
                this._oHashChanger.detachEvent("hashChanged", this._fnHashChanged);
            }
            return Router.prototype.destroy.apply(this, arguments);
        },

        setFclControlId: function (sId) {
            this._sFclControlId = sId || "fcl";
        },

        navTo: function (sName, oParameters, bReplace, oOptions) {
            var mOptions = oOptions || {};
            var mPolicy = this._getPolicyForRoute(sName);
            var sLayout = mOptions.layout || mPolicy.layout;
            var sTargetColumn = mOptions.targetColumn || mPolicy.targetColumn;
            var oFcl = this._getFclControl();

            // WHY: keep graceful fallback for old shells where FCL may be absent.
            if (!oFcl) {
                Router.prototype.navTo.call(this, sName, oParameters, bReplace);
                return;
            }

            if (sLayout) {
                this._applyLayoutToFcl(sLayout);
            }
            if (sTargetColumn) {
                this._applyTargetColumnOverride(sName, sTargetColumn);
            }

            Router.prototype.navTo.call(this, sName, oParameters, bReplace);

            if (sLayout) {
                this._writeLayoutToHashQuery(sLayout, !!bReplace);
                this._pushLayoutHistory(sLayout);
            }
        },

        _readPolicyFromManifestRoutes: function (vRoutes, mTargets) {
            var aRoutes = Array.isArray(vRoutes) ? vRoutes : [];
            var mTargetConfig = (mTargets && typeof mTargets === "object") ? mTargets : {};
            for (var i = 0; i < aRoutes.length; i += 1) {
                var oRoute = aRoutes[i] || {};
                if (!oRoute.name) {
                    continue;
                }

                var aRouteTargets = Array.isArray(oRoute.target) ? oRoute.target : [oRoute.target];
                var oPrimaryTarget = mTargetConfig[aRouteTargets[0]] || {};

                // WHY: allow both route-level and target-level policy custom fields from manifest.
                this._mRoutePolicy[oRoute.name] = Object.assign({}, this._mRoutePolicy[oRoute.name] || {}, {
                    layout: oRoute.layout || oPrimaryTarget.layout || (this._mRoutePolicy[oRoute.name] && this._mRoutePolicy[oRoute.name].layout) || "",
                    targetColumn: oRoute.targetColumn || oPrimaryTarget.targetColumn || (this._mRoutePolicy[oRoute.name] && this._mRoutePolicy[oRoute.name].targetColumn) || ""
                });
            }
        },

        _getPolicyForRoute: function (sName) {
            return this._mRoutePolicy[sName] || {};
        },

        _getFclControl: function () {
            var oOwner = this._oOwner;
            if (!oOwner || !oOwner.getRootControl) {
                return null;
            }
            var oRoot = oOwner.getRootControl();
            if (!oRoot || !oRoot.byId) {
                return null;
            }
            return oRoot.byId(this._sFclControlId);
        },

        _applyLayoutToFcl: function (sLayout) {
            var oFcl = this._getFclControl();
            if (!oFcl || !oFcl.setLayout) {
                return;
            }
            oFcl.setLayout(sLayout);
            this._lastLayout = sLayout;
            _logInfo("Applied FCL layout", { layout: sLayout });
        },

        _applyTargetColumnOverride: function (sRouteName, sTargetColumn) {
            var mMap = {
                begin: "beginColumnPages",
                mid: "midColumnPages",
                end: "endColumnPages"
            };
            var sAgg = mMap[String(sTargetColumn || "").toLowerCase()];
            if (!sAgg) {
                return;
            }

            var oRoute = this.getRoute(sRouteName);
            if (!oRoute || !oRoute._oConfig) {
                return;
            }

            var aTargets = oRoute._oConfig.target;
            if (!Array.isArray(aTargets)) {
                aTargets = [aTargets];
            }

            var oTargets = this.getTargets();
            if (!oTargets || !oTargets._mTargets) {
                _logInfo("Target override skipped (targets internals unavailable)", { route: sRouteName, aggregation: sAgg });
                return;
            }

            aTargets.forEach(function (sTargetName) {
                var oTarget = oTargets._mTargets[sTargetName];
                if (oTarget && oTarget._oOptions) {
                    oTarget._oOptions.controlAggregation = sAgg;
                }
            });
        },

        _writeLayoutToHashQuery: function (sLayout, bReplace) {
            var oHashChanger = this.getHashChanger();
            var sHash = oHashChanger.getHash() || "";
            var oParts = _parseHashParts(sHash);
            var mParams = {};
            if (oParts.query) {
                oParts.query.split("&").forEach(function (sPair) {
                    if (!sPair) {
                        return;
                    }
                    var iEq = sPair.indexOf("=");
                    var sKey = iEq >= 0 ? sPair.slice(0, iEq) : sPair;
                    var sVal = iEq >= 0 ? sPair.slice(iEq + 1) : "";
                    mParams[_decodeUriComponentSafe(sKey)] = _decodeUriComponentSafe(sVal);
                });
            }
            mParams.layout = sLayout;

            var aPairs = [];
            Object.keys(mParams).forEach(function (sKey) {
                if (mParams[sKey] === "" || mParams[sKey] === null || mParams[sKey] === undefined) {
                    return;
                }
                aPairs.push(encodeURIComponent(sKey) + "=" + encodeURIComponent(mParams[sKey]));
            });

            var sNewHash = oParts.path + (aPairs.length ? "?" + aPairs.join("&") : "");
            if (sNewHash === sHash) {
                return;
            }
            if (bReplace) {
                oHashChanger.replaceHash(sNewHash);
            } else {
                oHashChanger.setHash(sNewHash);
            }
        },

        _onRouteMatched: function (oEvent) {
            var sName = oEvent.getParameter("name") || "";
            var sHash = this.getHashChanger().getHash() || "";
            var sLayoutFromQuery = _readLayoutFromHash(sHash);
            var mPolicy = this._getPolicyForRoute(sName);
            var sLayout = sLayoutFromQuery || mPolicy.layout || this._lastLayout;

            if (sLayout) {
                this._applyLayoutToFcl(sLayout);
                this._pushLayoutHistory(sLayout);
            }
        },

        _onHashChanged: function (oEvent) {
            var sNewHash = (oEvent && oEvent.getParameter && oEvent.getParameter("newHash")) || "";
            var sLayoutFromQuery = _readLayoutFromHash(sNewHash);
            if (sLayoutFromQuery) {
                this._applyLayoutToFcl(sLayoutFromQuery);
                this._mLayoutByHash[_parseHashParts(sNewHash).path] = sLayoutFromQuery;
                return;
            }

            var sPath = _parseHashParts(sNewHash).path;
            var sKnownLayout = this._mLayoutByHash[sPath];
            if (sKnownLayout) {
                this._applyLayoutToFcl(sKnownLayout);
            }
        },

        _pushLayoutHistory: function (sLayout) {
            if (!sLayout) {
                return;
            }
            this._aLayoutStack.push({ layout: sLayout, ts: Date.now() });
            if (this._aLayoutStack.length > 50) {
                this._aLayoutStack.shift();
            }
            try {
                if (typeof window !== "undefined" && window.localStorage) {
                    window.localStorage.setItem(this._sStorageKey, JSON.stringify(this._aLayoutStack));
                }
            } catch (e) {
                // best effort only
            }
        },

        _hydrateLayoutStack: function () {
            try {
                if (typeof window === "undefined" || !window.localStorage) {
                    return;
                }

                var sRaw = window.localStorage.getItem(this._sStorageKey);
                if (!sRaw) {
                    return;
                }
                var a = JSON.parse(sRaw);
                if (Array.isArray(a)) {
                    this._aLayoutStack = a;
                    if (a.length) {
                        this._lastLayout = a[a.length - 1].layout;
                    }
                }
            } catch (e) {
                this._aLayoutStack = [];
            }
        }
    });
});
