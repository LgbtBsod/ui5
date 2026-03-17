sap.ui.define([], function () {
    "use strict";

    function getNodes() {
        return {
            root: document && document.documentElement,
            body: document && document.body,
            container: document && document.getElementById && document.getElementById("ui5_container")
        };
    }

    function forEachNode(aNodes, fnWork) {
        (Array.isArray(aNodes) ? aNodes : []).forEach(function (oNode) {
            if (oNode && oNode.classList) {
                fnWork(oNode);
            }
        });
    }

    function addClass(aNodes, sClassName) {
        forEachNode(aNodes, function (oNode) {
            oNode.classList.add(sClassName);
        });
    }

    function removeClass(aNodes, sClassName) {
        forEachNode(aNodes, function (oNode) {
            oNode.classList.remove(sClassName);
        });
    }

    function toggleClass(aNodes, sClassName, bEnabled) {
        forEachNode(aNodes, function (oNode) {
            oNode.classList.toggle(sClassName, !!bEnabled);
        });
    }

    function setBodyAttribute(sName, sValue) {
        var oBody = getNodes().body;
        if (!oBody) {
            return;
        }
        oBody.setAttribute(sName, String(sValue));
    }

    function setStyleProperty(aNodes, sName, sValue, sPriority) {
        var bChanged = false;
        (Array.isArray(aNodes) ? aNodes : []).forEach(function (oNode) {
            if (oNode && oNode.style && typeof oNode.style.setProperty === "function") {
                oNode.style.setProperty(sName, String(sValue), sPriority || "");
                bChanged = true;
            }
        });
        return bChanged;
    }

    function setStyleProperties(aNodes, mValues, sPriority) {
        var bChanged = false;
        Object.keys(mValues || {}).forEach(function (sName) {
            bChanged = setStyleProperty(aNodes, sName, mValues[sName], sPriority) || bChanged;
        });
        return bChanged;
    }

    return {
        addClass: addClass,
        getNodes: getNodes,
        removeClass: removeClass,
        setBodyAttribute: setBodyAttribute,
        setStyleProperty: setStyleProperty,
        setStyleProperties: setStyleProperties,
        toggleClass: toggleClass
    };
});
