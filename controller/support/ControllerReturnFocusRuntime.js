sap.ui.define([
    "sap_ui5/service/framework/FocusRuntime"
], function (FocusRuntime) {
    "use strict";

    function resolveStoreProperty(mOptions, sDefaultName) {
        return String((mOptions && mOptions.storeProperty) || sDefaultName || "_mReturnFocus");
    }

    function resolveSkipProperty(mOptions, sDefaultName) {
        return String((mOptions && mOptions.skipProperty) || sDefaultName || "_mReturnFocusSkip");
    }

    function ensureMap(oHost, sPropertyName) {
        if (!oHost || !sPropertyName) {
            return null;
        }
        if (!oHost[sPropertyName]) {
            oHost[sPropertyName] = {};
        }
        return oHost[sPropertyName];
    }

    function remember(oHost, sKey, oControl, mOptions) {
        var sStoreProperty = resolveStoreProperty(mOptions);
        var mStore;
        if (!oHost || !sKey) {
            return null;
        }
        mStore = ensureMap(oHost, sStoreProperty);
        if (!mStore) {
            return null;
        }
        mStore[sKey] = oControl || null;
        return mStore[sKey];
    }

    function markSkipRestore(oHost, sKey, mOptions) {
        var sSkipProperty = resolveSkipProperty(mOptions);
        var mSkipStore;
        if (!oHost || !sKey) {
            return false;
        }
        mSkipStore = ensureMap(oHost, sSkipProperty);
        if (!mSkipStore) {
            return false;
        }
        mSkipStore[sKey] = true;
        return true;
    }

    function consumeSkipRestore(oHost, sKey, mOptions) {
        var sSkipProperty = resolveSkipProperty(mOptions);
        var mSkipStore = oHost && oHost[sSkipProperty];
        if (!mSkipStore || !sKey || !mSkipStore[sKey]) {
            return false;
        }
        mSkipStore[sKey] = false;
        return true;
    }

    function resolveRemembered(oHost, sKey, mOptions) {
        var sStoreProperty = resolveStoreProperty(mOptions);
        var mStore = oHost && oHost[sStoreProperty];
        if (!mStore || !sKey) {
            return null;
        }
        return mStore[sKey] || null;
    }

    function restore(oHost, sKey, mOptions) {
        var oTarget;
        if (!oHost || !sKey) {
            return null;
        }
        if (consumeSkipRestore(oHost, sKey, mOptions)) {
            return null;
        }
        oTarget = resolveRemembered(oHost, sKey, mOptions);
        FocusRuntime.focusSoon(oTarget);
        return oTarget;
    }

    return {
        markSkipRestore: markSkipRestore,
        remember: remember,
        resolveRemembered: resolveRemembered,
        restore: restore
    };
});
