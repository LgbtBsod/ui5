sap.ui.define([], function () {
    "use strict";

    function destroyMapEntries(mEntries) {
        Object.keys(mEntries || {}).forEach(function (sKey) {
            var oEntry = mEntries[sKey];
            if (oEntry && oEntry.destroy) {
                oEntry.destroy();
            }
        });
    }

    function destroyBinding(oBinding, fnHandler, oListener) {
        if (!oBinding) {
            return null;
        }
        if (fnHandler && oBinding.detachChange) {
            oBinding.detachChange(fnHandler, oListener);
        }
        if (oBinding.destroy) {
            oBinding.destroy();
        }
        return null;
    }

    return {
        destroyBinding: destroyBinding,
        destroyMapEntries: destroyMapEntries
    };
});
