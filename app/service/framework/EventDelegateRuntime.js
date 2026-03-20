sap.ui.define([], function () {
    "use strict";

    function ensure(oHost, sDelegateProperty, oTarget, oDelegate, oListener) {
        if (!oHost || !sDelegateProperty || !oTarget || !oTarget.addEventDelegate || !oDelegate) {
            return null;
        }
        if (oHost[sDelegateProperty] !== oDelegate) {
            oHost[sDelegateProperty] = oDelegate;
        }
        if (oTarget.removeEventDelegate) {
            oTarget.removeEventDelegate(oHost[sDelegateProperty]);
        }
        oTarget.addEventDelegate(oHost[sDelegateProperty], oListener || oHost);
        return oHost[sDelegateProperty];
    }

    function remove(oHost, sDelegateProperty, oTarget) {
        if (!oHost || !sDelegateProperty) {
            return false;
        }
        if (oTarget && oHost[sDelegateProperty] && oTarget.removeEventDelegate) {
            oTarget.removeEventDelegate(oHost[sDelegateProperty]);
        }
        oHost[sDelegateProperty] = null;
        return true;
    }

    return {
        ensure: ensure,
        remove: remove
    };
});
