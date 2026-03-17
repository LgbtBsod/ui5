sap.ui.define([], function () {
    "use strict";

    /**
     * @interface LockPort
     */
    return {
        /** @param {{rootId:string,sessionGuid:string,force?:boolean}} mArgs @returns {Promise<{ok:boolean,code?:string,killed?:boolean,owner?:{userId?:string,sessionGuid?:string},canTakeover?:boolean,messageKey?:string,lockToken?:string,expiresAt?:string}>} */
        acquire: function (mArgs) {},
        /** @param {{rootId:string,sessionGuid:string}} mArgs @returns {Promise<{ok:boolean,code?:string,killed?:boolean,owner?:{userId?:string,sessionGuid?:string},canTakeover?:boolean,messageKey?:string,expiresAt?:string}>} */
        heartbeat: function (mArgs) {},
        /** @param {{rootId:string,sessionGuid:string}} mArgs @returns {Promise<{ok:boolean,code?:string,killed?:boolean,owner?:{userId?:string,sessionGuid?:string},canTakeover?:boolean,messageKey?:string,expiresAt?:string}>} */
        status: function (mArgs) {},
        /** @param {{rootId:string,sessionGuid:string}} mArgs @returns {Promise<{ok:boolean,code?:string,released:boolean,killed?:boolean,messageKey?:string}>} */
        release: function (mArgs) {}
    };
});
