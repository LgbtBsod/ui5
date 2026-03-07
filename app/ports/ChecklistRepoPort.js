sap.ui.define([], function () {
    "use strict";

    /**
     * @interface ChecklistRepoPort
     * Interface-like port for checklist persistence and retrieval.
     */
    return {
        /**
         * @param {{rootId:string}} mArgs
         * @returns {Promise<{root:Object,basicInfo:Object,checks:Array,barriers:Array,attachments?:Array,meta:Object}>}
         */
        loadDetailSnapshot: function (mArgs) {},

        /**
         * @param {{rootId:string,delta:Object}} mArgs
         * @returns {Promise<{serverSnapshot:Object,lastChangeSet?:Object}>}
         */
        saveChecklist: function (mArgs) {},

        /**
         * @param {{rootId:string,delta:Object}} mArgs
         * @returns {Promise<{autosavedAt:string,serverHints?:Object}>}
         */
        autosaveChecklist: function (mArgs) {},

        /**
         * @param {{delta:Object}} mArgs
         * @returns {Promise<{serverSnapshot:Object,lastChangeSet?:Object}>}
         */
        createChecklist: function (mArgs) {},


        /**
         * @param {{rootId:string,statusCode:string}} mArgs
         * @returns {Promise<{statusCode:string}>}
         */
        setChecklistStatus: function (mArgs) {},

        /**
         * @param {{rootId:string}} mArgs
         * @returns {Promise<{deleted:true,rootId:string}>}
         */
        deleteChecklist: function (mArgs) {},

        /**
         * Optional implementation until backend attachments are available.
         * @param {{rootId:string}} mArgs
         * @returns {Promise<{attachments:Array}>}
         */
        loadAttachments: function (mArgs) {},

        /**
         * Optional implementation until backend attachments are available.
         * @param {{rootId:string,fileMeta:Object,contentBase64?:string}} mArgs
         * @returns {Promise<{attachment:Object}>}
         */
        uploadAttachment: function (mArgs) {},

        /**
         * Optional implementation until backend attachments are available.
         * @param {{rootId:string,attachmentId:string}} mArgs
         * @returns {Promise<{deleted:true}>}
         */
        deleteAttachment: function (mArgs) {}
    };
});
