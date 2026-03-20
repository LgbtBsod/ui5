sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        REQUEST: Object.freeze({
            BATCH: "BATCH",
            DELETE: "DELETE",
            GET: "GET",
            GET_FUNCTION: "GET_FUNCTION",
            POST: "POST",
            POST_FUNCTION: "POST_FUNCTION"
        }),
        DEDUPE: Object.freeze({
            GET: "GET|",
            GET_FUNCTION: "GET_FUNCTION|"
        })
    });
});
