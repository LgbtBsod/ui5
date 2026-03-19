sap.ui.define([], function () {
    "use strict";

    function LastChangeSetPort() {}

    LastChangeSetPort.prototype.readAggChangedOn = function () { return Promise.resolve(0); };

    return LastChangeSetPort;
});
