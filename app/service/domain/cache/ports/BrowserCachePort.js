sap.ui.define([], function () {
    "use strict";

    function BrowserCachePort() {}

    BrowserCachePort.prototype.read = function () { return Promise.resolve(null); };
    BrowserCachePort.prototype.write = function () { return Promise.resolve(); };
    BrowserCachePort.prototype.clear = function () { return Promise.resolve(); };

    return BrowserCachePort;
});
