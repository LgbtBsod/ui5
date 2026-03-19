sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchShortcutRuntime"
], function (SearchShortcutRuntime) {
    "use strict";

    QUnit.module("SearchShortcutRuntime");

    QUnit.test("bindPowerUserShortcuts scopes listener to search view DOM", function (assert) {
        var oDomRef = {
            addEventListenerCalls: [],
            removeEventListenerCalls: [],
            addEventListener: function (sType, fnHandler, bCapture) {
                this.addEventListenerCalls.push({ type: sType, handler: fnHandler, capture: bCapture });
            },
            removeEventListener: function (sType, fnHandler, bCapture) {
                this.removeEventListenerCalls.push({ type: sType, handler: fnHandler, capture: bCapture });
            }
        };
        var oView = {
            addEventDelegateCalls: [],
            removeEventDelegateCalls: [],
            getDomRef: function () { return oDomRef; },
            addEventDelegate: function (oDelegate) { this.addEventDelegateCalls.push(oDelegate); },
            removeEventDelegate: function (oDelegate) { this.removeEventDelegateCalls.push(oDelegate); }
        };
        var oController = {
            getView: function () { return oView; }
        };

        SearchShortcutRuntime.bindPowerUserShortcuts(oController);
        SearchShortcutRuntime.unbindPowerUserShortcuts(oController);

        assert.strictEqual(oDomRef.addEventListenerCalls.length, 1, "keydown listener bound on view DOM");
        assert.strictEqual(oDomRef.addEventListenerCalls[0].type, "keydown", "keydown event is used");
        assert.strictEqual(oView.addEventDelegateCalls.length, 1, "view delegate registered for rerendering");
        assert.strictEqual(oDomRef.removeEventListenerCalls.length, 1, "keydown listener removed from view DOM");
        assert.strictEqual(oView.removeEventDelegateCalls.length, 1, "view delegate removed on cleanup");
    });
});
