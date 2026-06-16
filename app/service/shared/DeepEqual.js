sap.ui.define([], function () {
    "use strict";

    /* Order-insensitive structural equality.
     *
     * Replaces the previous `JSON.stringify(a) === JSON.stringify(b)` comparison
     * in DeltaCore.eq, which produced false negatives (and therefore phantom
     * change-set rows) whenever two objects with the same keys were built in
     * different insertion orders. This is the same contract as lodash/fast-deep-equal
     * but implemented inline to avoid a runtime npm dependency in an enterprise
     * SAPUI5 preload bundle.
     *
     * Semantics:
     *   - primitives compared by Strict Equality (===), with NaN treated as equal to NaN.
     *   - Date compared by getTime().
     *   - RegExp compared by source + flags.
     *   - arrays compared element-wise by length + element equality.
     *   - plain objects compared by key set + value equality (key order irrelevant).
     *   - functions / DOM nodes fall back to reference equality.
     *   - null !== undefined, null !== {}, {} !== [].
     */
    function deepEqual(a, b) {
        if (a === b) {
            return true;
        }
        if (a === null || b === null || typeof a !== "object" || typeof b !== "object") {
            // NaN handled here: only reached when === failed and at least one side is non-object
            return a !== a && b !== b;
        }

        var sClassA = Object.prototype.toString.call(a);
        var sClassB = Object.prototype.toString.call(b);
        if (sClassA !== sClassB) {
            return false;
        }

        if (sClassA === "[object Date]") {
            return a.getTime() === b.getTime();
        }
        if (sClassA === "[object RegExp]") {
            return a.source === b.source && a.flags === b.flags;
        }

        var bIsArray = Array.isArray(a);
        if (bIsArray !== Array.isArray(b)) {
            return false;
        }

        if (bIsArray) {
            if (a.length !== b.length) {
                return false;
            }
            for (var i = 0; i < a.length; i += 1) {
                if (!deepEqual(a[i], b[i])) {
                    return false;
                }
            }
            return true;
        }

        var aKeys = Object.keys(a);
        var bKeys = Object.keys(b);
        if (aKeys.length !== bKeys.length) {
            return false;
        }
        for (var k = 0; k < aKeys.length; k += 1) {
            var sKey = aKeys[k];
            if (!Object.prototype.hasOwnProperty.call(b, sKey)) {
                return false;
            }
            if (!deepEqual(a[sKey], b[sKey])) {
                return false;
            }
        }
        return true;
    }

    return Object.freeze({
        deepEqual: deepEqual
    });
});
