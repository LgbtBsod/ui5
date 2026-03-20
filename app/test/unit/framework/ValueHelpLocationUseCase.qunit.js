sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ValueHelpLocationUseCase"
], function (ValueHelpLocationUseCase) {
    "use strict";

    function createUiState(mSeed) {
        var mStore = Object.assign({}, mSeed || {});
        return {
            get: function (sModelName, sPath) {
                if (sModelName !== "view") {
                    return undefined;
                }
                return mStore[sPath];
            },
            set: function (sModelName, sPath, vValue) {
                if (sModelName === "view") {
                    mStore[sPath] = vValue;
                }
            }
        };
    }

    function findPatch(aEffects, sModelName, sPath) {
        return (aEffects || []).filter(function (oEffect) {
            return oEffect.type === "modelPatch" && oEffect.modelName === sModelName && oEffect.path === sPath;
        }).pop();
    }

    QUnit.module("ValueHelpLocationUseCase");

    QUnit.test("search clears stale hidden selection before showing filtered tree", function (assert) {
        var done = assert.async();
        var oUseCase = new ValueHelpLocationUseCase();
        var oUiState = createUiState({
            "/locationVhLoaded": true,
            "/locationVhCacheKey": "2026-03-20",
            "/locationVhTreeSource": [
                { location_id: "L1", location_name: "Alpha" },
                { location_id: "L2", location_name: "Beta" }
            ],
            "/locationVhTree": [
                { location_id: "L1", location_name: "Alpha" },
                { location_id: "L2", location_name: "Beta" }
            ],
            "/locationVhSelection": { location_id: "L1", location_name: "Alpha" },
            "/locationVhHasSelection": true
        });

        oUseCase.execute({ intent: "search", value: "Beta" }, {
            uiState: oUiState,
            locationLookup: {
                search: function () {
                    return Promise.resolve({
                        items: [
                            { location_id: "L1", location_name: "Alpha" },
                            { location_id: "L2", location_name: "Beta" }
                        ]
                    });
                }
            }
        }).then(function (oResult) {
            assert.ok(oResult.ok, "search succeeds");
            assert.strictEqual(findPatch(oResult.effects, "view", "/locationVhSelection").value, null, "stale selection is cleared");
            assert.strictEqual(findPatch(oResult.effects, "view", "/locationVhHasSelection").value, false, "selection flag is reset");
            assert.deepEqual(findPatch(oResult.effects, "view", "/locationVhTree").value, [
                { location_id: "L2", location_name: "Beta" }
            ], "tree contains only filtered visible rows");
            done();
        });
    });

    QUnit.test("confirm ignores selection that is no longer visible after filtering", function (assert) {
        var done = assert.async();
        var oUseCase = new ValueHelpLocationUseCase();
        var oUiState = createUiState({
            "/locationVhTree": [
                { location_id: "L2", location_name: "Beta", location_text: "Beta text", location_code: "B2" }
            ],
            "/locationVhSelection": { location_id: "L1", location_name: "Alpha", location_text: "Alpha text", location_code: "A1" },
            "/locationVhHasSelection": false
        });

        oUseCase.execute({ intent: "confirm" }, {
            uiState: oUiState
        }).then(function (oResult) {
            assert.ok(oResult.ok, "confirm succeeds");
            assert.strictEqual(findPatch(oResult.effects, "selected", "/basic/LOCATION_NAME").value, "", "stale hidden selection is not applied");
            assert.strictEqual(findPatch(oResult.effects, "selected", "/basic/LOCATION_KEY").value, "", "stale key is not applied");
            assert.strictEqual(findPatch(oResult.effects, "state", "/isDirty").value, false, "dirty flag stays clean without valid selection");
            assert.strictEqual(findPatch(oResult.effects, "view", "/locationVhSelection").value, null, "selection is cleared after invalid confirm");
            done();
        });
    });
});
