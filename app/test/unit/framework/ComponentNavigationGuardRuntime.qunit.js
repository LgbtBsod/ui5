sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentNavigationGuardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (ComponentNavigationGuardRuntime, StatePaths, WorkflowContracts) {
    "use strict";

    function createStateModel(mSeed) {
        var mState = Object.assign({}, mSeed || {});
        return {
            getProperty: function (sPath) {
                return mState[sPath];
            },
            setProperty: function (sPath, vValue) {
                mState[sPath] = vValue;
            }
        };
    }

    function createRouteEvent(sName, mArgs) {
        var bPrevented = false;
        return {
            preventDefault: function () {
                bPrevented = true;
            },
            getParameter: function (sParam) {
                if (sParam === "name") {
                    return sName;
                }
                if (sParam === "arguments") {
                    return mArgs || {};
                }
                return undefined;
            },
            wasPrevented: function () {
                return bPrevented;
            }
        };
    }

    function createHarness(mOverrides) {
        var mCounts = {
            queued: 0,
            reverted: 0,
            resumed: 0,
            cleared: 0,
            guardedSave: 0,
            released: 0
        };
        var oStateModel = createStateModel(Object.assign({
            "/activeObjectId": "CHK-1",
            "/selectedId": "CHK-1",
            "/isDirty": true,
            "/saveInFlight": false
        }, (mOverrides && mOverrides.state) || {}));
        var oRouter = {
            attachBeforeRouteMatched: function (fnHandler, oListener) {
                this._fnHandler = fnHandler;
                this._oListener = oListener;
            },
            initialize: function () {},
            navTo: function () {}
        };
        var oComponent = {
            getRouter: function () {
                return oRouter;
            },
            getModel: function () {
                return {
                    getResourceBundle: function () {
                        return {};
                    }
                };
            }
        };

        ComponentNavigationGuardRuntime.attachBeforeRouteMatched({
            component: oComponent,
            stateModel: oStateModel,
            statePaths: {
                SAVE_IN_FLIGHT: StatePaths.SAVE_IN_FLIGHT,
                PENDING_NAVIGATION_INTENT: StatePaths.PENDING_NAVIGATION_INTENT,
                WORKFLOW_DETAIL_EDIT_MODE: StatePaths.WORKFLOW_DETAIL_EDIT_MODE,
                WORKFLOW_DETAIL_LOCK_STATE: StatePaths.WORKFLOW_DETAIL_LOCK_STATE
            },
            workflowCoordinator: {
                confirmUnsavedAndHandle: function (_oController, fnOnSave, mOptions) {
                    var sDecision = (mOverrides && mOverrides.decision) || "SAVE";
                    if (sDecision === "SAVE") {
                        return Promise.resolve(fnOnSave && fnOnSave()).then(function () {
                            return "SAVE";
                        });
                    }
                    if (sDecision === "CANCEL") {
                        return Promise.resolve(mOptions && typeof mOptions.onCancel === "function" ? mOptions.onCancel() : null).then(function () {
                            return "CANCEL";
                        });
                    }
                    return Promise.resolve(sDecision);
                },
                releaseWithTrySave: function () {
                    mCounts.released += 1;
                    return Promise.resolve(null);
                }
            },
            runGuardedSave: function () {
                mCounts.guardedSave += 1;
                return Promise.resolve(true);
            },
            queuePendingNavigationIntent: function () {
                mCounts.queued += 1;
            },
            clearPendingNavigationIntent: function () {
                mCounts.cleared += 1;
            },
            revertPendingNavigationIntent: function () {
                mCounts.reverted += 1;
            },
            resumePendingNavigationIntent: function () {
                mCounts.resumed += 1;
            },
            restorePendingNavigationIntent: function () {
                return null;
            },
            resetDetailAccessGuard: function () {},
            resetDetailNavigationState: function () {}
        });

        return {
            counts: mCounts,
            routeEvent: createRouteEvent("analytics", {}),
            fire: function () {
                oRouter._fnHandler.call(oRouter._oListener, this.routeEvent);
            }
        };
    }

    QUnit.module("ComponentNavigationGuardRuntime");

    QUnit.test("dirty save flow delegates resume ownership to guarded save", function (assert) {
        var done = assert.async();
        var oHarness = createHarness({ decision: "SAVE" });

        oHarness.fire();

        setTimeout(function () {
            assert.ok(oHarness.routeEvent.wasPrevented(), "navigation is guarded");
            assert.strictEqual(oHarness.counts.queued, 1, "pending intent is queued");
            assert.strictEqual(oHarness.counts.reverted, 1, "route is reverted while confirmation runs");
            assert.strictEqual(oHarness.counts.guardedSave, 1, "guarded save is invoked");
            assert.strictEqual(oHarness.counts.resumed, 0, "navigation guard does not resume eagerly on SAVE");
            done();
        }, 0);
    });

    QUnit.test("dirty no-change flow resumes pending navigation immediately", function (assert) {
        var done = assert.async();
        var oHarness = createHarness({ decision: "NO_CHANGES" });

        oHarness.fire();

        setTimeout(function () {
            assert.strictEqual(oHarness.counts.guardedSave, 0, "save callback is skipped for no-change path");
            assert.strictEqual(oHarness.counts.resumed, 1, "pending navigation resumes for no-change path");
            done();
        }, 0);
    });

    QUnit.test("failed save clears queued pending navigation intent", function (assert) {
        var done = assert.async();
        var oHarness = createHarness({ decision: "SAVE_FAILED" });

        oHarness.fire();

        setTimeout(function () {
            assert.strictEqual(oHarness.counts.resumed, 0, "failed save does not resume navigation");
            assert.strictEqual(oHarness.counts.cleared, 1, "stale pending navigation intent is cleared");
            done();
        }, 0);
    });

    QUnit.test("post-create hydration does not trigger lock release guard", function (assert) {
        var oHarness = createHarness({
            state: {
                "/activeObjectId": "__CREATE",
                "/selectedId": "__CREATE",
                "/postOpenHydratedRootId": "CHK-REAL-1",
                "/isDirty": false,
                "/saveInFlight": false,
                "/workflow/detail/editMode": WorkflowContracts.EDIT_MODES.EDIT,
                "/workflow/detail/lock/state": WorkflowContracts.LOCK_STATES.EDIT_LOCKED
            }
        });

        oHarness.routeEvent = createRouteEvent("detail", { id: "CHK-REAL-1" });
        oHarness.fire();

        assert.notOk(oHarness.routeEvent.wasPrevented(), "navigation continues without guard");
        assert.strictEqual(oHarness.counts.released, 0, "lock release guard is not invoked");
        assert.strictEqual(oHarness.counts.queued, 0, "pending intent is not queued");
    });
});
