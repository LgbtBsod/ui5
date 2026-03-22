sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentSaveGuardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (ComponentSaveGuardRuntime, StatePaths, WorkflowContracts) {
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

    QUnit.module("ComponentSaveGuardRuntime");

    QUnit.test("guarded save does not resume stale pending navigation by default", function (assert) {
        var done = assert.async();
        var iResumeCalls = 0;
        var oStateModel = createStateModel({
            "/activeObjectId": "CHK-1",
            "/pendingNavigationIntent": { routeName: "analytics", routeArgs: {} },
            "/workflow/detail/editMode": WorkflowContracts.EDIT_MODES.EDIT,
            "/workflow/detail/lock/state": WorkflowContracts.LOCK_STATES.EDIT_LOCKED
        });
        var fnRunGuardedSave = ComponentSaveGuardRuntime.createRunGuardedSave({
            component: { _oHeartbeat: null },
            stateModel: oStateModel,
            mainServiceModel: null,
            statePaths: StatePaths,
            detailFacade: {
                save: function () {
                    return Promise.resolve({ ok: true });
                }
            },
            buildLatestCtx: function () { return {}; },
            applyFacadeResult: function () {},
            emitTelemetry: function () {},
            resumePendingNavigationIntent: function () { iResumeCalls += 1; },
            resolveCorrelationId: function () { return ""; },
            isSessionExpiredError: function () { return false; },
            setGlobalBanner: function () {},
            clearGlobalBanner: function () {}
        });

        Promise.resolve(fnRunGuardedSave()).then(function () {
            assert.strictEqual(iResumeCalls, 0, "manual guarded save keeps pending navigation ownership untouched");
            done();
        });
    });

    QUnit.test("guarded save resumes queued navigation only for navigation-owned save", function (assert) {
        var done = assert.async();
        var iResumeCalls = 0;
        var oStateModel = createStateModel({
            "/activeObjectId": "CHK-1",
            "/pendingNavigationIntent": { routeName: "analytics", routeArgs: {} },
            "/workflow/detail/editMode": WorkflowContracts.EDIT_MODES.EDIT,
            "/workflow/detail/lock/state": WorkflowContracts.LOCK_STATES.EDIT_LOCKED
        });
        var fnRunGuardedSave = ComponentSaveGuardRuntime.createRunGuardedSave({
            component: { _oHeartbeat: null },
            stateModel: oStateModel,
            mainServiceModel: null,
            statePaths: StatePaths,
            detailFacade: {
                save: function () {
                    return Promise.resolve({ ok: true });
                }
            },
            buildLatestCtx: function () { return {}; },
            applyFacadeResult: function () {},
            emitTelemetry: function () {},
            resumePendingNavigationIntent: function () { iResumeCalls += 1; },
            resolveCorrelationId: function () { return ""; },
            isSessionExpiredError: function () { return false; },
            setGlobalBanner: function () {},
            clearGlobalBanner: function () {}
        });

        Promise.resolve(fnRunGuardedSave({ resumePendingNavigation: true })).then(function () {
            assert.strictEqual(iResumeCalls, 1, "navigation-owned save resumes queued navigation once");
            done();
        });
    });
});
