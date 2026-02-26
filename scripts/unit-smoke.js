#!/usr/bin/env node
const fs = require('fs');
const vm = require('vm');

const results = [];

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}

async function runTest(name, fn) {
  try {
    await fn();
    results.push({ name, status: 'passed' });
  } catch (e) {
    results.push({ name, status: 'failed', error: e.message });
    throw e;
  }
}

function loadSapModule(path, depMap = {}) {
  const code = fs.readFileSync(path, 'utf8');
  let exported;
  const sandbox = {
    sap: {
      ui: {
        define: (deps, factory) => {
          if (typeof deps === 'function') {
            exported = deps();
            return;
          }
          const resolved = (deps || []).map((d) => {
            if (!(d in depMap)) throw new Error(`Missing dependency: ${d} for ${path}`);
            return depMap[d];
          });
          exported = factory(...resolved);
        }
      }
    },
    console
  };
  vm.createContext(sandbox);
  vm.runInContext(code, sandbox, { filename: path });
  return exported;
}

function createFilterStub() {
  function Filter(pathOrCfg, op, value) {
    if (typeof pathOrCfg === 'object') {
      this.filters = pathOrCfg.filters || [];
      this.and = !!pathOrCfg.and;
      return;
    }
    this.path = pathOrCfg;
    this.operator = op;
    this.value1 = value;
  }
  return Filter;
}


function testDetailFormattersLockOperationPresentation() {
  const mod = loadSapModule('util/DetailFormatters.js');
  const bundle = {
    getText: (k) => ({ modeEdit: 'Edit mode', modeRead: 'Read mode' }[k] || k)
  };

  assert(mod.lockOperationText('Custom text', 'READ', bundle) === 'Custom text',
    'lockOperationText should prefer explicit operation text');
  assert(mod.lockOperationText('', 'EDIT', bundle) === 'Edit mode',
    'lockOperationText should fallback to modeEdit text in EDIT mode');
  assert(mod.lockOperationText('', 'READ', bundle) === 'Read mode',
    'lockOperationText should fallback to modeRead text in READ mode');

  assert(mod.lockOperationState('SUCCESS', 'READ') === 'Success',
    'lockOperationState should map SUCCESS to Success');
  assert(mod.lockOperationState('ERROR', 'EDIT') === 'Error',
    'lockOperationState should map ERROR to Error');
  assert(mod.lockOperationState('', 'EDIT') === 'Success',
    'lockOperationState should fallback to Success in EDIT mode');
  assert(mod.lockOperationState('', 'READ') === 'Information',
    'lockOperationState should fallback to Information in READ mode');
}

function testDetailLifecycleUseCase() {
  const mod = loadSapModule('service/usecase/DetailLifecycleUseCase.js');
  const m = {};
  const model = {
    setProperty: (k, v) => { m[k] = v; },
    getProperty: (k) => m[k]
  };

  mod.setReadUnlocked(model);
  assert(m['/mode'] === 'READ', 'setReadUnlocked must set /mode=READ');
  assert(m['/isLocked'] === false, 'setReadUnlocked must set /isLocked=false');

  mod.setEditLocked(model);
  assert(m['/mode'] === 'EDIT', 'setEditLocked must set /mode=EDIT');
  assert(m['/isLocked'] === true, 'setEditLocked must set /isLocked=true');

  m['/isDirty'] = true;
  mod.resetDirty(model);
  assert(m['/isDirty'] === false, 'resetDirty must set /isDirty=false');

  m['/layout'] = 'TwoColumnsMidExpanded';
  m['/activeObjectId'] = '123';
  m['/isDirty'] = true;
  mod.prepareCloseNavigation(model);
  assert(m['/layout'] === 'OneColumn', 'prepareCloseNavigation must set /layout=OneColumn');
  assert(m['/activeObjectId'] === null, 'prepareCloseNavigation must clear /activeObjectId');
  assert(m['/isDirty'] === false, 'prepareCloseNavigation must reset /isDirty');

  m['/isLocked'] = true;
  m['/mode'] = 'READ';
  m['/isDirty'] = true;
  mod.keepEditModeAfterSave(model);
  assert(m['/mode'] === 'EDIT', 'keepEditModeAfterSave should keep EDIT if locked');
  assert(m['/isDirty'] === false, 'keepEditModeAfterSave should reset dirty');

  m['/isLocked'] = false;
  m['/mode'] = 'READ';
  mod.keepEditModeAfterSave(model);
  assert(m['/mode'] === 'READ', 'keepEditModeAfterSave should not force EDIT if unlocked');
}

function testSearchSmartControlCoordinator() {
  const Filter = createFilterStub();
  const FilterOperator = { Contains: 'Contains', EQ: 'EQ' };
  const mod = loadSapModule('util/SearchSmartControlCoordinator.js', {
    'sap/ui/model/Filter': Filter,
    'sap/ui/model/FilterOperator': FilterOperator
  });

  const viewState = {};
  const viewModel = {
    getProperty: (k) => viewState[k],
    setProperty: (k, v) => { viewState[k] = v; }
  };

  mod.setEnabled(viewModel, true, '');
  assert(mod.isEnabled(viewModel) === true, 'isEnabled should reflect setEnabled');

  let bootCalled = 0;
  const state = { '/mainServiceMetadataOk': false, '/mainServiceMetadataError': 'meta-fail' };
  const stateModel = { getProperty: (k) => state[k] };
  mod.syncAvailability({ stateModel, viewModel, unavailableText: 'fallback', bootstrap: () => { bootCalled += 1; } });
  assert(viewState['/useSmartControls'] === false, 'syncAvailability should disable controls when metadata failed');
  assert(viewState['/smartControlsReason'] === 'meta-fail', 'syncAvailability should set explicit reason');
  assert(viewState['/smartControlsReasonCode'] === 'metadata_error',
    'syncAvailability should expose metadata_error reason code for consolidation');
  assert(bootCalled === 0, 'syncAvailability should not bootstrap when metadata failed');

  const pendingAvailability = mod.resolveMetadataAvailability({ metadataOk: null, metadataError: '', unavailableText: 'fallback' });
  assert(pendingAvailability.enabled === true && pendingAvailability.reasonCode === 'metadata_pending',
    'resolveMetadataAvailability should expose metadata_pending state while metadata is unresolved');

  state['/mainServiceMetadataOk'] = true;
  mod.syncAvailability({ stateModel, viewModel, unavailableText: 'fallback', bootstrap: () => { bootCalled += 1; } });
  assert(viewState['/useSmartControls'] === true, 'syncAvailability should re-enable controls on metadata recovery');
  assert(viewState['/smartControlsReasonCode'] === 'metadata_ready',
    'syncAvailability should expose metadata_ready reason code after recovery');
  assert(bootCalled === 1, 'syncAvailability should bootstrap exactly once on recovery');

  const id = mod.extractChecklistId({ root: { id: 'A1' } });
  assert(id === 'A1', 'extractChecklistId should read root.id');

  const event = {
    getParameter: (k) => (k === 'listItem' ? {
      getBindingContext: () => ({ getObject: () => ({ CHECKLIST_ID: 'C-42' }) })
    } : null)
  };
  assert(mod.extractChecklistIdFromSelectionEvent(event) === 'C-42', 'extractChecklistIdFromSelectionEvent should read object id');

  const bindingParams = { filters: [], parameters: {}, events: {} };
  let dataReceivedCalled = 0;
  mod.applyRebindParams({
    bindingParams,
    state: {
      filterId: '77',
      filterLpc: 'LPC1',
      filterFailedChecks: 'TRUE',
      filterFailedBarriers: 'FALSE',
      searchMaxResults: '150'
    },
    onDataReceived: () => { dataReceivedCalled += 1; }
  });

  assert(bindingParams.filters.length >= 4, 'applyRebindParams should append expected filters');
  const hasLpcGatewayLikeFilter = bindingParams.filters.some((f) => Array.isArray(f.filters)
    && f.filters.some((inner) => inner.path === 'lpc')
    && f.filters.some((inner) => inner.path === 'LPC_KEY'));
  assert(hasLpcGatewayLikeFilter === true,
    'applyRebindParams should compose LPC filter for both gateway-like and legacy field names');
  const looseBinding = { filters: [], parameters: {}, events: {} };
  mod.applyRebindParams({
    bindingParams: looseBinding,
    state: {
      filterId: 'A',
      filterLpc: 'L1',
      filterFailedChecks: 'ALL',
      filterFailedBarriers: 'ALL',
      searchMaxResults: '10',
      searchMode: 'LOOSE'
    },
    onDataReceived: () => {}
  });
  assert(looseBinding.filters.length === 1 && looseBinding.filters[0].and === false,
    'applyRebindParams should compose a single OR filter group for LOOSE search mode');
  assert(bindingParams.parameters.top === 150, 'applyRebindParams should set top parameter');
  bindingParams.events.dataReceived({});
  assert(dataReceivedCalled === 1, 'applyRebindParams should chain dataReceived callback');

  let fallbackCalls = 0;
  mod.rebindOrFallback({ enabled: false, fallbackSearch: () => { fallbackCalls += 1; } });
  assert(fallbackCalls === 1, 'rebindOrFallback should call fallback when disabled');

  const bindingNoTop = { filters: [], parameters: { top: 500 }, events: {} };
  mod.applyRebindParams({
    bindingParams: bindingNoTop,
    state: {
      filterId: '',
      filterLpc: '',
      filterFailedChecks: 'ALL',
      filterFailedBarriers: 'ALL',
      searchMaxResults: '  '
    },
    onDataReceived: () => {}
  });
  assert(!Object.prototype.hasOwnProperty.call(bindingNoTop.parameters, 'top'),
    'applyRebindParams should remove top parameter when max-results is empty/invalid');
}


function testSearchWorkflowOrchestrator() {
  const mod = loadSapModule('util/SearchWorkflowOrchestrator.js');

  assert(Array.isArray(mod.normalizeRows({ results: [1, 2] })), 'normalizeRows should return array');
  assert(mod.normalizeRows({ results: [1, 2] }).length === 2, 'normalizeRows should parse OData v2 result');
  assert(mod.normalizeRows({ value: [3] }).length === 1, 'normalizeRows should parse OData v4 value');

  const rows = [
    { has_failed_checks: true, has_failed_barriers: false },
    { has_failed_checks: false, has_failed_barriers: true },
    { has_failed_checks: false, has_failed_barriers: false }
  ];
  const kpi = mod.buildKpis(rows, 10);
  assert(kpi.visible === 3, 'buildKpis visible should match rows length');
  assert(kpi.total === 10, 'buildKpis total should respect override');
  assert(kpi.failedChecks === 1, 'buildKpis should count failed checks');
  assert(kpi.failedBarriers === 1, 'buildKpis should count failed barriers');
  assert(kpi.healthy === 1, 'buildKpis should count healthy rows');

  const metrics = mod.buildMetrics(rows);
  assert(metrics.kpi.workflowStage === 'ANALYZE', 'buildMetrics should expose workflow stage ANALYZE when rows present');
}

function testChecklistValidationService() {
  const mod = loadSapModule('util/ChecklistValidationService.js');

  const checklist = {
    basic: {
      date: '2026-02-01',
      time: '09:00',
      timezone: 'UTC+3',
      OBSERVER_FULLNAME: 'A',
      OBSERVED_FULLNAME: 'B',
      LOCATION_KEY: 'L1',
      LPC_KEY: 'LPC100',
      PROF_KEY: 'P1'
    },
    checks: [{ id: 'c1', result: true }],
    barriers: [{ id: 'b1', result: true }]
  };

  const validRes = mod.validateForStatusChange(checklist);
  assert(validRes.valid === true, 'validateForStatusChange should return valid for full checklist');

  const invalidRes = mod.validateForStatusChange({ basic: {}, checks: [] });
  assert(invalidRes.valid === false, 'validateForStatusChange should fail when fields/checks missing');
  assert(invalidRes.hasAtLeastOneCheck === false, 'validateForStatusChange should detect missing checks');

  assert(mod.isBarrierSectionAllowed('lpc000') === false, 'isBarrierSectionAllowed should disable lpc000');
  assert(mod.isBarrierSectionAllowed('lpc777') === true, 'isBarrierSectionAllowed should allow non-blocked lpc');

  assert(mod.buildOverallResult({ checks: [{ result: true }], barriers: [{ result: true }] }) === true,
    'buildOverallResult should be true when all passed');
  assert(mod.buildOverallResult({ checks: [{ result: true }], barriers: [{ result: false }] }) === false,
    'buildOverallResult should be false when at least one failed');
}

function testDeltaPayloadBuilder() {
  const mod = loadSapModule('util/DeltaPayloadBuilder.js');

  const base = {
    root: { id: '1', version_number: 1, changed_on: 'old', status: 'DRAFT' },
    basic: { OBSERVER_FULLNAME: 'A' },
    checks: [{ id: 'c1', result: false }],
    barriers: [{ id: 'b1', result: true }]
  };
  const current = {
    root: { id: '1', version_number: 2, changed_on: 'new', status: 'REGISTERED' },
    basic: { OBSERVER_FULLNAME: 'A2' },
    checks: [{ id: 'c1', result: true }, { id: 'c2', result: false }],
    barriers: []
  };

  const delta = mod.buildDeltaPayload(current, base);
  assert(delta !== null, 'buildDeltaPayload should produce delta for changed data');
  assert(delta.root.status === 'REGISTERED', 'buildDeltaPayload should include changed root fields');
  assert(!('version_number' in delta.root), 'buildDeltaPayload should exclude technical fields from root delta');
  assert(delta.basic.OBSERVER_FULLNAME === 'A2', 'buildDeltaPayload should include basic field diff');
  assert(delta.checks.some((r) => r.id === 'c1' && r.edit_mode === 'U'), 'buildDeltaPayload should mark updated rows');
  assert(delta.checks.some((r) => r.id === 'c2' && r.edit_mode === 'C'), 'buildDeltaPayload should mark created rows');
  assert(delta.barriers.some((r) => r.id === 'b1' && r.edit_mode === 'D'), 'buildDeltaPayload should mark deleted rows');
}




function testSearchPresentationUseCase() {
  const mod = loadSapModule('service/usecase/SearchPresentationUseCase.js');
  const bundle = { getText: (k) => k };

  assert(mod.normalizeMaxRowsInput(' 0 ') === '1', 'normalizeMaxRowsInput should clamp low values to 1');
  assert(mod.normalizeMaxRowsInput(' 42 ') === '42', 'normalizeMaxRowsInput should keep valid values');
  assert(mod.normalizeMaxRowsInput(' 100000 ') === '9999', 'normalizeMaxRowsInput should clamp max value');

  assert(mod.formatOverallResultText(true, 'REGISTERED', bundle) === 'statusOk', 'formatOverallResultText should map true to i18n ok');
  assert(mod.formatOverallResultText(false, 'REGISTERED', bundle) === 'statusFailed', 'formatOverallResultText should map false to i18n failed');
  assert(mod.formatOverallResultState(null, 'REGISTERED') === 'Warning', 'formatOverallResultState should map REGISTERED to Warning');
  assert(mod.formatStatus('CRITICAL') === 'Error', 'formatStatus should map CRITICAL to Error');
}


function testDetailToolbarValidationUseCase() {
  const ChecklistUiState = { normalizeStatus: (s) => String(s || '').toUpperCase() };
  const mod = loadSapModule('service/usecase/DetailToolbarValidationUseCase.js', {
    'sap_ui5/util/ChecklistUiState': ChecklistUiState
  });

  const viewState = {};
  const viewModel = { setProperty: (k, v) => { viewState[k] = v; } };
  mod.applyValidationState(viewModel, { valid: false, missingPaths: ['a', 'b'], hasAtLeastOneCheck: false }, (a) => ({ count: a.length }));
  assert(viewState['/validationShown'] === true, 'applyValidationState should toggle validationShown for invalid checklist');
  assert(viewState['/validationMissing'].count === 2, 'applyValidationState should apply mapped missing paths');

  assert(mod.resolveValidationWarningCount({ missingPaths: ['a'], hasAtLeastOneCheck: false }) === 2,
    'resolveValidationWarningCount should include missing checks marker');

  const selectedState = {};
  const selectedModel = { setProperty: (k, v) => { selectedState[k] = v; } };
  const appState = {};
  const stateModel = { setProperty: (k, v) => { appState[k] = v; } };
  mod.markDirtyStatusAndNormalize(selectedModel, stateModel, 'registered');
  assert(selectedState['/root/status'] === 'REGISTERED', 'markDirtyStatusAndNormalize should normalize and set status');
  assert(appState['/isDirty'] === true, 'markDirtyStatusAndNormalize should mark state as dirty');
}


function testDetailSaveErrorPresentationUseCase() {
  let conflictCalls = 0;
  const DetailSaveConflictFlowUseCase = {
    buildConflictHandler: (cfg) => {
      return (choice) => {
        conflictCalls += 1;
        if (choice === cfg.reloadLabel) {
          return cfg.onReload();
        }
        if (choice === cfg.overwriteLabel) {
          return cfg.onOverwrite();
        }
        return Promise.resolve(null);
      };
    }
  };

  const mod = loadSapModule('service/usecase/DetailSaveErrorPresentationUseCase.js', {
    'sap_ui5/service/usecase/DetailSaveConflictFlowUseCase': DetailSaveConflictFlowUseCase
  });

  let reloadCalls = 0;
  let overwriteCalls = 0;
  let backendCalls = 0;

  return mod.handleSaveError({
    host: { id: 'H1' },
    error: new Error('conflict'),
    handleBackendError: (host, err, adapter) => {
      backendCalls += 1;
      return adapter.onConflictChoice('reload').then(() => adapter.onConflictChoice('cancel'));
    },
    reloadLabel: 'reload',
    overwriteLabel: 'overwrite',
    onReload: () => { reloadCalls += 1; return Promise.resolve('reloaded'); },
    onOverwrite: () => { overwriteCalls += 1; return Promise.resolve('overwritten'); }
  }).then((res) => {
    assert(res.ok === false && res.reason === 'cancelled' && res.result === null,
      'handleSaveError should normalize cancelled conflict flow outcome');
    assert(backendCalls === 1, 'handleSaveError should call backend error handler once');
    assert(conflictCalls === 2 && reloadCalls === 1 && overwriteCalls === 0,
      'createConflictAdapter should route reload and keep abort decision side-effect free');

    const normalizedLegacy = mod.normalizeHandledResult('reload', { reloadLabel: 'reload', overwriteLabel: 'overwrite' });
    assert(normalizedLegacy.ok === true && normalizedLegacy.reason === 'legacy_reload',
      'normalizeHandledResult should map legacy reload label to structured outcome');

    const normalizedError = mod.normalizeHandledResult(new Error('resolved-backend-error'), { reloadLabel: 'reload', overwriteLabel: 'overwrite' });
    assert(normalizedError.ok === false && normalizedError.reason === 'backend_error' && normalizedError.error.message === 'resolved-backend-error',
      'normalizeHandledResult should map resolved Error objects to backend_error outcome');

    return mod.handleSaveError({
      host: { id: 'H2' },
      error: new Error('network-error'),
      handleBackendError: () => Promise.resolve(new Error('backend-returned-error-object')),
      reloadLabel: 'reload',
      overwriteLabel: 'overwrite',
      onReload: () => Promise.resolve(),
      onOverwrite: () => Promise.resolve()
    });
  }).then((backendResolvedError) => {
    assert(backendResolvedError.ok === false && backendResolvedError.reason === 'backend_error' && backendResolvedError.error && backendResolvedError.error.message === 'backend-returned-error-object',
      'handleSaveError should normalize resolved Error objects from backend adapter to backend_error outcome');

    return mod.handleSaveError({
      host: { id: 'H3' },
      error: new Error('network-error'),
      handleBackendError: () => Promise.reject(new Error('backend-failed')),
      reloadLabel: 'reload',
      overwriteLabel: 'overwrite',
      onReload: () => Promise.resolve(),
      onOverwrite: () => Promise.resolve()
    });
  }).then((backendFailure) => {
    assert(backendFailure.ok === false && backendFailure.reason === 'backend_error' && backendFailure.error && backendFailure.error.message === 'backend-failed',
      'handleSaveError should normalize backend adapter rejections to backend_error outcome');
  });
}


function testDetailEditOrchestrationFreshnessFailure() {
  const mod = loadSapModule('service/usecase/DetailEditOrchestrationUseCase.js');
  let recoverCalls = 0;
  let acquireCalls = 0;
  let acquireFailedCalls = 0;

  return mod.runToggleEditFlow({
    editMode: true,
    isDirty: false,
    shouldPromptBeforeDisableEdit: () => false,
    isCancelDecision: () => false,
    confirmUnsaved: () => Promise.resolve('SAVE'),
    runPendingRelease: () => Promise.resolve(),
    runPendingToggle: (fn) => fn(),
    releaseEdit: () => Promise.resolve(),
    ensureFreshBeforeEdit: () => Promise.reject(new Error('freshness check failed')),
    confirmIntegrationEdit: () => Promise.resolve(true),
    onStayReadOnly: () => {},
    acquireLock: () => { acquireCalls += 1; return Promise.resolve({ success: true }); },
    onLockAcquired: () => {},
    tryRecoverFromAcquireError: () => { recoverCalls += 1; return Promise.resolve(false); },
    onAcquireFailed: () => { acquireFailedCalls += 1; return Promise.resolve('freshness-failed'); }
  }).then((res) => {
    assert(res === 'freshness-failed', 'runToggleEditFlow should route freshness failures to onAcquireFailed');
    assert(acquireCalls === 0, 'runToggleEditFlow should not attempt acquireLock when freshness check fails');
    assert(recoverCalls === 0 && acquireFailedCalls === 1,
      'runToggleEditFlow should not try recovery for pre-acquire failures and should call onAcquireFailed once');
  });
}

function testDetailEditOrchestrationRetryExhaustion() {
  const mod = loadSapModule('service/usecase/DetailEditOrchestrationUseCase.js');
  let recoverCalls = 0;
  let acquireFailedCalls = 0;

  return mod.runToggleEditFlow({
    editMode: true,
    isDirty: false,
    shouldPromptBeforeDisableEdit: () => false,
    isCancelDecision: () => false,
    confirmUnsaved: () => Promise.resolve('SAVE'),
    runPendingRelease: () => Promise.resolve(),
    runPendingToggle: (fn) => fn(),
    releaseEdit: () => Promise.resolve(),
    ensureFreshBeforeEdit: () => Promise.resolve(),
    confirmIntegrationEdit: () => Promise.resolve(true),
    onStayReadOnly: () => {},
    acquireLock: () => Promise.reject(new Error('acquire failed')),
    onLockAcquired: () => {},
    tryRecoverFromAcquireError: () => { recoverCalls += 1; return Promise.resolve(false); },
    onAcquireFailed: () => { acquireFailedCalls += 1; return Promise.resolve('failed'); }
  }).then((res) => {
    assert(res === 'failed', 'runToggleEditFlow should return acquire-failed result when recovery is exhausted');
    assert(recoverCalls === 1 && acquireFailedCalls === 1,
      'runToggleEditFlow should try recovery once and then call onAcquireFailed once');
  });
}

function testDetailDialogLifecycleUseCase() {
  const Fragment = {
    load: () => Promise.resolve({
      opened: 0,
      open() { this.opened += 1; },
      destroy() { this.destroyed = true; }
    })
  };
  const mod = loadSapModule('service/usecase/DetailDialogLifecycleUseCase.js', {
    'sap/ui/core/Fragment': Fragment
  });

  const host = {};
  const view = {
    getId: () => 'V',
    addDependent: () => {}
  };

  return mod.openDialog({ host, prop: '_pDialog', fragment: 'frag', view, controller: {} }).then((dialog) => {
    assert(dialog.opened === 1, 'openDialog should open loaded dialog');
    mod.destroyDialogsByIds({
      ids: ['x'],
      byId: () => dialog,
      host,
      props: ['_pDialog']
    });
    assert(host._pDialog === null, 'destroyDialogsByIds should clear host promise ref');
  });
}

function testSmartSearchAdapterFilterModes() {
  const mod = loadSapModule('service/SmartSearchAdapter.js');

  const data = [
    {
      root: { id: 'A-100', successRateChecks: 90, successRateBarriers: 100 },
      basic: { LPC_KEY: 'L1' }
    },
    {
      root: { id: 'B-200', hasFailedChecks: false, hasFailedBarriers: true },
      basic: { LPC_KEY: 'L2' }
    },
    {
      root: { id: 'C-300', hasFailedChecks: true, hasFailedBarriers: false },
      basic: { LPC_KEY: 'L3' }
    }
  ];

  const exact = mod.filterData(data, {
    filterId: 'A',
    filterLpc: 'L1',
    filterFailedChecks: 'TRUE',
    filterFailedBarriers: 'FALSE'
  }, 'EXACT');
  assert(exact.length === 1 && exact[0].root.id === 'A-100', 'filterData EXACT should apply active filters as AND');

  const loose = mod.filterData(data, {
    filterId: 'B',
    filterLpc: 'L3',
    filterFailedChecks: 'FALSE',
    filterFailedBarriers: 'TRUE'
  }, 'LOOSE');
  assert(loose.length === 2, 'filterData LOOSE should apply active filters as OR');

  const checksTrueByRate = mod.filterData(data, {
    filterId: '',
    filterLpc: '',
    filterFailedChecks: 'TRUE',
    filterFailedBarriers: 'ALL'
  }, 'EXACT');
  assert(checksTrueByRate.some((r) => r.root.id === 'A-100'), 'filterFailedChecks TRUE should fallback to successRateChecks when hasFailedChecks absent');

  const checksFalse = mod.filterData(data, {
    filterId: '',
    filterLpc: '',
    filterFailedChecks: 'FALSE',
    filterFailedBarriers: 'ALL'
  }, 'EXACT');
  assert(checksFalse.some((r) => r.root.id === 'B-200'), 'filterFailedChecks FALSE should respect hasFailedChecks=false');

  const barriersTrueByRate = mod.filterData(data, {
    filterId: '',
    filterLpc: '',
    filterFailedChecks: 'ALL',
    filterFailedBarriers: 'TRUE'
  }, 'EXACT');
  assert(barriersTrueByRate.some((r) => r.root.id === 'B-200'), 'filterFailedBarriers TRUE should respect hasFailedBarriers=true');

  const barriersFalseByRate = mod.filterData(data, {
    filterId: '',
    filterLpc: '',
    filterFailedChecks: 'ALL',
    filterFailedBarriers: 'FALSE'
  }, 'EXACT');
  assert(barriersFalseByRate.some((r) => r.root.id === 'A-100'), 'filterFailedBarriers FALSE should fallback to successRateBarriers when hasFailedBarriers absent');
}

function testSearchActionUseCase() {
  const mod = loadSapModule('service/usecase/SearchActionUseCase.js');

  assert(mod.extractSelectedChecklistId({ root: { id: 'ZX-1' } }) === 'ZX-1',
    'extractSelectedChecklistId should return trimmed root id');

  const payload = mod.buildAnalyticsPayload({
    filterId: '1',
    filterLpc: 'LPC',
    filterFailedChecks: 'TRUE',
    filterFailedBarriers: 'ALL',
    ignored: 'x'
  });
  assert(!('ignored' in payload), 'buildAnalyticsPayload should keep only analytics fields');

  const rows = mod.buildExportRowsFromVisible([{ root: { id: 'A', status: 'DRAFT' }, basic: { checklist_id: 'C1', OBSERVER_FULLNAME: 'O' } }]);
  assert(rows.length === 1 && rows[0].id === 'A', 'buildExportRowsFromVisible should map visible rows');

  assert(mod.shouldProceedAfterUnsavedDecision('SAVE') === true, 'shouldProceedAfterUnsavedDecision should allow SAVE');
  assert(mod.shouldProceedAfterUnsavedDecision('CANCEL') === false, 'shouldProceedAfterUnsavedDecision should block CANCEL');
}

function testDetailCommandFlowUseCase() {
  const mod = loadSapModule('service/usecase/DetailCommandFlowUseCase.js');

  assert(mod.shouldPromptBeforeClose(true) === true, 'shouldPromptBeforeClose should require prompt for dirty state');
  assert(mod.shouldPromptBeforeClose(false) === false, 'shouldPromptBeforeClose should skip prompt when clean');

  assert(mod.shouldProceedAfterUnsavedDecision('DISCARD') === true, 'shouldProceedAfterUnsavedDecision should allow DISCARD');
  assert(mod.shouldProceedAfterUnsavedDecision('CANCEL') === false, 'shouldProceedAfterUnsavedDecision should block CANCEL');

  assert(mod.shouldPromptBeforeDisableEdit(false, true) === true, 'shouldPromptBeforeDisableEdit should prompt on disable+dirty');
  assert(mod.shouldPromptBeforeDisableEdit(true, true) === false, 'shouldPromptBeforeDisableEdit should not prompt when enabling edit');

  assert(mod.isCancelDecision('CANCEL') === true, 'isCancelDecision should detect cancel');
}


function testSearchUiFlowUseCase() {
  const mod = loadSapModule('service/usecase/SearchUiFlowUseCase.js');

  const event = {
    getParameter: (k) => (k === 'listItem' ? {
      getBindingContext: () => ({ getObject: () => ({ root: { id: 'SEL-1' } }) })
    } : null),
    getSource: () => null
  };
  assert(mod.extractIdFromListSelectionEvent(event) === 'SEL-1', 'extractIdFromListSelectionEvent should extract root.id');

  const menuEvent = { getParameter: () => ({ data: () => 'all' }) };
  assert(mod.resolveExportEntityFromMenuEvent(menuEvent, 'screen') === 'all', 'resolveExportEntityFromMenuEvent should read menu entity');
  assert(mod.hasDialog({}) === true, 'hasDialog should detect existing dialog');
}


function testDetailSaveErrorOutcomePresentationUseCase() {
  const mod = loadSapModule('service/usecase/DetailSaveErrorOutcomePresentationUseCase.js');

  assert(mod.resolveMessageKey('reloaded') === 'saveConflictReloaded',
    'resolveMessageKey should map reloaded outcome');
  assert(mod.resolveMessageKey('missing_overwrite_handler') === 'saveConflictHandlerMissing',
    'resolveMessageKey should map missing-handler outcome');
  assert(mod.resolveMessageKey('unknown') === '',
    'resolveMessageKey should return empty key for unknown outcomes');

  const bundle = {
    getText: (k, params) => ({
      saveConflictCancelled: 'Save cancelled by user.',
      genericOperationFailed: `Operation failed: ${(params || [])[0] || ''}`
    }[k] || '')
  };
  const messages = [];

  const presented = mod.presentOutcome({
    result: { reason: 'cancelled' },
    bundle,
    showToast: (text) => messages.push(text)
  });
  assert(presented.ok === true && presented.reason === 'presented' && presented.messageKey === 'saveConflictCancelled' && messages[0] === 'Save cancelled by user.',
    'presentOutcome should return structured presented outcome for cancelled branch');

  const unknown = mod.presentOutcome({ result: { reason: 'unknown' }, bundle, showToast: () => {} });
  assert(unknown.ok === false && unknown.reason === 'unknown_outcome',
    'presentOutcome should return structured unknown_outcome for unmapped branches');

  const missingAdapter = mod.presentOutcome({ result: { reason: 'cancelled' }, bundle, showToast: null });
  assert(missingAdapter.ok === false && missingAdapter.reason === 'missing_toast_adapter',
    'presentOutcome should return missing_toast_adapter when toast callback is absent');

  const missingBundle = mod.presentOutcome({ result: { reason: 'cancelled' }, bundle: null, showToast: () => {} });
  assert(missingBundle.ok === false && missingBundle.reason === 'missing_bundle_adapter',
    'presentOutcome should return missing_bundle_adapter when bundle adapter is absent');


  const backendPresented = mod.presentOutcome({
    result: { reason: 'backend_error', error: new Error('backend-failed') },
    bundle,
    showToast: (text) => messages.push(text)
  });
  assert(backendPresented.ok === true && backendPresented.messageKey === 'genericOperationFailed' && messages[messages.length - 1] === 'Operation failed: backend-failed',
    'presentOutcome should map backend_error to genericOperationFailed message with error param');
}

function testDetailSaveConflictUseCase() {
  const mod = loadSapModule('service/usecase/DetailSaveConflictUseCase.js');

  let reloaded = 0;
  let overwritten = 0;
  return mod.handleConflictChoice('reload', {
    reloadLabel: 'reload',
    overwriteLabel: 'overwrite',
    onReload: () => { reloaded += 1; return Promise.resolve('reloaded'); },
    onOverwrite: () => { overwritten += 1; return Promise.resolve('overwritten'); }
  }).then((reloadRes) => {
    assert(reloaded === 1 && overwritten === 0 && reloadRes.ok === true && reloadRes.reason === 'reloaded',
      'handleConflictChoice should dispatch reload and return reloaded outcome');

    return mod.handleConflictChoice('cancel', {
      reloadLabel: 'reload',
      overwriteLabel: 'overwrite',
      onReload: () => Promise.resolve(),
      onOverwrite: () => Promise.resolve()
    });
  }).then((cancelRes) => {
    assert(cancelRes.ok === false && cancelRes.reason === 'cancelled',
      'handleConflictChoice should return cancelled outcome on unknown choice');

    return mod.handleConflictChoice('overwrite', {
      reloadLabel: 'reload',
      overwriteLabel: 'overwrite',
      onReload: null,
      onOverwrite: null
    });
  }).then((missingHandlerRes) => {
    assert(missingHandlerRes.ok === false && missingHandlerRes.reason === 'missing_overwrite_handler',
      'handleConflictChoice should return missing handler outcome when overwrite adapter is absent');
  });
}


async function testDetailEditOrchestrationUseCase() {
  const mod = loadSapModule('service/usecase/DetailEditOrchestrationUseCase.js');

  let flowCalled = 0;
  await mod.runToggleEditFlow({
    editMode: false,
    isDirty: true,
    shouldPromptBeforeDisableEdit: () => true,
    isCancelDecision: () => false,
    confirmUnsaved: () => Promise.resolve('SAVE'),
    runPendingRelease: () => { flowCalled += 1; return Promise.resolve(); },
    runPendingToggle: () => Promise.resolve(),
    releaseEdit: () => Promise.resolve(),
    ensureFreshBeforeEdit: () => Promise.resolve(),
    confirmIntegrationEdit: () => Promise.resolve(true),
    onStayReadOnly: () => {},
    acquireLock: () => Promise.resolve({ success: true }),
    onLockAcquired: () => {},
    tryRecoverFromAcquireError: () => Promise.resolve(false),
    onAcquireFailed: () => Promise.resolve()
  });
  assert(flowCalled === 1, 'runToggleEditFlow should execute pending release when disabling dirty edit after confirm');
}


function testSearchAnalyticsExportUseCase() {
  const mod = loadSapModule('service/usecase/SearchAnalyticsExportUseCase.js');

  let opened = 0;
  let closed = 0;
  mod.openDialog({ open: () => { opened += 1; } }, () => { opened += 1; });
  mod.closeDialog({ close: () => { closed += 1; } });
  assert(opened === 2, 'openDialog should open dialog and run callback');
  assert(closed === 1, 'closeDialog should close dialog');

  return mod.buildExportPromise('screen', () => [{ id: '1' }], () => Promise.resolve({ rows: [] })).then((oRes) => {
    assert(Array.isArray(oRes.rows) && oRes.rows.length === 1, 'buildExportPromise should use screen rows for screen entity');
    return mod.buildExportPromise('barrier', () => [{ id: 'screen' }], () => Promise.resolve({ rows: [{ id: 'remote-1' }] }));
  }).then((oRemote) => {
    assert(Array.isArray(oRemote.rows) && oRemote.rows[0].id === 'remote-1',
      'buildExportPromise should route non-screen entities to remote export promise');
  });
}




function testDetailCloseFlowUseCase() {
  const mod = loadSapModule('service/usecase/DetailCloseFlowUseCase.js');
  let proceeded = 0;

  return mod.runCloseFlow({
    isDirty: true,
    shouldPromptBeforeClose: () => true,
    shouldProceedAfterUnsavedDecision: (d) => d !== 'CANCEL',
    confirmUnsaved: () => Promise.resolve('DISCARD'),
    proceed: () => { proceeded += 1; }
  }).then((ok) => {
    assert(ok === true && proceeded === 1, 'runCloseFlow should proceed for non-cancel decisions');
    return mod.runCloseFlow({
      isDirty: true,
      shouldPromptBeforeClose: () => true,
      shouldProceedAfterUnsavedDecision: (d) => d !== 'CANCEL',
      confirmUnsaved: () => Promise.resolve('CANCEL'),
      proceed: () => { proceeded += 1; }
    });
  }).then((ok2) => {
    assert(ok2 === false && proceeded === 1, 'runCloseFlow should stop on cancel decision');
  });
}

function testDetailLockReleaseUseCaseNegative() {
  const DetailLifecycleUseCase = {
    setReadUnlocked: () => {}
  };
  const mod = loadSapModule('service/usecase/DetailLockReleaseUseCase.js', {
    'sap_ui5/service/usecase/DetailLifecycleUseCase': DetailLifecycleUseCase
  });

  let idleCalls = 0;
  return mod.runReleaseFlow({
    stateModel: {},
    releaseLock: () => Promise.reject(new Error('release failed')),
    setLockUiIdle: () => { idleCalls += 1; }
  }).then(() => {
    assert(false, 'runReleaseFlow should propagate release errors');
  }).catch(() => {
    assert(idleCalls === 1, 'runReleaseFlow should set idle state even when release fails');
  });
}

function testDetailSaveConflictFlowUseCase() {
  let called = 0;
  const DetailSaveConflictUseCase = {
    handleConflictChoice: (choice, cfg) => {
      called += 1;
      return choice === cfg.reloadLabel ? cfg.onReload() : cfg.onOverwrite();
    }
  };
  const mod = loadSapModule('service/usecase/DetailSaveConflictFlowUseCase.js', {
    'sap_ui5/service/usecase/DetailSaveConflictUseCase': DetailSaveConflictUseCase
  });

  let reloaded = 0;
  let overwritten = 0;
  const mActions = mod.createConflictActions({
    reloadLabel: 'reload',
    overwriteLabel: 'overwrite',
    onReload: () => { reloaded += 1; return Promise.resolve(); },
    onOverwrite: () => { overwritten += 1; return Promise.resolve(); }
  });
  assert(mActions.reloadLabel === 'reload' && mActions.overwriteLabel === 'overwrite', 'createConflictActions should preserve labels');

  const fn = mod.buildConflictHandler(mActions);
  return fn('reload').then(() => fn('overwrite')).then(() => {
    assert(called === 2 && reloaded === 1 && overwritten === 1, 'buildConflictHandler should route both reload and overwrite branches');
  });
}


function testDetailSaveConflictFlowUseCasePropagation() {
  const DetailSaveConflictUseCase = {
    handleConflictChoice: (choice, cfg) => {
      if (choice === cfg.reloadLabel) {
        return cfg.onReload();
      }
      return cfg.onOverwrite();
    }
  };
  const mod = loadSapModule('service/usecase/DetailSaveConflictFlowUseCase.js', {
    'sap_ui5/service/usecase/DetailSaveConflictUseCase': DetailSaveConflictUseCase
  });

  const fn = mod.buildConflictHandler({
    reloadLabel: 'reload',
    overwriteLabel: 'overwrite',
    onReload: () => Promise.reject(new Error('reload-failed')),
    onOverwrite: () => Promise.resolve()
  });

  return fn('reload').then(() => {
    assert(false, 'buildConflictHandler should propagate reload errors');
  }).catch((e) => {
    assert(String(e.message).indexOf('reload-failed') >= 0, 'buildConflictHandler should preserve underlying reload error');
  });
}

function testDetailSaveSuccessFlowUseCase() {
  const DetailLifecycleUseCase = {
    keepEditModeAfterSave: () => {}
  };
  const mod = loadSapModule('service/usecase/DetailSaveSuccessFlowUseCase.js', {
    'sap_ui5/service/usecase/DetailLifecycleUseCase': DetailLifecycleUseCase
  });

  const dataState = {};
  const dataModel = { setProperty: (k, v) => { dataState[k] = v; } };
  let selected = null;
  const selectedModel = { setData: (v) => { selected = v; } };
  let dispatched = 0;
  let toasted = 0;
  const stateChanges = {};
  const stateModel = {
    setProperty: (k, v) => { stateChanges[k] = v; }
  };

  const result = {
    checkLists: [{ id: '1' }],
    savedChecklist: { root: { id: '1' } }
  };

  mod.applySaveSuccess({
    result,
    dataModel,
    selectedModel,
    stateModel,
    dispatchFullSave: () => { dispatched += 1; },
    showSavedToast: () => { toasted += 1; }
  });

  assert(Array.isArray(dataState['/checkLists']) && dataState['/visibleCheckLists'].length === 1,
    'applySaveSuccess should write checklist collections');
  assert(selected && selected.root.id === '1', 'applySaveSuccess should refresh selected model');
  assert(stateChanges['/activeObjectId'] === '1' && stateChanges['/copySourceId'] === null && stateChanges['/objectAction'] === '',
    'applySaveSuccess should normalize state object references after save');
  assert(dispatched === 1 && toasted === 1, 'applySaveSuccess should call post-save hooks once');
}

function testDetailLockEditFlowUseCase() {
  const DetailEditOrchestrationUseCase = {
    runToggleEditFlow: (args) => Promise.resolve(args)
  };
  const DetailCommandFlowUseCase = {
    shouldPromptBeforeDisableEdit: () => true,
    isCancelDecision: () => false
  };
  const mod = loadSapModule('service/usecase/DetailLockEditFlowUseCase.js', {
    'sap_ui5/service/usecase/DetailEditOrchestrationUseCase': DetailEditOrchestrationUseCase,
    'sap_ui5/service/usecase/DetailCommandFlowUseCase': DetailCommandFlowUseCase
  });

  return mod.runToggleEditFlow({
    editMode: true,
    isDirty: true,
    confirmUnsaved: () => Promise.resolve('SAVE'),
    runPendingRelease: () => Promise.resolve(),
    runPendingToggle: (fn) => fn(),
    releaseEdit: () => Promise.resolve(),
    ensureFreshBeforeEdit: () => Promise.resolve(),
    confirmIntegrationEdit: () => Promise.resolve(true),
    onStayReadOnly: () => {},
    acquireLock: () => Promise.resolve({ success: true }),
    onLockAcquired: () => {},
    tryRecoverFromAcquireError: () => Promise.resolve(false),
    onAcquireFailed: () => Promise.resolve()
  }).then((args) => {
    assert(typeof args.shouldPromptBeforeDisableEdit === 'function', 'DetailLockEditFlowUseCase should inject disable-prompt policy');
    assert(typeof args.isCancelDecision === 'function', 'DetailLockEditFlowUseCase should inject cancel policy');
  });
}

function testDetailStatusRowUseCase() {
  const ChecklistUiState = { isSameStatus: (a, b) => String(a || '').toUpperCase() === String(b || '').toUpperCase() };
  const mod = loadSapModule('service/usecase/DetailStatusRowUseCase.js', {
    'sap_ui5/util/ChecklistUiState': ChecklistUiState
  });

  const v = mod.formatInfoCardValue('location', { locationName: 'L1', locationText: 'Shop' });
  assert(v === 'L1 — Shop', 'formatInfoCardValue should format location composite');

  assert(mod.shouldApplyStatusChange('DRAFT', 'REGISTERED') === true, 'shouldApplyStatusChange should allow different status');
  assert(mod.shouldApplyStatusChange('DRAFT', 'draft') === false, 'shouldApplyStatusChange should block same status');
  assert(mod.requiresIntegrationConfirmation({ this_is_integration_data: true }) === true, 'requiresIntegrationConfirmation should detect integration flag');
  assert(mod.shouldSyncAfterDeleteResult({ deleted: true }) === true, 'shouldSyncAfterDeleteResult should check delete flag');
  assert(mod.resolveExpandedDialogMeta('checks').prop === '_pChecksDialog', 'resolveExpandedDialogMeta should map checks dialog');
}


function testDetailStatusCommandUseCase() {
  const mod = loadSapModule('service/usecase/DetailStatusCommandUseCase.js');

  let applied = 0;
  return mod.runStatusChangeFlow({
    targetStatus: 'REGISTERED',
    validateChecklist: () => Promise.resolve(true),
    getSelectedRoot: () => ({ status: 'DRAFT', this_is_integration_data: false }),
    shouldApplyStatusChange: () => true,
    requiresIntegrationConfirmation: () => false,
    confirmIntegrationEdit: () => Promise.resolve(true),
    applyStatusAndSave: () => { applied += 1; return Promise.resolve(); }
  }).then(() => {
    assert(applied === 1, 'runStatusChangeFlow should apply and save when status change is allowed');

    let synced = 0;
    const didSync = mod.handleDeleteRowResult({ deleted: true }, (o) => !!o.deleted, () => { synced += 1; });
    assert(didSync === true && synced === 1, 'handleDeleteRowResult should sync when predicate allows');
  });
}

function testSearchAnalyticsDialogExportFlowUseCase() {
  const mod = loadSapModule('service/usecase/SearchAnalyticsDialogExportFlowUseCase.js');

  let opened = 0;
  let loaded = 0;
  let closed = 0;
  mod.openAnalyticsDialog({ open: () => { opened += 1; } }, () => { loaded += 1; });
  mod.closeAnalyticsDialog({ close: () => { closed += 1; } });
  assert(opened === 1 && loaded === 1 && closed === 1, 'analytics dialog open/close helpers should work');

  let success = 0;
  return mod.runExportFlow({
    runWithLoading: (fn) => fn(),
    buildExportPromise: () => Promise.resolve({ rows: [{ id: '1' }] }),
    onEmpty: () => {},
    onSuccess: () => { success += 1; },
    onError: () => {}
  }).then(() => {
    assert(success === 1, 'runExportFlow should call onSuccess for non-empty rows');
  });
}

function testSearchAnalyticsDialogExportFlowUseCaseNegative() {
  const mod = loadSapModule('service/usecase/SearchAnalyticsDialogExportFlowUseCase.js');
  let empty = 0;
  let errors = 0;

  return mod.runExportFlow({
    runWithLoading: (fn) => fn(),
    buildExportPromise: () => Promise.resolve({ rows: [] }),
    onEmpty: () => { empty += 1; },
    onSuccess: () => {},
    onError: () => {}
  }).then(() => {
    return mod.runExportFlow({
      runWithLoading: (fn) => fn(),
      buildExportPromise: () => Promise.reject(new Error('export failed')),
      onEmpty: () => {},
      onSuccess: () => {},
      onError: () => { errors += 1; }
    });
  }).then(() => {
    assert(empty === 1, 'runExportFlow should call onEmpty for empty export rows');
    assert(errors === 1, 'runExportFlow should call onError for rejected export promise');
  });
}


function testDetailRowDialogCommandUseCase() {
  const mod = loadSapModule('service/usecase/DetailRowDialogCommandUseCase.js');
  assert(mod.resolveRowPath('checks') === '/checks', 'resolveRowPath should map checks path');
  assert(mod.resolveExpandedDialogId('barriers') === 'barriersExpandedDialog', 'resolveExpandedDialogId should map barriers dialog id');
  assert(mod.shouldProcessRowDeleteResult({ deleted: true }) === true, 'shouldProcessRowDeleteResult should respect deleted flag');
}

function testDetailExpandedRowsFlowUseCase() {
  const DetailStatusRowUseCase = {
    resolveExpandedDialogMeta: (type) => type === 'checks'
      ? { prop: '_pChecksDialog', fragment: 'sap_ui5.view.fragment.ChecksExpandedDialog' }
      : { prop: '_pBarriersDialog', fragment: 'sap_ui5.view.fragment.BarriersExpandedDialog' },
    shouldSyncAfterDeleteResult: (res) => !!(res && res.deleted)
  };
  const DetailDialogLifecycleUseCase = {
    openDialog: (args) => Promise.resolve({ openedWith: args })
  };
  const DetailRowDialogCommandUseCase = {
    resolveRowPath: (type) => type === 'checks' ? '/checks' : '/barriers',
    resolveExpandedDialogId: (type) => type === 'checks' ? 'checksExpandedDialog' : 'barriersExpandedDialog',
    shouldProcessRowDeleteResult: (res) => !!(res && res.deleted)
  };
  const DetailStatusCommandUseCase = {
    handleDeleteRowResult: (res, shouldSync, sync) => {
      if (!shouldSync(res)) {
        return false;
      }
      sync();
      return true;
    }
  };
  const RowListHelper = {
    addRow: (rows) => (rows || []).concat([{ id: 'new-row' }])
  };

  const mod = loadSapModule('service/usecase/DetailExpandedRowsFlowUseCase.js', {
    'sap_ui5/service/usecase/DetailStatusRowUseCase': DetailStatusRowUseCase,
    'sap_ui5/service/usecase/DetailDialogLifecycleUseCase': DetailDialogLifecycleUseCase,
    'sap_ui5/service/usecase/DetailRowDialogCommandUseCase': DetailRowDialogCommandUseCase,
    'sap_ui5/service/usecase/DetailStatusCommandUseCase': DetailStatusCommandUseCase,
    'sap_ui5/util/RowListHelper': RowListHelper
  });

  let closeCalls = 0;
  mod.closeExpandedDialogByType({ type: 'checks', byId: () => ({ close: () => { closeCalls += 1; } }) });
  assert(closeCalls === 1, 'closeExpandedDialogByType should close mapped dialog');

  const selectedState = { '/checks': [{ id: 'c1' }] };
  const selectedModel = {
    getProperty: (path) => selectedState[path],
    setProperty: (path, value) => { selectedState[path] = value; }
  };
  let syncCalls = 0;
  mod.addRowByType({ type: 'checks', selectedModel, onMutated: () => { syncCalls += 1; } });
  assert(selectedState['/checks'].length === 2 && syncCalls === 1,
    'addRowByType should mutate rows and trigger sync callback');

  const deleteResult = mod.deleteRowByType({
    event: { id: 'E' },
    type: 'checks',
    deleteRowFromEvent: () => ({ deleted: true }),
    onSyncSelectionMeta: () => { syncCalls += 1; }
  });
  assert(deleteResult.deleted === true && syncCalls === 2,
    'deleteRowByType should propagate delete result and sync selected meta');

  return mod.openExpandedDialogByType({
    type: 'checks',
    host: {},
    view: {},
    controller: {}
  }).then((dialog) => {
    assert(dialog.openedWith.prop === '_pChecksDialog', 'openExpandedDialogByType should pass checks dialog metadata');
  });
}

function testDetailLocationValueHelpUseCase() {
  const mod = loadSapModule('service/usecase/DetailLocationValueHelpUseCase.js');

  let opened = 0;
  let closed = 0;
  const dialog = { open: () => { opened += 1; }, close: () => { closed += 1; } };
  const viewModel = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  const selectedModel = { m: {}, setProperty(k, v) { this.m[k] = v; } };

  const rows = [
    { node_id: 'ROOT', parent_id: '', location_name: 'Root' },
    { node_id: 'A-1', parent_id: 'ROOT', location_name: 'Assembly' },
    { node_id: 'B-2', parent_id: 'ROOT', location_name: 'Buffer' }
  ];
  const buildTree = (flat) => (flat || []).map((r) => ({ node_id: r.node_id, parent_id: r.parent_id, location_name: r.location_name }));
  const norm = (v) => String(v || '').trim().toLowerCase();

  const openedOk = mod.openValueHelp({ dialog, locations: rows, viewModel, buildLocationTree: buildTree });
  assert(openedOk === true && opened === 1, 'openValueHelp should open dialog and initialize tree');
  assert(Array.isArray(viewModel.m['/locationVhTree']) && viewModel.m['/locationVhTree'].length === 3,
    'openValueHelp should hydrate location value-help tree');

  const filteredTree = mod.buildFilteredLocationTree({ query: 'assem', locations: rows, buildLocationTree: buildTree, normalizeText: norm });
  assert(filteredTree.length >= 1, 'buildFilteredLocationTree should keep matching nodes and ancestors');

  const appliedTree = mod.applyFilteredTreeToViewModel({
    query: 'zzz',
    locations: rows,
    viewModel,
    buildLocationTree: buildTree,
    normalizeText: norm
  });
  assert(Array.isArray(appliedTree) && appliedTree.length === 0,
    'applyFilteredTreeToViewModel should support empty-result filter branch');

  const node = mod.resolveNodeFromTreeSelectionEvent({
    getParameter: (name) => name === 'rowContext' ? { getObject: () => rows[1] } : null
  });
  assert(node && node.node_id === 'A-1', 'resolveNodeFromTreeSelectionEvent should resolve row context node');

  const comboNode = mod.resolveNodeFromComboChangeEvent({
    getParameter: (name) => name === 'selectedItem'
      ? { getBindingContext: () => ({ getObject: () => rows[2] }) }
      : null
  }, 'mpl');
  assert(comboNode && comboNode.node_id === 'B-2', 'resolveNodeFromComboChangeEvent should resolve selected item node');

  let afterApply = 0;
  const applied = mod.applyLocationSelection({ node, selectedModel, onAfterApply: () => { afterApply += 1; } });
  assert(applied === true && afterApply === 1, 'applyLocationSelection should update selected model and invoke callback');
  assert(selectedModel.m['/basic/LOCATION_KEY'] === 'A-1', 'applyLocationSelection should map node_id to LOCATION_KEY');

  const failedApply = mod.applyLocationSelection({ node: null, selectedModel });
  assert(failedApply === false, 'applyLocationSelection should return false for invalid node input');

  const closedOk = mod.closeValueHelp({ dialog });
  assert(closedOk === true && closed === 1, 'closeValueHelp should close dialog');
}

function testDetailDictionarySelectionUseCase() {
  const mod = loadSapModule('service/usecase/DetailDictionarySelectionUseCase.js');

  const selectedKey = mod.resolveSelectedKey({
    getParameter: (name) => name === 'selectedItem' ? { getKey: () => 'L2' } : null,
    getSource: () => ({ getSelectedKey: () => 'fallback' })
  });
  assert(selectedKey === 'L2', 'resolveSelectedKey should prefer selected item key when available');

  const fallbackKey = mod.resolveSelectedKey({
    getParameter: () => null,
    getSource: () => ({ getSelectedKey: () => 'P1' })
  });
  assert(fallbackKey === 'P1', 'resolveSelectedKey should fallback to source selected key');

  const selectedState = {};
  const applied = mod.applyDictionarySelection({
    key: 'L2',
    dictionary: [{ key: 'L1', text: 'Local-1' }, { key: 'L2', text: 'Local-2' }],
    keyPath: '/basic/LPC_KEY',
    textPath: '/basic/LPC_TEXT',
    selectedModel: { setProperty: (path, value) => { selectedState[path] = value; } }
  });
  assert(applied === true && selectedState['/basic/LPC_TEXT'] === 'Local-2',
    'applyDictionarySelection should map key and text from dictionary');

  const shouldConfirm = mod.shouldConfirmBarrierReset({ barrierAllowed: false, barriers: [{ id: 1 }] });
  assert(shouldConfirm === true, 'shouldConfirmBarrierReset should require confirmation when barriers would be lost');
  assert(mod.shouldConfirmBarrierReset({ barrierAllowed: true, barriers: [{ id: 1 }] }) === false,
    'shouldConfirmBarrierReset should skip confirmation when barriers are allowed');

  const lpcState = {};
  mod.applyLpcDecision({
    confirmed: false,
    selectedModel: { setProperty: (path, value) => { lpcState[path] = value; } }
  });
  assert(lpcState['/basic/LPC_KEY'] === '' && lpcState['/basic/LPC_TEXT'] === '',
    'applyLpcDecision should rollback LPC fields when user rejects barrier cleanup');

  const barrierState = {};
  mod.applyLpcDecision({
    confirmed: true,
    selectedModel: { setProperty: (path, value) => { barrierState[path] = value; } }
  });
  assert(Array.isArray(barrierState['/barriers']) && barrierState['/barriers'].length === 0,
    'applyLpcDecision should clear barriers when user confirms cleanup');
}


function testDetailLpcBarrierWarningFlowUseCase() {
  const mod = loadSapModule('service/usecase/DetailLpcBarrierWarningFlowUseCase.js', {
    'sap_ui5/service/usecase/DetailDictionarySelectionUseCase': loadSapModule('service/usecase/DetailDictionarySelectionUseCase.js')
  });

  assert(mod.shouldPromptWarning({ barrierAllowed: false, barriers: [{ id: 1 }] }) === true,
    'shouldPromptWarning should require prompt when barriers exist and LPC disallows barriers');
  assert(mod.shouldPromptWarning({ barrierAllowed: true, barriers: [{ id: 1 }] }) === false,
    'shouldPromptWarning should skip prompt when barriers are allowed');
  assert(mod.shouldPromptWarning({ barrierAllowed: false, barriers: [] }) === false,
    'shouldPromptWarning should skip prompt when there are no barriers to reset');

  const selectedStateReject = {};
  const appliedReject = mod.applyWarningDecision({
    confirmed: false,
    selectedModel: { setProperty: (path, value) => { selectedStateReject[path] = value; } }
  });
  assert(appliedReject === true && selectedStateReject['/basic/LPC_KEY'] === '' && selectedStateReject['/basic/LPC_TEXT'] === '',
    'applyWarningDecision should rollback LPC fields when warning is rejected');

  const selectedStateConfirm = {};
  let afterApplyCalls = 0;
  const appliedConfirm = mod.applyWarningDecision({
    confirmed: true,
    selectedModel: { setProperty: (path, value) => { selectedStateConfirm[path] = value; } },
    onAfterApply: () => { afterApplyCalls += 1; }
  });
  assert(appliedConfirm === true && Array.isArray(selectedStateConfirm['/barriers']) && selectedStateConfirm['/barriers'].length === 0 && afterApplyCalls === 1,
    'applyWarningDecision should clear barriers and trigger callback when confirmed');

  assert(mod.applyWarningDecision({ confirmed: true, selectedModel: null }) === false,
    'applyWarningDecision should guard missing selected model');

  let warningCalls = 0;
  const messageBox = {
    Action: { YES: 'YES', NO: 'NO' },
    warning: (text, opts) => {
      warningCalls += 1;
      opts.onClose('YES');
    }
  };
  const openState = {};
  return mod.openWarningDialog({
    messageBox,
    promptText: 'barriers-warn',
    barrierAllowed: false,
    barriers: [{ id: 1 }],
    selectedModel: { setProperty: (path, value) => { openState[path] = value; } }
  }).then((opened) => {
    assert(opened === true && warningCalls === 1 && Array.isArray(openState['/barriers']) && openState['/barriers'].length === 0,
      'openWarningDialog should show warning and apply confirmed decision branch');
    return mod.openWarningDialog({
      messageBox,
      promptText: 'noop',
      barrierAllowed: true,
      barriers: [{ id: 1 }],
      selectedModel: { setProperty: () => {} }
    }).then((noOpen) => {
      assert(noOpen === false,
        'openWarningDialog should short-circuit when prompt preconditions are not met');
    });
  });
}


function testDetailIntegrationEditWarningUseCase() {
  const mod = loadSapModule('service/usecase/DetailIntegrationEditWarningUseCase.js');

  const resolvedRoot = mod.resolveRoot({ selectedRoot: { id: 'SEL' }, dataRoot: { id: 'DATA' } });
  assert(resolvedRoot.id === 'SEL', 'resolveRoot should prefer selected root when present');
  const fallbackRoot = mod.resolveRoot({ selectedRoot: null, dataRoot: { id: 'DATA' } });
  assert(fallbackRoot.id === 'DATA', 'resolveRoot should fallback to data root when selected root is missing');

  assert(mod.isIntegrationRoot({ this_is_integration_data: true }) === true,
    'isIntegrationRoot should detect integration root by this_is_integration_data');
  assert(mod.isIntegrationRoot({ integrationFlag: true }) === true,
    'isIntegrationRoot should detect integration root by integrationFlag');
  assert(mod.isIntegrationRoot({}) === false,
    'isIntegrationRoot should return false for non-integration root');

  assert(mod.shouldPrompt({ selectedRoot: { this_is_integration_data: true } }) === true,
    'shouldPrompt should require warning for integration root');
  assert(mod.shouldPrompt({ selectedRoot: { this_is_integration_data: false } }) === false,
    'shouldPrompt should skip warning for non-integration root');

  return mod.confirmIntegrationEdit({
    selectedRoot: { this_is_integration_data: false },
    messageBox: null,
    bundle: { getText: (k) => k }
  }).then((allowedWithoutPrompt) => {
    assert(allowedWithoutPrompt === true,
      'confirmIntegrationEdit should allow edit immediately when warning is not required');

    return mod.confirmIntegrationEdit({
      selectedRoot: { this_is_integration_data: true },
      messageBox: null,
      bundle: { getText: (k) => k }
    }).then((blockedWithoutAdapter) => {
      assert(blockedWithoutAdapter === false,
        'confirmIntegrationEdit should fail-safe block edit when warning is required but adapter missing');

      const messageBoxYes = {
        Action: { YES: 'YES', NO: 'NO' },
        warning: (_text, opts) => opts.onClose('YES')
      };
      return mod.confirmIntegrationEdit({
        selectedRoot: { integrationFlag: true },
        messageBox: messageBoxYes,
        bundle: { getText: (k) => k }
      }).then((allowedByYes) => {
        assert(allowedByYes === true,
          'confirmIntegrationEdit should allow edit when warning dialog resolves YES');

        const messageBoxNo = {
          Action: { YES: 'YES', NO: 'NO' },
          warning: (_text, opts) => opts.onClose('NO')
        };
        return mod.confirmIntegrationEdit({
          selectedRoot: { integrationFlag: true },
          messageBox: messageBoxNo,
          bundle: { getText: (k) => k }
        }).then((blockedByNo) => {
          assert(blockedByNo === false,
            'confirmIntegrationEdit should block edit when warning dialog resolves NO');
        });
      });
    });
  });
}


function testDetailUnsavedDecisionFlowUseCase() {
  const mod = loadSapModule('service/usecase/DetailUnsavedDecisionFlowUseCase.js');

  assert(mod.shouldProceedAfterDecision('SAVE') === true,
    'shouldProceedAfterDecision should allow SAVE decision');
  assert(mod.shouldProceedAfterDecision('DISCARD') === true,
    'shouldProceedAfterDecision should allow DISCARD decision');
  assert(mod.shouldProceedAfterDecision('CANCEL') === false,
    'shouldProceedAfterDecision should block CANCEL decision');

  return mod.requestUnsavedDecision({
    host: {},
    onSave: () => Promise.resolve(),
    confirmUnsavedAndHandle: () => Promise.resolve('SAVE')
  }).then((saveDecision) => {
    assert(saveDecision === 'SAVE',
      'requestUnsavedDecision should resolve with adapter decision result');

    return mod.requestUnsavedDecision({
      host: {},
      onSave: null,
      confirmUnsavedAndHandle: null
    }).then((fallbackDecision) => {
      assert(fallbackDecision === 'CANCEL',
        'requestUnsavedDecision should fail-safe to CANCEL when adapter is missing');

      return mod.requestUnsavedDecision({
        host: {},
        onSave: () => Promise.resolve(),
        confirmUnsavedAndHandle: () => Promise.reject(new Error('dialog failed'))
      }).then((rejectedDecision) => {
        assert(rejectedDecision === 'CANCEL',
          'requestUnsavedDecision should fail-safe to CANCEL when adapter promise rejects');

        var cleanProceeded = 0;
        return mod.runUnsavedCloseFlow({
          isDirty: false,
          proceed: () => { cleanProceeded += 1; }
        }).then((cleanResult) => {
          assert(cleanResult.proceeded === true && cleanResult.decision === 'CLEAN' && cleanProceeded === 1,
            'runUnsavedCloseFlow should short-circuit clean branch and proceed immediately');

          var dirtyProceeded = 0;
          return mod.runUnsavedCloseFlow({
            isDirty: true,
            host: {},
            onSave: () => Promise.resolve(),
            confirmUnsavedAndHandle: () => Promise.resolve('CANCEL'),
            proceed: () => { dirtyProceeded += 1; }
          }).then((cancelResult) => {
            assert(cancelResult.proceeded === false && cancelResult.decision === 'CANCEL' && dirtyProceeded === 0,
              'runUnsavedCloseFlow should block proceed on CANCEL decision');

            var saveProceeded = 0;
            return mod.runUnsavedCloseFlow({
              isDirty: true,
              host: {},
              onSave: () => Promise.resolve(),
              confirmUnsavedAndHandle: () => Promise.resolve('SAVE'),
              proceed: () => { saveProceeded += 1; }
            }).then((saveResult) => {
              assert(saveResult.proceeded === true && saveResult.decision === 'SAVE' && saveProceeded === 1,
                'runUnsavedCloseFlow should proceed on SAVE decision');
            });
          });
        });
      });
    });
  });
}


function testDetailCloseNavigationFlowUseCase() {
  const mod = loadSapModule('service/usecase/DetailCloseNavigationFlowUseCase.js');

  const ctx = mod.resolveCloseContext({
    stateModel: { getProperty: (path) => path === '/activeObjectId' ? 'CHK-1' : 'S-1' }
  });
  assert(ctx.objectId === 'CHK-1' && ctx.sessionId === 'S-1',
    'resolveCloseContext should read active object id and session id from state model');

  let prepareCalls = 0;
  let navigateCalls = 0;
  return mod.runCloseNavigation({
    stateModel: { getProperty: () => '' },
    prepareCloseNavigation: () => { prepareCalls += 1; },
    navigateToSearch: () => { navigateCalls += 1; }
  }).then((skipResult) => {
    assert(skipResult.skippedRelease === true && skipResult.navigated === true && prepareCalls === 1 && navigateCalls === 1,
      'runCloseNavigation should skip release and still navigate when active object id is missing');

    let releaseCalls = 0;
    return mod.runCloseNavigation({
      stateModel: { getProperty: (path) => path === '/activeObjectId' ? 'CHK-2' : 'S-2' },
      releaseLock: () => { releaseCalls += 1; return Promise.resolve(); },
      prepareCloseNavigation: () => { prepareCalls += 1; },
      navigateToSearch: () => { navigateCalls += 1; }
    }).then((okResult) => {
      assert(okResult.released === true && okResult.navigated === true && releaseCalls === 1,
        'runCloseNavigation should release lock and navigate on success');

      let errorCalls = 0;
      return mod.runCloseNavigation({
        stateModel: { getProperty: (path) => path === '/activeObjectId' ? 'CHK-3' : 'S-3' },
        releaseLock: () => Promise.reject(new Error('release-failed')),
        prepareCloseNavigation: () => { prepareCalls += 1; },
        navigateToSearch: () => { navigateCalls += 1; },
        onReleaseError: () => { errorCalls += 1; }
      }).then((errorResult) => {
        assert(errorResult.releaseError === true && errorResult.navigated === true && errorCalls === 1,
          'runCloseNavigation should fallback to navigation when release fails and report release error');

        return mod.runCloseNavigation({
          stateModel: { getProperty: (path) => path === '/activeObjectId' ? 'CHK-4' : 'S-4' },
          releaseLock: () => Promise.resolve(),
          prepareCloseNavigation: () => {}
        }).then((missingRouterResult) => {
          assert(missingRouterResult.navigated === false,
            'runCloseNavigation should report non-navigated branch when navigate adapter is missing');
        });
      });
    });
  });
}

function testDetailPersonSuggestionUseCase() {
  const mod = loadSapModule('service/usecase/DetailPersonSuggestionUseCase.js');

  const persons = [
    { perner: '10001', fullName: 'Alex Stone', position: 'Inspector', orgUnit: 'Q1', integrationName: 'INT-A' },
    { perner: '10001', fullName: 'Alex Stone', position: 'Inspector', orgUnit: 'Q1', integrationName: 'INT-A' },
    { perner: '10002', fullName: 'Mira Lane', position: 'Observer', orgUnit: 'Q3', integrationName: 'INT-C' }
  ];
  const normalize = (v) => String(v || '').trim().toLowerCase();

  const filtered = mod.filterSuggestions({
    query: 'alex',
    persons,
    normalizeText: normalize,
    limit: 10
  });
  assert(filtered.length === 1 && filtered[0].perner === '10001',
    'filterSuggestions should apply contains filter and dedupe duplicated person rows');

  assert(mod.shouldFetchRemoteSuggestions({ query: 'ax', localCount: 1, minLocalBeforeRemote: 3 }) === true,
    'shouldFetchRemoteSuggestions should request remote when local matches are below threshold');
  assert(mod.shouldFetchRemoteSuggestions({ query: '', localCount: 0, minLocalBeforeRemote: 3 }) === false,
    'shouldFetchRemoteSuggestions should skip remote for empty queries');

  const viewState = {};
  const appliedView = mod.applySuggestionsToViewModel({
    target: 'observed',
    suggestions: filtered,
    viewModel: { setProperty: (path, value) => { viewState[path] = value; } }
  });
  assert(appliedView === true && (viewState['/observedSuggestions'] || []).length === 1,
    'applySuggestionsToViewModel should map observed target to observed suggestions path');

  const resolved = mod.resolvePersonFromSuggestionEvent({
    getParameter: (name) => name === 'selectedItem'
      ? { getBindingContext: () => ({ getObject: () => filtered[0] }) }
      : null,
    getSource: () => ({ data: () => 'observer' })
  });
  assert(resolved.target === 'observer' && resolved.person && resolved.person.perner === '10001',
    'resolvePersonFromSuggestionEvent should resolve both target and person from selected item context');

  const selectedState = {};
  const appliedSelection = mod.applyPersonSelection({
    target: resolved.target,
    person: resolved.person,
    selectedModel: { setProperty: (path, value) => { selectedState[path] = value; } }
  });
  assert(appliedSelection === true && selectedState['/basic/OBSERVER_FULLNAME'] === 'Alex Stone',
    'applyPersonSelection should write observer fields to selected model');
}


function testSearchRetryLoadPresentationUseCase() {
  const mod = loadSapModule('service/usecase/SearchRetryLoadPresentationUseCase.js');

  assert(Array.isArray(mod.normalizeRows(null)) && mod.normalizeRows(null).length === 0,
    'normalizeRows should fallback non-array input to empty array');

  const state = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  const data = { m: {}, setProperty(k, v) { this.m[k] = v; } };

  const resetOk = mod.resetLoadErrorState(state);
  assert(resetOk === true && state.m['/loadError'] === false,
    'resetLoadErrorState should clear load error flags');

  let afterApplyCalls = 0;
  const successOk = mod.applyLoadSuccess({
    dataModel: data,
    rows: [{ id: 1 }],
    onAfterApply: () => { afterApplyCalls += 1; }
  });
  assert(successOk === true && (data.m['/visibleCheckLists'] || []).length === 1 && afterApplyCalls === 1,
    'applyLoadSuccess should map rows to both data collections and invoke callback');

  const errorOk = mod.applyLoadError({
    stateModel: state,
    error: new Error('retry-failed')
  });
  assert(errorOk === true && state.m['/loadError'] === true && state.m['/loadErrorMessage'] === 'retry-failed',
    'applyLoadError should set load error flags and message');

  return mod.runRetryFlow({
    stateModel: state,
    dataModel: data,
    getCheckLists: null
  }).then((missingLoaderResult) => {
    assert(missingLoaderResult.ok === false && missingLoaderResult.reason === 'missing_loader',
      'runRetryFlow should fail-safe with missing_loader when loader callback is absent');

    return mod.runRetryFlow({
      stateModel: state,
      dataModel: data,
      getCheckLists: () => Promise.resolve([]),
      treatEmptyAsError: true,
      emptyRowsMessage: 'empty-after-retry'
    }).then((emptyRowsResult) => {
      assert(emptyRowsResult.ok === false && emptyRowsResult.reason === 'empty_rows' && state.m['/loadErrorMessage'] === 'empty-after-retry',
        'runRetryFlow should report empty_rows branch when retry returns no data');

      let failCalls = 0;
      return mod.runRetryFlow({
        stateModel: state,
        dataModel: data,
        getCheckLists: () => {
          failCalls += 1;
          return Promise.reject(new Error('still-failing'));
        },
        maxAttempts: 2
      }).then((repeatedFailResult) => {
        assert(repeatedFailResult.ok === false && repeatedFailResult.reason === 'error' && repeatedFailResult.attempts === 2 && failCalls === 2,
          'runRetryFlow should support repeated retry failures up to maxAttempts');

        return mod.runRetryFlow({
          stateModel: state,
          dataModel: data,
          getCheckLists: () => Promise.resolve([{ id: 'OK' }]),
          runWithLoading: (fn) => fn()
        }).then((okResult) => {
          assert(okResult.ok === true && (data.m['/checkLists'] || []).length === 1,
            'runRetryFlow should apply successful retry rows to data model');
        });
      });
    });
  });
}



function testSearchNavigationIntentUseCase() {
  const mod = loadSapModule('service/usecase/SearchNavigationIntentUseCase.js', {
    'sap_ui5/service/usecase/SearchActionUseCase': loadSapModule('service/usecase/SearchActionUseCase.js')
  });

  const createIntent = mod.buildCreateIntent();
  assert(createIntent.objectAction === 'CREATE' && createIntent.routeParams.id === '__create',
    'buildCreateIntent should build create route intent');

  const copyMissing = mod.buildCopyIntent({ selectedId: '' });
  assert(copyMissing === null, 'buildCopyIntent should return null when selected id is missing');

  const copyIntent = mod.buildCopyIntent({ selectedId: 'CHK-1' });
  assert(copyIntent && copyIntent.objectAction === 'COPY' && copyIntent.routeParams.id === 'CHK-1',
    'buildCopyIntent should build copy route intent');

  const openDetailMissing = mod.buildOpenDetailIntent({ id: '' });
  assert(openDetailMissing === null, 'buildOpenDetailIntent should return null when detail id is missing');

  const openDetailIntent = mod.buildOpenDetailIntent({ id: 'CHK-2' });
  assert(openDetailIntent && openDetailIntent.route === 'detail' && openDetailIntent.routeParams.id === 'CHK-2',
    'buildOpenDetailIntent should build detail route intent');

  const selectedId = mod.resolveSelectedId({
    selectedModel: { getData: () => ({ root: { id: 'CHK-3' } }) }
  });
  assert(selectedId === 'CHK-3', 'resolveSelectedId should extract selected checklist id via SearchActionUseCase');

  const stateModel = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  let navCalls = 0;
  const applied = mod.applyIntent({
    intent: copyIntent,
    stateModel,
    navTo: (_route, _params) => { navCalls += 1; }
  });
  assert(applied === true && stateModel.m['/layout'] === 'TwoColumnsMidExpanded' && stateModel.m['/objectAction'] === 'COPY' && navCalls === 1,
    'applyIntent should apply state and call nav adapter');

  assert(mod.applyIntent({ intent: copyIntent, stateModel, navTo: null }) === false,
    'applyIntent should fail gracefully when router adapter is missing');
}







function testSearchExportIntentGuardUseCase() {
  const mod = loadSapModule('service/usecase/SearchExportIntentGuardUseCase.js');

  assert(mod.normalizeEntity('check', 'screen', ['screen', 'check', 'barrier']) === 'check',
    'normalizeEntity should keep allowed entity');
  assert(mod.normalizeEntity('unknown', 'screen', ['screen', 'check', 'barrier']) === 'screen',
    'normalizeEntity should fallback for unknown entity');

  const menuIntent = mod.resolveEntityFromIntent({
    event: { getParameter: () => ({ data: (k) => k === 'entity' ? 'barrier' : null }) },
    defaultEntity: 'screen',
    allowedEntities: ['screen', 'barrier', 'check'],
    resolveEntityFromMenuEvent: (event, fallback) => {
      const item = event.getParameter('item');
      return item ? item.data('entity') : fallback;
    }
  });
  assert(menuIntent.entity === 'barrier' && menuIntent.fallbackUsed === false,
    'resolveEntityFromIntent should resolve menu-selected export entity');

  const fallbackIntent = mod.resolveEntityFromIntent({
    source: { data: () => 'unknown' },
    defaultEntity: 'screen',
    allowedEntities: ['screen', 'barrier', 'check']
  });
  assert(fallbackIntent.entity === 'screen' && fallbackIntent.fallbackUsed === true,
    'resolveEntityFromIntent should fallback for unsupported source entity');

  return mod.runExportIntent({
    defaultEntity: 'screen',
    runExport: null
  }).then((missingRunner) => {
    assert(missingRunner.ok === false && missingRunner.reason === 'missing_run_export',
      'runExportIntent should guard missing export runner');

    return mod.runExportIntent({
      defaultEntity: 'screen',
      isEnabled: () => false,
      runExport: () => Promise.resolve()
    }).then((disabled) => {
      assert(disabled.ok === false && disabled.reason === 'disabled',
        'runExportIntent should short-circuit when export is disabled');

      let runCalls = 0;
      return mod.runExportIntent({
        source: { data: () => 'check' },
        defaultEntity: 'screen',
        allowedEntities: ['screen', 'barrier', 'check'],
        isEnabled: () => true,
        runExport: (entity) => {
          runCalls += 1;
          assert(entity === 'check', 'runExportIntent should pass resolved entity to runner');
          return Promise.resolve();
        }
      }).then((applied) => {
        assert(applied.ok === true && applied.reason === 'applied' && runCalls === 1,
          'runExportIntent should apply export intent when adapters are valid');

        return mod.runExportIntent({
          defaultEntity: 'screen',
          isEnabled: () => true,
          runExport: () => Promise.reject(new Error('export-run-failed'))
        }).then((runError) => {
          assert(runError.ok === false && runError.reason === 'run_error',
            'runExportIntent should report runner errors');
        });
      });
    });
  });
}

function testSearchCreateCopyNavigationGuardUseCase() {
  const mod = loadSapModule('service/usecase/SearchCreateCopyNavigationGuardUseCase.js');

  return mod.runCreateNavigationFlow({
    confirmNavigation: () => Promise.resolve(true),
    buildCreateIntent: () => ({ route: 'detail', routeParams: { id: '__create' } }),
    applyIntent: null
  }).then((missingNav) => {
    assert(missingNav.ok === false && missingNav.reason === 'missing_nav_adapter',
      'runCreateNavigationFlow should guard missing nav adapter');

    return mod.runCreateNavigationFlow({
      confirmNavigation: () => Promise.resolve(false),
      buildCreateIntent: () => ({ route: 'detail', routeParams: { id: '__create' } }),
      applyIntent: () => true
    }).then((cancelled) => {
      assert(cancelled.ok === false && cancelled.reason === 'cancelled',
        'runCreateNavigationFlow should cancel on rejected unsaved decision');

      return mod.runCopyNavigationFlow({
        resolveSelectedId: () => '',
        confirmNavigation: () => Promise.resolve(true),
        buildCopyIntent: (id) => ({ route: 'detail', routeParams: { id } }),
        applyIntent: () => true
      }).then((missingSelection) => {
        assert(missingSelection.ok === false && missingSelection.reason === 'missing_selection',
          'runCopyNavigationFlow should guard missing selected id');

        return mod.runCopyNavigationFlow({
          resolveSelectedId: () => 'CHK-1',
          confirmNavigation: () => Promise.resolve(true),
          buildCopyIntent: () => null,
          applyIntent: () => true
        }).then((nullIntent) => {
          assert(nullIntent.ok === false && nullIntent.reason === 'null_intent',
            'runCopyNavigationFlow should guard null intent');

          return mod.runCopyNavigationFlow({
            resolveSelectedId: () => 'CHK-2',
            confirmNavigation: () => Promise.resolve(true),
            buildCopyIntent: (id) => ({ route: 'detail', routeParams: { id } }),
            applyIntent: () => false
          }).then((missingAdapter) => {
            assert(missingAdapter.ok === false && missingAdapter.reason === 'missing_nav_adapter',
              'runCopyNavigationFlow should guard missing nav adapter response');

            return mod.runCopyNavigationFlow({
              resolveSelectedId: () => 'CHK-3',
              confirmNavigation: () => Promise.resolve(true),
              buildCopyIntent: (id) => ({ route: 'detail', routeParams: { id } }),
              applyIntent: () => true
            }).then((applied) => {
              assert(applied.ok === true && applied.reason === 'applied' && applied.selectedId === 'CHK-3',
                'runCopyNavigationFlow should apply when all adapters are valid');
            });
          });
        });
      });
    });
  });
}

function testSearchOpenDetailGuardUseCase() {
  const mod = loadSapModule('service/usecase/SearchOpenDetailGuardUseCase.js');

  assert(mod.normalizeId('  A-1 ') === 'A-1', 'normalizeId should trim id values');

  return mod.runOpenDetailFlow({ id: '' }).then((missingId) => {
    assert(missingId.ok === false && missingId.reason === 'missing_id',
      'runOpenDetailFlow should return missing_id when id is empty');

    return mod.runOpenDetailFlow({
      id: 'CHK-1',
      confirmNavigation: () => Promise.resolve(false),
      buildIntent: (id) => ({ route: 'detail', routeParams: { id } }),
      applyIntent: () => true
    }).then((cancelled) => {
      assert(cancelled.ok === false && cancelled.reason === 'cancelled',
        'runOpenDetailFlow should return cancelled when unsaved decision rejects navigation');

      return mod.runOpenDetailFlow({
        id: 'CHK-2',
        confirmNavigation: () => Promise.resolve(true),
        buildIntent: () => null,
        applyIntent: () => true
      }).then((nullIntent) => {
        assert(nullIntent.ok === false && nullIntent.reason === 'null_intent',
          'runOpenDetailFlow should guard null intent branch');

        return mod.runOpenDetailFlow({
          id: 'CHK-3',
          confirmNavigation: () => Promise.resolve(true),
          buildIntent: (id) => ({ route: 'detail', routeParams: { id } }),
          applyIntent: () => false
        }).then((missingRouter) => {
          assert(missingRouter.ok === false && missingRouter.reason === 'missing_router_adapter',
            'runOpenDetailFlow should report missing router adapter when applyIntent returns false');

          return mod.runOpenDetailFlow({
            id: 'CHK-4',
            confirmNavigation: () => Promise.reject(new Error('confirm-failed')),
            buildIntent: (id) => ({ route: 'detail', routeParams: { id } }),
            applyIntent: () => true
          }).then((confirmError) => {
            assert(confirmError.ok === false && confirmError.reason === 'confirm_error',
              'runOpenDetailFlow should handle rejected confirm promise');

            return mod.runOpenDetailFlow({
              id: 'CHK-5',
              confirmNavigation: () => Promise.resolve(true),
              buildIntent: (id) => ({ route: 'detail', routeParams: { id } }),
              applyIntent: () => true
            }).then((applied) => {
              assert(applied.ok === true && applied.reason === 'applied' && applied.id === 'CHK-5',
                'runOpenDetailFlow should apply intent when all adapters are valid');
            });
          });
        });
      });
    });
  });
}

function testSearchSelectionHydrationUseCase() {
  const mod = loadSapModule('service/usecase/SearchSelectionHydrationUseCase.js');

  const missingIdModel = { data: { root: { id: 'STALE' } }, setData(v) { this.data = v; } };
  const viewMissing = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  return mod.runSelectionHydration({ id: '', selectedModel: missingIdModel, viewModel: viewMissing }).then((missingId) => {
    assert(missingId.ok === false && missingId.reason === 'missing_id' && viewMissing.m['/hasSelection'] === false,
      'runSelectionHydration should short-circuit when id is missing and clear selection flag');
    assert(Object.keys(missingIdModel.data || {}).length === 0,
      'runSelectionHydration should clear stale selected model data when id is missing');

    return mod.runSelectionHydration({ id: 'CHK-1', selectedModel: null }).then((missingModel) => {
      assert(missingModel.ok === false && missingModel.reason === 'missing_selected_model',
        'runSelectionHydration should guard missing selected model');

      const selected = { data: null, setData(v){ this.data = v; } };
      const view = { m: {}, setProperty(k,v){ this.m[k]=v; } };

      return mod.runSelectionHydration({
        id: 'CHK-2',
        selectedModel: selected,
        viewModel: view,
        loadChecklistById: () => Promise.resolve({ root: { id: 'CHK-2' }, basic: { checklist_id: 'X' } })
      }).then((okRes) => {
        assert(okRes.ok === true && okRes.reason === 'loaded' && selected.data.root.id === 'CHK-2' && view.m['/hasSelection'] === true,
          'runSelectionHydration should apply loaded checklist and mark selection');

        return mod.runSelectionHydration({
          id: 'CHK-3',
          selectedModel: selected,
          viewModel: view,
          loadChecklistById: () => Promise.resolve({ invalid: true })
        }).then((invalidShape) => {
          assert(invalidShape.ok === true && invalidShape.checklist.root.id === 'CHK-3',
            'runSelectionHydration should normalize invalid checklist shape to fallback object');

          return mod.runSelectionHydration({
            id: 'CHK-4',
            selectedModel: selected,
            viewModel: view,
            loadChecklistById: () => Promise.reject(new Error('network-failed'))
          }).then((failed) => {
            assert(failed.ok === false && failed.reason === 'load_error' && failed.checklist.root.id === 'CHK-4',
              'runSelectionHydration should fallback to id-only checklist when hydration promise rejects');
          });
        });
      });
    });
  });
}




function testSearchEmptyStatePresentationUseCase() {
  const mod = loadSapModule('service/usecase/SearchEmptyStatePresentationUseCase.js');

  assert(mod.resolveEmptyStateKind({ hasRows: true, loadError: false, useSmartControls: true }) === 'has_data',
    'resolveEmptyStateKind should resolve has_data for non-empty dataset');
  assert(mod.resolveEmptyStateKind({ hasRows: false, loadError: true, useSmartControls: true }) === 'load_error',
    'resolveEmptyStateKind should prioritize load_error when load flag is set');
  assert(mod.resolveEmptyStateKind({ hasRows: false, loadError: false, useSmartControls: false }) === 'smart_degraded_empty',
    'resolveEmptyStateKind should map smart-controls degraded empty branch');

  const bundle = { getText: (k) => ({ noDataDefault: 'No checklists found.', noDataLoadError: 'Unable to load data.' }[k]) };

  const missingView = mod.applyEmptyStatePresentation({
    viewModel: null,
    dataModel: { getProperty: () => [] },
    bundle
  });
  assert(missingView.ok === false && missingView.reason === 'missing_view_model',
    'applyEmptyStatePresentation should guard missing view model');

  const missingData = mod.applyEmptyStatePresentation({
    viewModel: { setProperty: () => {}, getProperty: () => false },
    dataModel: null,
    bundle
  });
  assert(missingData.ok === false && missingData.reason === 'missing_data_model',
    'applyEmptyStatePresentation should guard missing data model');

  const view = { m: { '/useSmartControls': false, '/loadError': false }, setProperty(k,v){ this.m[k]=v; }, getProperty(k){ return this.m[k]; } };
  const data = { getProperty: (k) => k === '/visibleCheckLists' ? [] : [] };
  const table = { text: '', setNoDataText(t){ this.text=t; } };

  const applied = mod.applyEmptyStatePresentation({
    viewModel: view,
    dataModel: data,
    bundle,
    table,
    unknownFallbackKey: 'unknown_key'
  });

  assert(applied.ok === true && applied.kind === 'smart_degraded_empty' && view.m['/noDataText'] === 'No checklists found.' && table.text === 'No checklists found.',
    'applyEmptyStatePresentation should apply no-data text for degraded empty branch');
}

function testSearchSummaryPresentationUseCase() {
  const mod = loadSapModule('service/usecase/SearchSummaryPresentationUseCase.js');

  assert(mod.normalizeCount(7.9) === 7 && mod.normalizeCount(-1) === 0,
    'normalizeCount should floor positives and clamp negatives');
  assert(mod.normalizeStage('review') === 'REVIEW' && mod.normalizeStage('unexpected') === 'DISCOVER',
    'normalizeStage should normalize allowed values and fallback unknown stage');

  const bundle = {
    getText: (key, params) => {
      if (key === 'resultSummary') {
        return `${(params || [])[0]} of ${(params || [])[1]}`;
      }
      throw new Error('missing-key');
    }
  };

  const missingModel = mod.applySummaryPresentation({
    viewModel: null,
    bundle,
    kpi: { visible: 1, total: 2 },
    visible: 1,
    total: 2
  });
  assert(missingModel.ok === false && missingModel.reason === 'missing_view_model',
    'applySummaryPresentation should guard missing view model');

  const view = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  const applied = mod.applySummaryPresentation({
    viewModel: view,
    bundle,
    kpi: { visible: 'bad', total: null, failedChecks: -3, failedBarriers: 2, healthy: 1.8, workflowStage: 'unknown' },
    visible: 'NaN-like',
    total: 5
  });

  assert(applied.ok === true && view.m['/workflowStage'] === 'DISCOVER' && view.m['/kpiFailedChecks'] === 0,
    'applySummaryPresentation should sanitize invalid counters and unknown stage');
  assert(view.m['/resultSummary'] === '0 of 5',
    'applySummaryPresentation should build result summary with sanitized counters');
}

function testSearchRetryMessagePresentationUseCase() {
  const mod = loadSapModule('service/usecase/SearchRetryMessagePresentationUseCase.js');

  const bundle = {
    getText: (key, params) => {
      const m = {
        retryLoadSuccess: `Data reloaded: ${(params || [])[0]} rows`,
        retryLoadEmpty: 'No rows loaded.',
        retryLoadUnavailable: 'Retry is unavailable.',
        retryLoadEmptyError: 'No rows returned from backend.',
        retryLoadFailed: `Retry failed: ${(params || [])[0] || ''}`,
        retryLoadFailedAfterRetries: `Retry failed after ${(params || [])[0]} attempts: ${(params || [])[1] || ''}`,
        loadErrorMessage: 'Fallback load error message'
      };
      if (!m[key]) {
        throw new Error('missing-key');
      }
      return m[key];
    }
  };

  const state = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  const toasts = [];

  const success = mod.presentRetryOutcome({
    result: { ok: true, rows: [{ id: 1 }, { id: 2 }] },
    bundle,
    stateModel: state,
    showToast: (t) => toasts.push(t)
  });
  assert(success.ok === true && success.message === 'Data reloaded: 2 rows' && state.m['/loadError'] === false,
    'presentRetryOutcome should show success toast and clear error banner');

  const repeatedError = mod.presentRetryOutcome({
    result: { ok: false, reason: 'error', attempts: 2, error: new Error('network') },
    bundle,
    stateModel: state,
    showToast: (t) => toasts.push(t)
  });
  assert(repeatedError.ok === true && state.m['/loadError'] === true && state.m['/loadErrorMessage'] === 'network',
    'presentRetryOutcome should set banner for repeated errors');

  const missingToast = mod.presentRetryOutcome({
    result: { ok: false, reason: 'missing_loader' },
    bundle,
    stateModel: state
  });
  assert(missingToast.ok === true && missingToast.reason === 'banner_only' && state.m['/loadError'] === true,
    'presentRetryOutcome should return banner_only outcome when toast adapter is absent but banner is applied');

  const toastErrorWithBanner = mod.presentRetryOutcome({
    result: { ok: false, reason: 'error', attempts: 1, error: new Error('timeout') },
    bundle,
    stateModel: state,
    showToast: () => { throw new Error('toast-failed'); }
  });
  assert(toastErrorWithBanner.ok === false && toastErrorWithBanner.reason === 'toast_error_banner_applied'
    && state.m['/loadError'] === true && state.m['/loadErrorMessage'] === 'timeout',
    'presentRetryOutcome should expose toast_error_banner_applied when banner is set but toast fails');

  const missingToastNoBanner = mod.presentRetryOutcome({
    result: { ok: true, rows: [{ id: 1 }] },
    bundle,
    stateModel: state
  });

  const missingStateModel = mod.presentRetryOutcome({
    result: { ok: false, reason: 'missing_loader' },
    bundle,
    showToast: (t) => toasts.push(t)
  });
  assert(missingStateModel.ok === false && missingStateModel.reason === 'missing_state_model_adapter',
    'presentRetryOutcome should fail deterministically when banner branch has no state model adapter');
  assert(missingToastNoBanner.ok === false && missingToastNoBanner.reason === 'missing_toast_adapter',
    'presentRetryOutcome should keep missing_toast_adapter for non-banner success branch without toast adapter');

  const unknownFallback = mod.presentRetryOutcome({
    result: { ok: false, reason: 'unknown_reason' },
    bundle,
    stateModel: state,
    showToast: (t) => toasts.push(t),
    unknownFallbackKey: 'notExistingKey'
  });
  assert(unknownFallback.ok === true && unknownFallback.message === 'Unexpected retry result',
    'presentRetryOutcome should fallback for unknown reason and missing bundle key');

  const stateApply = mod.applyBannerState({ stateModel: state, shouldSetBanner: true, bannerText: 'oops' });
  assert(stateApply.ok === true && stateApply.reason === 'applied',
    'applyBannerState should return structured applied outcome on valid state model');

  const stateMissing = mod.applyBannerState({ stateModel: null, shouldSetBanner: true, bannerText: 'oops' });
  assert(stateMissing.ok === false && stateMissing.reason === 'missing_state_model_adapter',
    'applyBannerState should return missing_state_model_adapter when state model adapter is absent');
}

function testSearchActionMessagePresentationUseCase() {
  const mod = loadSapModule('service/usecase/SearchActionMessagePresentationUseCase.js');

  const bundle = {
    getText: (key, params) => {
      const m = {
        checklistIdMissing: 'Checklist id not found',
        nothingToCopy: 'Select a checklist to copy.',
        deleted: 'Checklist deleted.',
        nothingToDelete: 'Select a checklist to delete.',
        deleteFailed: `Delete failed: ${(params || [])[0] || ''}`,
        exportEmpty: 'No data to export',
        exportFailed: `Export failed: ${(params || [])[0] || ''}`
      };
      if (!m[key]) {
        throw new Error('missing-key');
      }
      return m[key];
    }
  };

  const messages = [];
  const showToast = (text) => { messages.push(text); };

  assert(mod.presentMissingChecklistId({ bundle, showToast }) === true && messages[messages.length - 1] === 'Checklist id not found',
    'presentMissingChecklistId should show checklist-id-missing message');

  assert(mod.presentCopyMissingSelection({ bundle, showToast }) === true && messages[messages.length - 1] === 'Select a checklist to copy.',
    'presentCopyMissingSelection should show copy-missing-selection message');

  assert(mod.presentDeleteFlowResult({ bundle, showToast, result: { ok: true } }) === true && messages[messages.length - 1] === 'Checklist deleted.',
    'presentDeleteFlowResult should show deleted message for success');

  assert(mod.presentDeleteFlowResult({ bundle, showToast, result: { ok: false, reason: 'missing_id' } }) === true && messages[messages.length - 1] === 'Select a checklist to delete.',
    'presentDeleteFlowResult should show nothing-to-delete message for missing id');

  assert(mod.presentDeleteFlowResult({ bundle, showToast, result: { ok: false, reason: 'delete_error', error: new Error('boom') } }) === true
    && messages[messages.length - 1] === 'Delete failed: boom',
    'presentDeleteFlowResult should include backend error message in delete-failed toast');


  const disabledExport = mod.presentExportIntentResult({ bundle, showToast, result: { ok: false, reason: 'disabled' } });
  assert(disabledExport.ok === true && disabledExport.reason === 'presented_disabled' && disabledExport.messageKey === 'exportEmpty'
    && messages[messages.length - 1] === 'No data to export',
    'presentExportIntentResult should return presented_disabled and show export-empty message');

  const erroredExport = mod.presentExportIntentResult({ bundle, showToast, result: { ok: false, reason: 'run_error', error: new Error('x') } });
  assert(erroredExport.ok === true && erroredExport.reason === 'presented_error' && erroredExport.messageKey === 'exportFailed'
    && messages[messages.length - 1] === 'Export failed: x',
    'presentExportIntentResult should return presented_error and show export-failed message for run_error');

  const missingRunnerExport = mod.presentExportIntentResult({ bundle, showToast, result: { ok: false, reason: 'missing_run_export' } });
  assert(missingRunnerExport.ok === true && missingRunnerExport.reason === 'presented_error' && missingRunnerExport.messageKey === 'exportFailed'
    && messages[messages.length - 1] === 'Export failed: Unknown error',
    'presentExportIntentResult should return presented_error fallback for missing export runner');



  const failedDisabledPresentation = mod.presentExportIntentResult({
    bundle,
    showToast: () => { throw new Error('toast-failed'); },
    result: { ok: false, reason: 'disabled' }
  });
  assert(failedDisabledPresentation.ok === false && failedDisabledPresentation.reason === 'presentation_failed_disabled',
    'presentExportIntentResult should return presentation_failed_disabled when disabled toast presentation fails');

  const failedErrorPresentation = mod.presentExportIntentResult({
    bundle,
    showToast: () => { throw new Error('toast-failed'); },
    result: { ok: false, reason: 'run_error', error: new Error('boom') }
  });
  assert(failedErrorPresentation.ok === false && failedErrorPresentation.reason === 'presentation_failed_error',
    'presentExportIntentResult should return presentation_failed_error when error toast presentation fails');
  const unknownExportReason = mod.presentExportIntentResult({ bundle, showToast, result: { ok: false, reason: 'weird' } });
  assert(unknownExportReason.ok === false && unknownExportReason.reason === 'unknown_result_reason',
    'presentExportIntentResult should return unknown_result_reason for unsupported export outcomes');

  const noPresentation = mod.presentExportIntentResult({ bundle, showToast, result: { ok: true } });
  assert(noPresentation.ok === false && noPresentation.reason === 'no_presentation_needed',
    'presentExportIntentResult should return no_presentation_needed for successful export intents');
  const fallbackMessages = [];
  assert(mod.presentToastMessage({
    bundle: { getText: () => { throw new Error('bundle-broken'); } },
    showToast: (text) => { fallbackMessages.push(text); },
    messageKey: 'deleteFailed',
    fallbackText: 'fallback-delete-message'
  }) === true && fallbackMessages[0] === 'fallback-delete-message',
    'presentToastMessage should fallback when bundle adapter fails');

  assert(mod.presentToastMessage({
    bundle,
    showToast: () => { throw new Error('toast-adapter-failed'); },
    messageKey: 'deleted',
    fallbackText: 'fallback'
  }) === false,
    'presentToastMessage should return false when toast adapter throws');

  assert(mod.presentToastMessage({
    bundle: null,
    showToast,
    messageKey: 'unknown',
    fallbackText: ''
  }) === false,
    'presentToastMessage should return false for unknown message without fallback text');
}

function testSearchDeleteOrchestrationUseCase() {
  const mod = loadSapModule('service/usecase/SearchDeleteOrchestrationUseCase.js');

  return mod.runDeleteFlow({
    resolveSelectedId: () => ''
  }).then((missingId) => {
    assert(missingId.ok === false && missingId.reason === 'missing_id',
      'runDeleteFlow should short-circuit when selected id is missing');

    return mod.runDeleteFlow({
      resolveSelectedId: () => 'CHK-1'
    }).then((missingAdapter) => {
      assert(missingAdapter.ok === false && missingAdapter.reason === 'missing_delete_adapter',
        'runDeleteFlow should guard missing backend delete adapter');

      const trace = { rebind: 0, reload: 0, clear: 0 };
      let rowsApplied = null;

      return mod.runDeleteFlow({
        resolveSelectedId: () => 'CHK-2',
        runWithLoading: (fn) => fn(),
        deleteAndReload: (id) => Promise.resolve([{ root: { id: id } }]),
        applyRows: (rows) => { rowsApplied = rows; },
        rebind: () => { trace.rebind += 1; },
        applySelectedChecklist: (checklist) => { if (!checklist || Object.keys(checklist).length === 0) { trace.clear += 1; } },
        reloadSelectionState: () => { trace.reload += 1; }
      }).then((okResult) => {
        assert(okResult.ok === true && okResult.id === 'CHK-2' && (rowsApplied || []).length === 1,
          'runDeleteFlow should run delete lifecycle and propagate updated rows');
        assert(trace.rebind === 1 && trace.reload === 1 && trace.clear === 1,
          'runDeleteFlow should rebind, clear selection and reload selection state');

        return mod.runDeleteFlow({
          resolveSelectedId: () => 'CHK-3',
          runWithLoading: (fn) => fn(),
          deleteAndReload: () => Promise.reject(new Error('delete-failed'))
        }).then((errorResult) => {
          assert(errorResult.ok === false && errorResult.reason === 'delete_error' && errorResult.error.message === 'delete-failed',
            'runDeleteFlow should surface delete_error branch when backend delete rejects');
        });
      });
    });
  });
}

function testSearchToolbarActionStateUseCase() {
  const mod = loadSapModule('service/usecase/SearchToolbarActionStateUseCase.js', {
    'sap_ui5/service/usecase/SearchActionUseCase': loadSapModule('service/usecase/SearchActionUseCase.js')
  });

  const selectedModel = { getData: () => ({ root: { id: 'CHK-1' } }) };
  const dataModel = { getProperty: (path) => path === '/visibleCheckLists' ? [{ root: { id: 'CHK-1' } }] : [] };

  const state = mod.resolveActionState({
    selectedModel,
    dataModel,
    isLoading: false,
    useSmartControls: true
  });
  assert(state.hasSelection === true && state.canCopy === true && state.canDelete === true && state.canExport === true,
    'resolveActionState should enable toolbar actions when selection/rows are valid');

  const staleSelectionState = mod.resolveActionState({
    selectedModel,
    dataModel: { getProperty: () => [{ root: { id: 'DIFF' } }] },
    isLoading: false,
    useSmartControls: false
  });
  assert(staleSelectionState.hasSelection === false && staleSelectionState.canCopy === false,
    'resolveActionState should disable selection actions for stale selection in fallback mode');

  const loadingState = mod.resolveActionState({
    selectedModel,
    dataModel,
    isLoading: true,
    useSmartControls: true
  });
  assert(loadingState.canCopy === false && loadingState.canDelete === false && loadingState.canExport === false && loadingState.canRetryLoad === false,
    'resolveActionState should disable actions while loading');

  const view = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  const applied = mod.applyActionStateToViewModel({
    selectedModel,
    dataModel,
    viewModel: view,
    isLoading: false,
    useSmartControls: true
  });
  assert(applied === true && view.m['/hasSelection'] === true && view.m['/canExport'] === true,
    'applyActionStateToViewModel should write resolved action state to view model');

  assert(mod.applyActionStateToViewModel({ viewModel: null }) === false,
    'applyActionStateToViewModel should guard missing view model adapter');
}

function testSearchSmartCoordinatorMetadataDegradedMode() {
  const Filter = createFilterStub();
  const FilterOperator = { Contains: 'Contains', EQ: 'EQ' };
  const mod = loadSapModule('util/SearchSmartControlCoordinator.js', {
    'sap/ui/model/Filter': Filter,
    'sap/ui/model/FilterOperator': FilterOperator
  });

  const vm = { m: {}, getProperty(k){ return this.m[k]; }, setProperty(k,v){ this.m[k]=v; } };
  const state = { m: { '/mainServiceMetadataOk': false, '/mainServiceMetadataError': '' }, getProperty(k){ return this.m[k]; } };

  mod.syncAvailability({ stateModel: state, viewModel: vm, unavailableText: 'smartUnavailable', bootstrap: () => {} });
  assert(vm.m['/useSmartControls'] === false, 'syncAvailability should disable smart controls in degraded metadata mode');
  assert(vm.m['/smartControlsReason'] === 'smartUnavailable', 'syncAvailability should use fallback unavailable reason when metadata error is empty');
}

function testDetailStatusCommandUseCaseIntegrationRestriction() {
  const mod = loadSapModule('service/usecase/DetailStatusCommandUseCase.js');
  let applyCalls = 0;

  return mod.runStatusChangeFlow({
    targetStatus: 'REGISTERED',
    validateChecklist: () => Promise.resolve(true),
    getSelectedRoot: () => ({ status: 'DRAFT', this_is_integration_data: true }),
    shouldApplyStatusChange: () => true,
    requiresIntegrationConfirmation: () => true,
    confirmIntegrationEdit: () => Promise.resolve(false),
    applyStatusAndSave: () => { applyCalls += 1; return Promise.resolve(); }
  }).then((res) => {
    assert(res === null, 'runStatusChangeFlow should abort when integration confirmation is rejected');
    assert(applyCalls === 0, 'runStatusChangeFlow should not save when integration edit is not confirmed');
  });
}

function testSearchIntentUseCase() {
  const mod = loadSapModule('service/usecase/SearchIntentUseCase.js');
  const view = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  let rebound = 0;
  mod.markSearchedAndRebind(view, () => { rebound += 1; });
  assert(view.m['/hasSearched'] === true && rebound === 1, 'markSearchedAndRebind should set flag and rebind');

  const state = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  const applied = mod.applyStatusFilter(state, '/filterFailedChecks', 'TRUE', () => { rebound += 1; });
  assert(applied === true && state.m['/filterFailedChecks'] === 'TRUE', 'applyStatusFilter should write state and rebind');
}

function testSearchSelectionNavigationUseCase() {
  const mod = loadSapModule('service/usecase/SearchSelectionNavigationUseCase.js');

  const selectedModel = {
    data: {},
    setData(v) { this.data = v; },
    getData() { return this.data; }
  };
  const viewModel = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  const dataModel = {
    m: {
      '/visibleCheckLists': [{ root: { id: 'A-1' } }],
      '/checkLists': [{ root: { id: 'A-1' } }, { root: { id: 'B-2' } }]
    },
    getProperty(k) { return this.m[k]; }
  };

  mod.applySelectedChecklist({ checklist: { root: { id: 'A-1' } }, selectedModel, viewModel });
  assert(viewModel.m['/hasSelection'] === true, 'applySelectedChecklist should mark hasSelection for valid id');

  const persisted = mod.syncSelectionState({ selectedModel, dataModel, viewModel });
  assert(persisted === true && viewModel.m['/hasSelection'] === true,
    'syncSelectionState should keep selection when selected id exists in visible scope');

  dataModel.m['/visibleCheckLists'] = [{ root: { id: 'C-3' } }];
  const dropped = mod.syncSelectionState({ selectedModel, dataModel, viewModel });
  assert(dropped === false && viewModel.m['/hasSelection'] === false,
    'syncSelectionState should drop selection flag when selected id is not present after data refresh');

  assert(mod.shouldProceedAfterUnsavedDecision('CANCEL') === false,
    'shouldProceedAfterUnsavedDecision should block navigation for cancel decision');
  assert(mod.shouldProceedAfterUnsavedDecision('DISCARD') === true,
    'shouldProceedAfterUnsavedDecision should allow navigation for non-cancel decisions');

  const nav = mod.buildNavigationState('ZX-9');
  assert(nav.layout === 'TwoColumnsMidExpanded' && nav.routeParams.id === 'ZX-9',
    'buildNavigationState should produce detail navigation state');
}

function testSearchSmartFilterFlowUseCase() {
  const mod = loadSapModule('service/usecase/SearchSmartFilterFlowUseCase.js');
  const Filter = createFilterStub();
  const FilterOperator = { Contains: 'Contains', EQ: 'EQ' };
  const coordinator = loadSapModule('util/SearchSmartControlCoordinator.js', {
    'sap/ui/model/Filter': Filter,
    'sap/ui/model/FilterOperator': FilterOperator
  });

  assert(mod.extractSmartFilterValue({ ranges: [{ value1: 'AA-01' }] }) === 'AA-01',
    'extractSmartFilterValue should read range value1 payloads');

  const patch = mod.resolveFilterPatch({ checklist_id: [{ key: 'CHK-1' }], LPC_KEY: { value: 'L3' } });
  assert(patch.filterId === 'CHK-1' && patch.filterLpc === 'L3',
    'resolveFilterPatch should normalize SmartFilter data into state patch');

  const stateBag = {};
  const stateModel = { setProperty: (k, v) => { stateBag[k] = v; } };
  mod.syncStateFiltersFromSmartFilter({
    stateModel,
    smartFilterBar: { getFilterData: () => ({ id: { value: 'R-77' }, lpc: { key: 'L2' } }) }
  });
  assert(stateBag['/filterId'] === 'R-77' && stateBag['/filterLpc'] === 'L2',
    'syncStateFiltersFromSmartFilter should write normalized id/lpc values to state model');

  const bindingParams = { filters: [], parameters: { top: 9 }, events: {} };
  mod.prepareRebindParams({
    bindingParams,
    state: {
      filterId: 'ID-5',
      filterLpc: 'L1',
      filterFailedChecks: 'ALL',
      filterFailedBarriers: 'ALL',
      searchMaxResults: ''
    },
    onDataReceived: () => {},
    applyRebindParams: coordinator.applyRebindParams
  });

  assert(bindingParams.filters.length >= 2,
    'prepareRebindParams should delegate filter composition through applyRebindParams policy');
  assert(!Object.prototype.hasOwnProperty.call(bindingParams.parameters, 'top'),
    'prepareRebindParams should preserve max-results policy and clear top when searchMaxResults is empty');
}


function testOperationalKpiInstrumentationUseCase() {
  const mod = loadSapModule('service/usecase/OperationalKpiInstrumentationUseCase.js');
  const state = {
    m: {},
    getProperty(k) { return this.m[k]; },
    setProperty(k, v) { this.m[k] = v; }
  };

  const bag = mod.ensureKpiBag(state);
  assert(!!bag && bag.saveAttempts === 0, 'ensureKpiBag should initialize operational KPI bag');

  mod.markSaveAttempt(state);
  mod.markSaveSuccess(state);
  mod.markSaveFailed(state);
  mod.markConflict(state);
  mod.markValidationFailure(state);
  mod.markRetryFailure(state);

  assert(state.m['/operationalKpi/saveAttempts'] === 1
    && state.m['/operationalKpi/saveSuccess'] === 1
    && state.m['/operationalKpi/saveFailed'] === 1
    && state.m['/operationalKpi/conflictCount'] === 1
    && state.m['/operationalKpi/validationFailures'] === 1
    && state.m['/operationalKpi/retryFailures'] === 1,
    'KPI counters should increment deterministically');

  const startedAt = mod.beginLatencySample();
  const done = mod.finishLatencySample(state, 'save', startedAt - 8);
  assert(done.ok === true && state.m['/operationalKpi/saveLatencySamples'] === 1
    && Number(state.m['/operationalKpi/saveLatencyMsLast']) >= 0,
    'finishLatencySample should persist latency sample stats for save prefix');
}

function testSearchWorkflowAnalyticsDialogUseCase() {
  const mod = loadSapModule('service/usecase/SearchWorkflowAnalyticsDialogUseCase.js');

  const vm = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  let opened = 0;
  let closed = 0;

  mod.openDialogLifecycle({
    dialog: {},
    runLoad: () => {},
    openDialog: () => { opened += 1; }
  });
  mod.closeDialogLifecycle({
    dialog: {},
    closeDialog: () => { closed += 1; }
  });
  assert(opened === 1 && closed === 1, 'dialog lifecycle helpers should delegate open/close callbacks');

  return mod.runAnalyticsLoadFlow({
    viewModel: vm,
    loadAnalytics: () => Promise.resolve({ total: 5 })
  }).then(() => {
    assert(vm.m['/analyticsBusy'] === false, 'runAnalyticsLoadFlow should always clear busy state after success');
    assert(vm.m['/analytics'].source === 'fallback', 'runAnalyticsLoadFlow should normalize missing source to fallback');

    return mod.runAnalyticsLoadFlow({
      viewModel: vm,
      loadAnalytics: () => Promise.reject(new Error('analytics-failed'))
    });
  }).then((res) => {
    assert(res === null, 'runAnalyticsLoadFlow should resolve null on analytics errors');
    assert(vm.m['/analyticsError'] === 'analytics-failed', 'runAnalyticsLoadFlow should write analytics error on failure');
    assert(vm.m['/analyticsBusy'] === false, 'runAnalyticsLoadFlow should clear busy state after error');
  });
}

function testSearchExportOrchestrationUseCase() {
  const mod = loadSapModule('service/usecase/SearchExportOrchestrationUseCase.js');

  const menuEntity = mod.resolveExportEntityFromAction({
    event: { getParameter: () => ({ data: (key) => key === 'entity' ? 'check' : null }) },
    defaultEntity: 'screen',
    resolveEntityFromMenuEvent: (event, fallback) => {
      const item = event.getParameter('item');
      return item ? item.data('entity') : fallback;
    }
  });
  assert(menuEntity === 'check', 'resolveExportEntityFromAction should resolve entity from menu event payload');

  const reportEntity = mod.resolveExportEntityFromAction({
    source: { data: (key) => key === 'entity' ? 'barrier' : null },
    defaultEntity: 'screen'
  });
  assert(reportEntity === 'barrier', 'resolveExportEntityFromAction should resolve entity from source custom data');

  assert(mod.buildExportFilename('screen', () => 42) === 'checklist_screen_42',
    'buildExportFilename should build deterministic export name pattern');

  let successRows = 0;
  let emptyCalls = 0;
  let errorCalls = 0;

  return mod.runExportLifecycle({
    runExportFlow: (args) => args.runWithLoading(() => args.buildExportPromise().then((res) => {
      const rows = (res && res.rows) || [];
      if (!rows.length) {
        args.onEmpty();
        return null;
      }
      args.onSuccess(rows);
      return rows;
    }).catch((e) => {
      args.onError(e);
      return null;
    })),
    runWithLoading: (fn) => fn(),
    buildExportPromise: () => Promise.resolve({ rows: [{ id: '1' }] }),
    onEmpty: () => { emptyCalls += 1; },
    onSuccess: (rows) => { successRows += rows.length; },
    onError: () => { errorCalls += 1; }
  }).then(() => {
    return mod.runExportLifecycle({
      runExportFlow: (args) => args.runWithLoading(() => args.buildExportPromise().then((res) => {
        const rows = (res && res.rows) || [];
        if (!rows.length) {
          args.onEmpty();
          return null;
        }
        args.onSuccess(rows);
        return rows;
      })),
      runWithLoading: (fn) => fn(),
      buildExportPromise: () => Promise.resolve({ rows: [] }),
      onEmpty: () => { emptyCalls += 1; },
      onSuccess: () => {},
      onError: () => { errorCalls += 1; }
    });
  }).then(() => {
    return mod.runExportLifecycle({
      runExportFlow: (args) => args.runWithLoading(() => args.buildExportPromise().catch((e) => {
        args.onError(e);
        return null;
      })),
      runWithLoading: (fn) => fn(),
      buildExportPromise: () => Promise.reject(new Error('fail')), 
      onEmpty: () => {},
      onSuccess: () => {},
      onError: () => { errorCalls += 1; }
    });
  }).then(() => {
    assert(successRows === 1 && emptyCalls === 1 && errorCalls === 1,
      'runExportLifecycle should preserve success/empty/error callback routing');
  });
}

function testWorkflowAnalyticsUseCaseFallback() {
  const BackendAdapter = { getProcessAnalytics: () => Promise.reject(new Error('backend down')) };
  const SmartSearchAdapter = { filterData: (rows) => rows };
  const mod = loadSapModule('service/usecase/WorkflowAnalyticsUseCase.js', {
    'sap_ui5/service/backend/BackendAdapter': BackendAdapter,
    'sap_ui5/service/SmartSearchAdapter': SmartSearchAdapter
  });

  const fallbackRows = [{ root: { status: 'CLOSED', has_failed_checks: true } }];
  return mod.loadProcessAnalytics({}, 'EXACT', fallbackRows).then((a) => {
    assert(a.source === 'fallback', 'WorkflowAnalyticsUseCase should fallback when backend analytics fails');
    assert(a.total === 1, 'WorkflowAnalyticsUseCase fallback should compute totals');
  });
}


function testWorkflowAnalyticsUseCaseFallbackNegative() {
  const BackendAdapter = { getProcessAnalytics: () => Promise.reject(new Error('backend down')) };
  const SmartSearchAdapter = { filterData: () => [{ root: { status: 'REGISTERED', has_failed_barriers: true } }] };
  const mod = loadSapModule('service/usecase/WorkflowAnalyticsUseCase.js', {
    'sap_ui5/service/backend/BackendAdapter': BackendAdapter,
    'sap_ui5/service/SmartSearchAdapter': SmartSearchAdapter
  });

  return mod.loadProcessAnalytics({}, 'LOOSE', [{ root: {} }]).then((a) => {
    assert(a.source === 'fallback', 'WorkflowAnalyticsUseCase should keep fallback source for backend errors');
    assert(a.failedBarriers === 1, 'WorkflowAnalyticsUseCase fallback should aggregate failed barriers');
  });
}

function testDetailEditOrchestrationRecoverBranch() {
  const mod = loadSapModule('service/usecase/DetailEditOrchestrationUseCase.js');
  let recoveredCalled = 0;
  return mod.runToggleEditFlow({
    editMode: true,
    isDirty: false,
    shouldPromptBeforeDisableEdit: () => false,
    isCancelDecision: () => false,
    confirmUnsaved: () => Promise.resolve('SAVE'),
    runPendingRelease: () => Promise.resolve(),
    runPendingToggle: (fn) => fn(),
    releaseEdit: () => Promise.resolve(),
    ensureFreshBeforeEdit: () => Promise.resolve(),
    confirmIntegrationEdit: () => Promise.resolve(true),
    onStayReadOnly: () => {},
    acquireLock: () => Promise.reject(new Error('lock conflict')),
    onLockAcquired: () => {},
    tryRecoverFromAcquireError: () => { recoveredCalled += 1; return Promise.resolve(true); },
    onAcquireFailed: () => Promise.resolve()
  }).then(() => {
    assert(recoveredCalled === 1, 'runToggleEditFlow should attempt recovery on lock acquire error');
  });
}


function testDetailSaveOrchestrationUseCase() {
  const mod = loadSapModule('service/usecase/DetailSaveOrchestrationUseCase.js');
  let applied = 0;
  return mod.runSaveFlow({
    saveChecklist: () => Promise.resolve({ root: { id: '1' } }),
    loadChecklistCollection: () => Promise.resolve([{ root: { id: '1' } }]),
    applySaveResult: () => { applied += 1; },
    handleSaveError: () => Promise.resolve(null)
  }).then(() => {
    assert(applied === 1, 'runSaveFlow should apply save result on success');
  });
}


function testSearchApplicationServiceTimeoutFallback() {
  const BackendAdapter = {
    queryCheckLists: () => Promise.reject(new Error('timeout'))
  };
  const SmartSearchAdapter = {
    filterData: (rows) => rows
  };
  const mod = loadSapModule('service/usecase/SearchApplicationService.js', {
    'sap_ui5/service/backend/BackendAdapter': BackendAdapter,
    'sap_ui5/service/SmartSearchAdapter': SmartSearchAdapter
  });

  const fallback = [{ root: { id: 'F-1' } }];
  return mod.runSearch({ filterId: 'F' }, 'EXACT', fallback).then((rows) => {
    assert(Array.isArray(rows) && rows.length === 1, 'runSearch should fallback to local collection on backend timeout');
  });
}

function testDetailSaveOrchestrationUseCaseErrorBranch() {
  const mod = loadSapModule('service/usecase/DetailSaveOrchestrationUseCase.js');
  let handled = 0;

  return mod.runSaveFlow({
    saveChecklist: () => Promise.reject(new Error('timeout')),
    loadChecklistCollection: () => Promise.resolve([]),
    applySaveResult: () => {},
    handleSaveError: () => { handled += 1; return Promise.resolve('handled'); }
  }).then((res) => {
    assert(handled === 1 && res === 'handled', 'runSaveFlow should delegate timeout errors to handleSaveError');
  });
}


function testSearchApplicationServiceConflictFallback() {
  const BackendAdapter = {
    queryCheckLists: () => Promise.reject(new Error('conflict'))
  };
  const SmartSearchAdapter = {
    filterData: (rows) => rows.filter((r) => !!r)
  };
  const mod = loadSapModule('service/usecase/SearchApplicationService.js', {
    'sap_ui5/service/backend/BackendAdapter': BackendAdapter,
    'sap_ui5/service/SmartSearchAdapter': SmartSearchAdapter
  });

  return mod.runSearch({ filterId: '' }, 'LOOSE', [{ root: { id: 'C-1' } }]).then((rows) => {
    assert(rows.length === 1, 'runSearch should fallback for backend conflict errors as well as timeouts');
  });
}

function testDetailSaveOrchestrationUseCaseConflictBranch() {
  const mod = loadSapModule('service/usecase/DetailSaveOrchestrationUseCase.js');
  let handled = 0;
  return mod.runSaveFlow({
    saveChecklist: () => Promise.reject(new Error('conflict')),
    loadChecklistCollection: () => Promise.resolve([]),
    applySaveResult: () => {},
    handleSaveError: () => { handled += 1; return Promise.resolve('conflict-handled'); }
  }).then((res) => {
    assert(handled === 1 && res === 'conflict-handled', 'runSaveFlow should delegate conflict errors to handleSaveError');
  });
}


function testSearchApplicationServiceNetworkFallback() {
  const BackendAdapter = {
    queryCheckLists: () => Promise.reject(new Error('network unreachable'))
  };
  const SmartSearchAdapter = {
    filterData: (rows) => rows
  };
  const mod = loadSapModule('service/usecase/SearchApplicationService.js', {
    'sap_ui5/service/backend/BackendAdapter': BackendAdapter,
    'sap_ui5/service/SmartSearchAdapter': SmartSearchAdapter
  });

  return mod.runSearch({ filterId: '' }, 'EXACT', [{ root: { id: 'N-1' } }]).then((rows) => {
    assert(rows.length === 1 && rows[0].root.id === 'N-1', 'runSearch should fallback to local collection on network errors');
  });
}

function testDetailSaveOrchestrationUseCaseNetworkBranch() {
  const mod = loadSapModule('service/usecase/DetailSaveOrchestrationUseCase.js');
  let handled = 0;
  return mod.runSaveFlow({
    saveChecklist: () => Promise.reject(new Error('network unreachable')),
    loadChecklistCollection: () => Promise.resolve([]),
    applySaveResult: () => {},
    handleSaveError: () => { handled += 1; return Promise.resolve('network-handled'); }
  }).then((res) => {
    assert(handled === 1 && res === 'network-handled', 'runSaveFlow should delegate network errors to handleSaveError');
  });
}

function testSearchLoadFilterUseCase() {
  const mod = loadSapModule('service/usecase/SearchLoadFilterUseCase.js');
  const state = { m: {}, setProperty(k, v) { this.m[k] = v; } };
  const view = { m: {}, setProperty(k, v) { this.m[k] = v; } };

  mod.resetFilters(state, view);
  assert(state.m['/filterId'] === '' && view.m['/hasSearched'] === true, 'resetFilters should clear filter state and mark searched');

  mod.applySearchMode(state, view, true);
  assert(state.m['/searchMode'] === 'LOOSE', 'applySearchMode should set LOOSE mode');

  let loaded = 0;
  return mod.runRetryLoad({
    resetLoadError: () => {},
    runWithLoading: (fn) => fn(),
    getCheckLists: () => Promise.resolve([1]),
    applyLoadedRows: () => { loaded += 1; },
    applyLoadError: () => {}
  }).then(() => {
    assert(loaded === 1, 'runRetryLoad should apply loaded rows on success');
  });
}

function testSearchLoadFilterUseCaseNegative() {
  const mod = loadSapModule('service/usecase/SearchLoadFilterUseCase.js');
  let errors = 0;

  return mod.runRetryLoad({
    resetLoadError: () => {},
    runWithLoading: (fn) => fn(),
    getCheckLists: () => Promise.reject(new Error('load failed')),
    applyLoadedRows: () => {},
    applyLoadError: () => { errors += 1; }
  }).then(() => {
    assert(errors === 1, 'runRetryLoad should call applyLoadError on load failure');
  });
}

function testSearchSmartCoordinatorMetadataRecoveryIntegration() {
  const Filter = createFilterStub();
  const FilterOperator = { Contains: 'Contains', EQ: 'EQ' };
  const mod = loadSapModule('util/SearchSmartControlCoordinator.js', {
    'sap/ui/model/Filter': Filter,
    'sap/ui/model/FilterOperator': FilterOperator
  });

  const vm = { m: {}, getProperty(k){ return this.m[k]; }, setProperty(k,v){ this.m[k]=v; } };
  const state = { m: { '/mainServiceMetadataOk': false, '/mainServiceMetadataError': 'meta' }, getProperty(k){ return this.m[k]; } };
  let bootstrapCalls = 0;

  mod.syncAvailability({ stateModel: state, viewModel: vm, unavailableText: 'fallback', bootstrap: () => { bootstrapCalls += 1; } });
  assert(vm.m['/useSmartControls'] === false, 'syncAvailability should disable smart controls on metadata fail');

  state.m['/mainServiceMetadataOk'] = true;
  mod.syncAvailability({ stateModel: state, viewModel: vm, unavailableText: 'fallback', bootstrap: () => { bootstrapCalls += 1; } });
  assert(vm.m['/useSmartControls'] === true && bootstrapCalls === 1, 'syncAvailability should re-enable and bootstrap on recovery');
}




function testSearchInlineAnalyticsRefreshOrchestrationUseCase() {
  const mod = loadSapModule('service/usecase/SearchInlineAnalyticsRefreshOrchestrationUseCase.js');
  const state = mod.ensureRefreshState({});
  const viewModel = { setProperty: () => {}, getProperty: () => ({ total: 0 }) };

  let appliedCount = 0;
  return Promise.all([
    mod.runRefreshLifecycle({
      refreshState: state,
      viewModel,
      loadAnalytics: () => new Promise((resolve) => setTimeout(() => resolve({ total: 1 }), 25)),
      applyPresentation: () => { appliedCount += 1; }
    }),
    mod.runRefreshLifecycle({
      refreshState: state,
      viewModel,
      loadAnalytics: () => new Promise((resolve) => setTimeout(() => resolve({ total: 2 }), 5)),
      applyPresentation: () => { appliedCount += 1; }
    })
  ]).then((pair) => {
    assert(pair.some((r) => r.stale === true), 'runRefreshLifecycle should mark stale earlier request');
    assert(appliedCount === 1, 'runRefreshLifecycle should apply only latest request presentation');

    return mod.runRefreshLifecycle({
      refreshState: state,
      viewModel,
      loadAnalytics: () => Promise.reject(new Error('boom')),
      applyPresentation: () => { appliedCount += 100; }
    });
  }).then((failed) => {
    assert(failed.applied === false && failed.error === 'boom', 'runRefreshLifecycle should map rejected load promise');

    return mod.runRefreshLifecycle({
      refreshState: state,
      viewModel: null,
      loadAnalytics: () => Promise.resolve({ total: 1 }),
      applyPresentation: () => {}
    });
  }).then((missingAdapter) => {
    assert(missingAdapter.reason === 'missing_adapter', 'runRefreshLifecycle should handle missing view model');

    assert(mod.shouldRefreshForTrigger('ROUTE_MATCHED') === true,
      'shouldRefreshForTrigger should allow ROUTE_MATCHED trigger');
    assert(mod.shouldRefreshForTrigger('UNKNOWN') === false,
      'shouldRefreshForTrigger should reject unknown trigger');
  });
}

function testSearchInlineAnalyticsPresentationUseCase() {
  const mod = loadSapModule('service/usecase/SearchInlineAnalyticsPresentationUseCase.js');
  const viewState = {};
  const viewModel = {
    setProperty: (k, v) => { viewState[k] = v; },
    getProperty: (k) => viewState[k]
  };
  const bundle = {
    getText: (k) => ({
      analyticsSourceBackend: 'Backend aggregate',
      analyticsSourceFallback: 'Client fallback'
    }[k] || k)
  };

  const mapped = mod.applyInlineAnalyticsPresentation({
    viewModel,
    bundle,
    analytics: {
      total: '8',
      failedChecks: 2,
      failedBarriers: 1,
      healthy: 5,
      avgChecksRate: '88',
      avgBarriersRate: 95,
      refreshedAt: '',
      source: 'backend'
    }
  });

  assert(mapped.total === 8 && mapped.avgChecksRate === 88, 'applyInlineAnalyticsPresentation should normalize numbers');
  assert(mapped.refreshedAt === '-', 'applyInlineAnalyticsPresentation should normalize refreshedAt fallback');
  assert(mapped.sourceText === 'Backend aggregate', 'applyInlineAnalyticsPresentation should map backend source text');

  const nullModel = mod.applyInlineAnalyticsPresentation({ viewModel: null, analytics: {} });
  assert(nullModel === null, 'applyInlineAnalyticsPresentation should return null when viewModel missing');
}

function testWorkflowAnalyticsUseCaseBackendEntityPayloadVariants() {
  const BackendAdapter = {
    getProcessAnalytics: () => Promise.resolve({
      value: [{
        TOTAL: 9,
        FAILED_CHECKS: 2,
        FAILED_BARRIERS: 1,
        HEALTHY: 7,
        CLOSED_COUNT: 3,
        REGISTERED_COUNT: 4,
        AVG_CHECKS_RATE: 91,
        AVG_BARRIERS_RATE: 89,
        REFRESHED_AT: '2026-02-26T10:00:00Z'
      }]
    })
  };
  const SmartSearchAdapter = { filterData: () => [] };
  const mod = loadSapModule('service/usecase/WorkflowAnalyticsUseCase.js', {
    'sap_ui5/service/backend/BackendAdapter': BackendAdapter,
    'sap_ui5/service/SmartSearchAdapter': SmartSearchAdapter
  });

  return mod.loadProcessAnalytics({}, 'EXACT', []).then((a) => {
    assert(a.source === 'backend' && a.total === 9 && a.failedChecks === 2,
      'WorkflowAnalyticsUseCase should map entity payload/uppercase fields');
    assert(a.refreshedAt === '2026-02-26T10:00:00Z',
      'WorkflowAnalyticsUseCase should map refreshedAt from REFRESHED_AT');
  });
}

function testWorkflowAnalyticsUseCaseBackendMalformedPayloadFallback() {
  const BackendAdapter = {
    getProcessAnalytics: () => Promise.resolve({ total: 'oops', failedChecks: null, source: 'backend' })
  };
  const SmartSearchAdapter = { filterData: () => [] };
  const mod = loadSapModule('service/usecase/WorkflowAnalyticsUseCase.js', {
    'sap_ui5/service/backend/BackendAdapter': BackendAdapter,
    'sap_ui5/service/SmartSearchAdapter': SmartSearchAdapter
  });

  return mod.loadProcessAnalytics({}, 'EXACT', []).then((a) => {
    assert(a.source === 'backend', 'WorkflowAnalyticsUseCase should keep backend source when payload exists');
    assert(a.total === 0 && a.failedChecks === 0 && a.failedBarriers === 0,
      'WorkflowAnalyticsUseCase should safe-normalize malformed backend counters');
    assert(typeof a.refreshedAt === 'string' && a.refreshedAt.length > 0,
      'WorkflowAnalyticsUseCase should provide fallback refreshedAt for malformed payload');
  });
}

function testSearchFilterHintPresentationUseCase() {
  const mod = loadSapModule('service/usecase/SearchFilterHintPresentationUseCase.js');
  const viewData = {};
  const viewModel = { setProperty: (k, v) => { viewData[k] = v; } };
  const stateModel = { getProperty: () => null };
  const bundle = {
    getText: (k) => {
      const m = {
        filterHintSmartActive: 'smart-active',
        filterHintFallbackActive: 'fallback-active',
        filterHintCleared: 'cleared'
      };
      if (!m[k]) {
        throw new Error('missing-key');
      }
      return m[k];
    }
  };

  const smart = mod.applyHintPresentation({
    viewModel,
    stateModel,
    useSmartControls: true,
    hasSmartFilters: true,
    fallbackPayload: { filterId: '', filterLpc: '', filterFailedChecks: 'ALL', filterFailedBarriers: 'ALL' },
    bundle
  });
  assert(smart.visible === true && smart.text === 'smart-active' && smart.hasActiveFilters === true,
    'applyHintPresentation should map smart-active branch');

  const fallback = mod.applyHintPresentation({
    viewModel,
    stateModel,
    useSmartControls: false,
    hasSmartFilters: false,
    fallbackPayload: { filterId: 'X-1', filterLpc: '', filterFailedChecks: 'ALL', filterFailedBarriers: 'ALL' },
    bundle
  });
  assert(fallback.visible === true && fallback.type === 'Warning' && fallback.text === 'fallback-active',
    'applyHintPresentation should map fallback-active branch');

  const missingModel = mod.applyHintPresentation({
    viewModel: null,
    stateModel,
    useSmartControls: true,
    hasSmartFilters: false,
    fallbackPayload: {},
    bundle
  });
  assert(missingModel === null, 'applyHintPresentation should return null when viewModel missing');

  const noBundleText = mod.resolveHintText({ bundle: null, textKey: 'filterHintSmartActive' });
  assert(noBundleText === '', 'resolveHintText should return empty string without bundle');

  const missingBundleKey = mod.resolveHintText({
    bundle: { getText: () => { throw new Error('no-key'); } },
    textKey: 'unknown_key'
  });
  assert(missingBundleKey === '', 'resolveHintText should return empty string for missing bundle key');

  const malformedState = mod.resolveHintState({ stateModel: null, useSmartControls: true, hasSmartFilters: true, fallbackPayload: {} });
  assert(malformedState.hasActiveFilters === false && malformedState.visible === false,
    'resolveHintState should safely handle missing state model');
}


function testWaveB3CriticalJourneysMatrix() {
  const SearchOpenDetailGuardUseCase = loadSapModule('service/usecase/SearchOpenDetailGuardUseCase.js');
  const DetailEditOrchestrationUseCase = loadSapModule('service/usecase/DetailEditOrchestrationUseCase.js');
  const DetailCommandFlowUseCase = loadSapModule('service/usecase/DetailCommandFlowUseCase.js');
  const DetailLockEditFlowUseCase = loadSapModule('service/usecase/DetailLockEditFlowUseCase.js', {
    'sap_ui5/service/usecase/DetailEditOrchestrationUseCase': DetailEditOrchestrationUseCase,
    'sap_ui5/service/usecase/DetailCommandFlowUseCase': DetailCommandFlowUseCase
  });
  const DetailSaveOrchestrationUseCase = loadSapModule('service/usecase/DetailSaveOrchestrationUseCase.js');
  const DetailLifecycleUseCase = loadSapModule('service/usecase/DetailLifecycleUseCase.js');
  const DetailLockReleaseUseCase = loadSapModule('service/usecase/DetailLockReleaseUseCase.js', {
    'sap_ui5/service/usecase/DetailLifecycleUseCase': DetailLifecycleUseCase
  });
  const SearchCreateCopyNavigationGuardUseCase = loadSapModule('service/usecase/SearchCreateCopyNavigationGuardUseCase.js');
  const SearchRetryLoadPresentationUseCase = loadSapModule('service/usecase/SearchRetryLoadPresentationUseCase.js');
  const DetailSaveConflictUseCase = loadSapModule('service/usecase/DetailSaveConflictUseCase.js');

  const stateModel = {
    m: {},
    setProperty: function (k, v) { this.m[k] = v; },
    getProperty: function (k) { return this.m[k]; }
  };

  return SearchOpenDetailGuardUseCase.runOpenDetailFlow({
    id: 'CHK-A',
    confirmNavigation: () => true,
    buildIntent: (id) => ({ route: 'detail', id }),
    applyIntent: () => true
  }).then((openRes) => {
    assert(openRes.ok === true && openRes.reason === 'applied',
      'A(read/open): open detail flow should produce applied outcome');

    let acquired = 0;
    return DetailLockEditFlowUseCase.runToggleEditFlow({
      editMode: true,
      isDirty: false,
      runPendingToggle: (fn) => Promise.resolve(fn()),
      runPendingRelease: () => Promise.resolve({ ok: true }),
      releaseEdit: () => Promise.resolve({ ok: true }),
      confirmUnsaved: () => Promise.resolve('DISCARD'),
      ensureFreshBeforeEdit: () => Promise.resolve({ ok: true }),
      confirmIntegrationEdit: () => Promise.resolve(true),
      acquireLock: () => Promise.resolve({ ok: true }),
      onLockAcquired: () => { acquired += 1; },
      tryRecoverFromAcquireError: () => Promise.resolve({ ok: false }),
      onAcquireFailed: () => {}
    }).then((editRes) => {
      assert(editRes.ok === true && acquired === 1,
        'B(edit/lock): toggle-edit flow should acquire lock and enter edit');

      return DetailSaveOrchestrationUseCase.runSaveFlow({
        saveChecklist: () => Promise.resolve({ root: { id: 'CHK-C' } }),
        loadChecklistCollection: () => Promise.resolve([{ root: { id: 'CHK-C' } }]),
        applySaveResult: () => {},
        handleSaveError: () => Promise.resolve({ ok: false })
      }).then((saveRes) => {
        assert(saveRes.savedChecklist.root.id === 'CHK-C',
          'C(autosave/fullsave): save orchestration should resolve saved checklist payload');

        let releaseCalled = 0;
        let idleCalled = 0;
        return DetailLockReleaseUseCase.runReleaseFlow({
          stateModel,
          releaseLock: () => { releaseCalled += 1; return Promise.resolve(); },
          setLockUiIdle: () => { idleCalled += 1; }
        }).then(() => {
          assert(releaseCalled === 1 && idleCalled === 1 && stateModel.getProperty('/mode') === 'READ',
            'D(unload/release): release flow should set READ mode and clear lock UI pending state');

          let recoverAttempts = 0;
          let recoveredLock = 0;
          return DetailLockEditFlowUseCase.runToggleEditFlow({
            editMode: true,
            isDirty: false,
            runPendingToggle: (fn) => Promise.resolve(fn()),
            runPendingRelease: () => Promise.resolve({ ok: true }),
            releaseEdit: () => Promise.resolve({ ok: true }),
            confirmUnsaved: () => Promise.resolve('DISCARD'),
            ensureFreshBeforeEdit: () => Promise.resolve({ ok: true }),
            confirmIntegrationEdit: () => Promise.resolve(true),
            acquireLock: () => Promise.reject(new Error('acquire-failed')),
            onLockAcquired: () => { recoveredLock += 1; },
            tryRecoverFromAcquireError: () => {
              recoverAttempts += 1;
              return Promise.resolve({ ok: true });
            },
            onAcquireFailed: () => {}
          }).then((recoverRes) => {
            assert(recoverRes === null && recoverAttempts === 1 && recoveredLock === 0,
              'E(steal/session-killed recovery): toggle-edit should complete deterministic recovery branch without acquire-failure escalation');

            return SearchCreateCopyNavigationGuardUseCase.runCreateNavigationFlow({
              confirmNavigation: () => true,
              buildCreateIntent: () => ({ route: 'detail', mode: 'create' }),
              applyIntent: () => true
            }).then((createRes) => {
              assert(createRes.ok === true && createRes.reason === 'applied',
                'F(copy/create): create navigation should apply intent');

              return SearchCreateCopyNavigationGuardUseCase.runCopyNavigationFlow({
                resolveSelectedId: () => 'CHK-F',
                confirmNavigation: () => true,
                buildCopyIntent: (id) => ({ route: 'detail', mode: 'copy', id }),
                applyIntent: () => true
              }).then((copyRes) => {
                assert(copyRes.ok === true && copyRes.reason === 'applied',
                  'F(copy/create): copy navigation should apply intent with selected id');

                return SearchRetryLoadPresentationUseCase.runRetryFlow({
                  stateModel,
                  dataModel: { setProperty: () => {} },
                  getCheckLists: () => Promise.resolve([{ root: { id: 'CHK-G' } }]),
                  runWithLoading: (fn) => fn(),
                  maxAttempts: 2
                }).then((retryRes) => {
                  assert(retryRes.ok === true && retryRes.rows.length === 1,
                    'G(retry): retry flow should load rows successfully');

                  return DetailSaveConflictUseCase.handleConflictChoice('Reload', {
                    reloadLabel: 'Reload',
                    overwriteLabel: 'Overwrite',
                    onReload: () => Promise.resolve('done')
                  }).then((conflictRes) => {
                    assert(conflictRes.ok === true && conflictRes.reason === 'reloaded',
                      'H(save conflict): conflict flow should return deterministic reloaded outcome');
                  });
                });
              });
            });
          });
        });
      });
    });
  });
}

async function main() {
  await runTest('DetailLifecycleUseCase', testDetailLifecycleUseCase);
  await runTest('DetailFormattersLockOperationPresentation', testDetailFormattersLockOperationPresentation);
  await runTest('SearchSmartControlCoordinator', testSearchSmartControlCoordinator);
  await runTest('SearchWorkflowOrchestrator', testSearchWorkflowOrchestrator);
  await runTest('ChecklistValidationService', testChecklistValidationService);
  await runTest('DeltaPayloadBuilder', testDeltaPayloadBuilder);
  await runTest('SearchPresentationUseCase', testSearchPresentationUseCase);
  await runTest('DetailToolbarValidationUseCase', testDetailToolbarValidationUseCase);
  await runTest('DetailSaveErrorPresentationUseCase', testDetailSaveErrorPresentationUseCase);
  await runTest('DetailSaveErrorOutcomePresentationUseCase', testDetailSaveErrorOutcomePresentationUseCase);
  await runTest('DetailDialogLifecycleUseCase', testDetailDialogLifecycleUseCase);
  await runTest('SmartSearchAdapterFilterModes', testSmartSearchAdapterFilterModes);
  await runTest('SearchActionUseCase', testSearchActionUseCase);
  await runTest('DetailCommandFlowUseCase', testDetailCommandFlowUseCase);
  await runTest('SearchUiFlowUseCase', testSearchUiFlowUseCase);
  await runTest('DetailSaveConflictUseCase', testDetailSaveConflictUseCase);
  await runTest('WaveB3CriticalJourneysMatrix', testWaveB3CriticalJourneysMatrix);
  await runTest('DetailEditOrchestrationUseCase', testDetailEditOrchestrationUseCase);
  await runTest('DetailEditOrchestrationFreshnessFailure', testDetailEditOrchestrationFreshnessFailure);
  await runTest('SearchAnalyticsExportUseCase', testSearchAnalyticsExportUseCase);
  await runTest('DetailCloseFlowUseCase', testDetailCloseFlowUseCase);
  await runTest('DetailLockReleaseUseCaseNegative', testDetailLockReleaseUseCaseNegative);
  await runTest('DetailSaveConflictFlowUseCase', testDetailSaveConflictFlowUseCase);
  await runTest('DetailSaveConflictFlowUseCasePropagation', testDetailSaveConflictFlowUseCasePropagation);
  await runTest('DetailSaveSuccessFlowUseCase', testDetailSaveSuccessFlowUseCase);
  await runTest('DetailLockEditFlowUseCase', testDetailLockEditFlowUseCase);
  await runTest('DetailStatusRowUseCase', testDetailStatusRowUseCase);
  await runTest('DetailStatusCommandUseCase', testDetailStatusCommandUseCase);
  await runTest('SearchAnalyticsDialogExportFlowUseCase', testSearchAnalyticsDialogExportFlowUseCase);
  await runTest('SearchAnalyticsDialogExportFlowUseCaseNegative', testSearchAnalyticsDialogExportFlowUseCaseNegative);
  await runTest('DetailRowDialogCommandUseCase', testDetailRowDialogCommandUseCase);
  await runTest('DetailExpandedRowsFlowUseCase', testDetailExpandedRowsFlowUseCase);
  await runTest('DetailLocationValueHelpUseCase', testDetailLocationValueHelpUseCase);
  await runTest('DetailPersonSuggestionUseCase', testDetailPersonSuggestionUseCase);
  await runTest('DetailDictionarySelectionUseCase', testDetailDictionarySelectionUseCase);
  await runTest('DetailLpcBarrierWarningFlowUseCase', testDetailLpcBarrierWarningFlowUseCase);
  await runTest('DetailIntegrationEditWarningUseCase', testDetailIntegrationEditWarningUseCase);
  await runTest('DetailUnsavedDecisionFlowUseCase', testDetailUnsavedDecisionFlowUseCase);
  await runTest('DetailCloseNavigationFlowUseCase', testDetailCloseNavigationFlowUseCase);
  await runTest('SearchSmartCoordinatorMetadataDegradedMode', testSearchSmartCoordinatorMetadataDegradedMode);
  await runTest('DetailStatusCommandUseCaseIntegrationRestriction', testDetailStatusCommandUseCaseIntegrationRestriction);
  await runTest('SearchIntentUseCase', testSearchIntentUseCase);
  await runTest('SearchSelectionNavigationUseCase', testSearchSelectionNavigationUseCase);
  await runTest('SearchRetryLoadPresentationUseCase', testSearchRetryLoadPresentationUseCase);
  await runTest('SearchToolbarActionStateUseCase', testSearchToolbarActionStateUseCase);
  await runTest('SearchNavigationIntentUseCase', testSearchNavigationIntentUseCase);
  await runTest('SearchDeleteOrchestrationUseCase', testSearchDeleteOrchestrationUseCase);
  await runTest('SearchActionMessagePresentationUseCase', testSearchActionMessagePresentationUseCase);
  await runTest('SearchRetryMessagePresentationUseCase', testSearchRetryMessagePresentationUseCase);
  await runTest('SearchSummaryPresentationUseCase', testSearchSummaryPresentationUseCase);
  await runTest('SearchEmptyStatePresentationUseCase', testSearchEmptyStatePresentationUseCase);
  await runTest('SearchSelectionHydrationUseCase', testSearchSelectionHydrationUseCase);
  await runTest('SearchOpenDetailGuardUseCase', testSearchOpenDetailGuardUseCase);
  await runTest('SearchCreateCopyNavigationGuardUseCase', testSearchCreateCopyNavigationGuardUseCase);
  await runTest('SearchExportIntentGuardUseCase', testSearchExportIntentGuardUseCase);
  await runTest('SearchSmartFilterFlowUseCase', testSearchSmartFilterFlowUseCase);
  await runTest('SearchWorkflowAnalyticsDialogUseCase', testSearchWorkflowAnalyticsDialogUseCase);
  await runTest('OperationalKpiInstrumentationUseCase', testOperationalKpiInstrumentationUseCase);
  await runTest('SearchExportOrchestrationUseCase', testSearchExportOrchestrationUseCase);
  await runTest('WorkflowAnalyticsUseCaseFallback', testWorkflowAnalyticsUseCaseFallback);
  await runTest('WorkflowAnalyticsUseCaseFallbackNegative', testWorkflowAnalyticsUseCaseFallbackNegative);
  await runTest('WorkflowAnalyticsUseCaseBackendEntityPayloadVariants', testWorkflowAnalyticsUseCaseBackendEntityPayloadVariants);
  await runTest('WorkflowAnalyticsUseCaseBackendMalformedPayloadFallback', testWorkflowAnalyticsUseCaseBackendMalformedPayloadFallback);
  await runTest('DetailEditOrchestrationRecoverBranch', testDetailEditOrchestrationRecoverBranch);
  await runTest('DetailEditOrchestrationRetryExhaustion', testDetailEditOrchestrationRetryExhaustion);
  await runTest('DetailSaveOrchestrationUseCase', testDetailSaveOrchestrationUseCase);
  await runTest('SearchApplicationServiceTimeoutFallback', testSearchApplicationServiceTimeoutFallback);
  await runTest('SearchApplicationServiceConflictFallback', testSearchApplicationServiceConflictFallback);
  await runTest('SearchApplicationServiceNetworkFallback', testSearchApplicationServiceNetworkFallback);
  await runTest('DetailSaveOrchestrationUseCaseErrorBranch', testDetailSaveOrchestrationUseCaseErrorBranch);
  await runTest('DetailSaveOrchestrationUseCaseConflictBranch', testDetailSaveOrchestrationUseCaseConflictBranch);
  await runTest('DetailSaveOrchestrationUseCaseNetworkBranch', testDetailSaveOrchestrationUseCaseNetworkBranch);
  await runTest('SearchLoadFilterUseCase', testSearchLoadFilterUseCase);
  await runTest('SearchLoadFilterUseCaseNegative', testSearchLoadFilterUseCaseNegative);
  await runTest('SearchSmartCoordinatorMetadataRecoveryIntegration', testSearchSmartCoordinatorMetadataRecoveryIntegration);
  await runTest('SearchFilterHintPresentationUseCase', testSearchFilterHintPresentationUseCase);
  await runTest('SearchInlineAnalyticsPresentationUseCase', testSearchInlineAnalyticsPresentationUseCase);
  await runTest('SearchInlineAnalyticsRefreshOrchestrationUseCase', testSearchInlineAnalyticsRefreshOrchestrationUseCase);

  if (process.argv.includes('--json')) {
    console.log(JSON.stringify({ status: 'ok', results }, null, 2));
    return;
  }
  console.log('unit-smoke: ok');
}

main().catch((e) => {
  if (process.argv.includes("--json")) {
    console.log(JSON.stringify({ status: "failed", results, error: e.message }, null, 2));
  } else {
    console.error(e);
  }
  process.exit(1);
});
