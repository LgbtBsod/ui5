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
  assert(bootCalled === 0, 'syncAvailability should not bootstrap when metadata failed');

  state['/mainServiceMetadataOk'] = true;
  mod.syncAvailability({ stateModel, viewModel, unavailableText: 'fallback', bootstrap: () => { bootCalled += 1; } });
  assert(viewState['/useSmartControls'] === true, 'syncAvailability should re-enable controls on metadata recovery');
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
  assert(bindingParams.parameters.top === 150, 'applyRebindParams should set top parameter');
  bindingParams.events.dataReceived({});
  assert(dataReceivedCalled === 1, 'applyRebindParams should chain dataReceived callback');

  let fallbackCalls = 0;
  mod.rebindOrFallback({ enabled: false, fallbackSearch: () => { fallbackCalls += 1; } });
  assert(fallbackCalls === 1, 'rebindOrFallback should call fallback when disabled');
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

function testDetailSaveConflictUseCase() {
  const mod = loadSapModule('service/usecase/DetailSaveConflictUseCase.js');

  let reloaded = 0;
  let overwritten = 0;
  mod.handleConflictChoice('reload', {
    reloadLabel: 'reload',
    overwriteLabel: 'overwrite',
    onReload: () => { reloaded += 1; return Promise.resolve(); },
    onOverwrite: () => { overwritten += 1; return Promise.resolve(); }
  });
  assert(reloaded === 1 && overwritten === 0, 'handleConflictChoice should dispatch reload');
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

  const result = {
    checkLists: [{ id: '1' }],
    savedChecklist: { root: { id: '1' } }
  };

  mod.applySaveSuccess({
    result,
    dataModel,
    selectedModel,
    stateModel: {},
    dispatchFullSave: () => { dispatched += 1; },
    showSavedToast: () => { toasted += 1; }
  });

  assert(Array.isArray(dataState['/checkLists']) && dataState['/visibleCheckLists'].length === 1,
    'applySaveSuccess should write checklist collections');
  assert(selected && selected.root.id === '1', 'applySaveSuccess should refresh selected model');
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

async function main() {
  await runTest('DetailLifecycleUseCase', testDetailLifecycleUseCase);
  await runTest('SearchSmartControlCoordinator', testSearchSmartControlCoordinator);
  await runTest('SearchWorkflowOrchestrator', testSearchWorkflowOrchestrator);
  await runTest('ChecklistValidationService', testChecklistValidationService);
  await runTest('DeltaPayloadBuilder', testDeltaPayloadBuilder);
  await runTest('SearchPresentationUseCase', testSearchPresentationUseCase);
  await runTest('DetailDialogLifecycleUseCase', testDetailDialogLifecycleUseCase);
  await runTest('SmartSearchAdapterFilterModes', testSmartSearchAdapterFilterModes);
  await runTest('SearchActionUseCase', testSearchActionUseCase);
  await runTest('DetailCommandFlowUseCase', testDetailCommandFlowUseCase);
  await runTest('SearchUiFlowUseCase', testSearchUiFlowUseCase);
  await runTest('DetailSaveConflictUseCase', testDetailSaveConflictUseCase);
  await runTest('DetailEditOrchestrationUseCase', testDetailEditOrchestrationUseCase);
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
  await runTest('SearchIntentUseCase', testSearchIntentUseCase);
  await runTest('WorkflowAnalyticsUseCaseFallback', testWorkflowAnalyticsUseCaseFallback);
  await runTest('WorkflowAnalyticsUseCaseFallbackNegative', testWorkflowAnalyticsUseCaseFallbackNegative);
  await runTest('DetailEditOrchestrationRecoverBranch', testDetailEditOrchestrationRecoverBranch);
  await runTest('DetailSaveOrchestrationUseCase', testDetailSaveOrchestrationUseCase);
  await runTest('SearchApplicationServiceTimeoutFallback', testSearchApplicationServiceTimeoutFallback);
  await runTest('SearchApplicationServiceConflictFallback', testSearchApplicationServiceConflictFallback);
  await runTest('DetailSaveOrchestrationUseCaseErrorBranch', testDetailSaveOrchestrationUseCaseErrorBranch);
  await runTest('DetailSaveOrchestrationUseCaseConflictBranch', testDetailSaveOrchestrationUseCaseConflictBranch);
  await runTest('SearchLoadFilterUseCase', testSearchLoadFilterUseCase);
  await runTest('SearchLoadFilterUseCaseNegative', testSearchLoadFilterUseCaseNegative);
  await runTest('SearchSmartCoordinatorMetadataRecoveryIntegration', testSearchSmartCoordinatorMetadataRecoveryIntegration);

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
