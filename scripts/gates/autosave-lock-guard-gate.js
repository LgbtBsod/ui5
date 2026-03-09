#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const componentFile = path.resolve(__dirname, '../../app/Component.js');
const coordinatorFile = path.resolve(__dirname, '../../app/service/runtime/AutoSaveCoordinator.js');
const useCaseFile = path.resolve(__dirname, '../../app/service/domain/detail/usecases/AutosaveDetailUseCase.js');
const componentTxt = fs.readFileSync(componentFile, 'utf8');
const coordinatorTxt = fs.readFileSync(coordinatorFile, 'utf8');
const useCaseTxt = fs.readFileSync(useCaseFile, 'utf8');

const hasComponentEditLock =
  /WORKFLOW_EDIT_MODE/.test(componentTxt) &&
  /WORKFLOW_LOCK_STATUS/.test(componentTxt) &&
  /=== "EDIT"/.test(componentTxt) &&
  /=== "LOCKED"/.test(componentTxt);
const hasCoordinatorLockGuard =
  /lockGuardFn/.test(coordinatorTxt) &&
  /_fnShouldSave/.test(coordinatorTxt) &&
  /autosave aborted: mode must be EDIT and lockOperationState must be LOCKED/.test(coordinatorTxt);
const hasUseCaseGuard =
  /WORKFLOW_EDIT_MODE/.test(useCaseTxt) &&
  /WORKFLOW_LOCK_STATUS/.test(useCaseTxt) &&
  /WORKFLOW_DIRTY/.test(useCaseTxt) &&
  /sEditMode === "EDIT"/.test(useCaseTxt) &&
  /sLockStatus === "LOCKED"/.test(useCaseTxt) &&
  /&& bDirty/.test(useCaseTxt);

if (!hasComponentEditLock || !hasCoordinatorLockGuard || !hasUseCaseGuard) {
  console.error('autosave-lock-guard-gate failed: missing EDIT/LOCKED/dirty guard in autosave path');
  process.exit(1);
}
console.log('autosave-lock-guard-gate passed');
