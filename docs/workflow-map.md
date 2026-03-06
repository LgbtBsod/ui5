# Workflow Map

## searchWorkflow
1. init search facade
2. execute search usecase
3. bind search results
4. select row state transition
- expected transitions:
  - results:IDLE->LOADED
  - selection:NONE->ACTIVE

## detailOpenWorkflow
1. select search row
2. fetch detail via facade
3. validate cache LastChangeSet
4. hydrate detail model
- expected transitions:
  - detail:CLOSED->OPEN
  - detailHydration:PENDING->READY

## editWorkflow
1. enter edit intent
2. tryAcquireLock
3. set editMode=EDIT on success
4. activate autosave when dirty
- expected transitions:
  - editMode:READ->EDIT
  - lockState:IDLE->LOCKED

## lockWorkflow
1. acquire lock
2. monitor lock heartbeat
3. handle killed/lockLost to READ
- expected transitions:
  - lockState:IDLE->LOCKED
  - lockLost:LOCKED->FAILED

## autosaveWorkflow
1. dirty=true in EDIT
2. autosave ACTIVE when LOCKED
3. stop autosave on lock lost
- expected transitions:
  - autosave:IDLE->ACTIVE
  - lockLost:ACTIVE->IDLE

## cacheValidationWorkflow
1. read IndexedDB cache
2. compare AggChangedOn with server stamp
3. accept if abs(diff)<=5500ms
- expected transitions:
  - cacheValidation:PENDING->VALID|INVALID

