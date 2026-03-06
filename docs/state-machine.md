# Detail State Machine

## Modes
- READ
- CREATE
- EDIT

## Lock states
- IDLE
- LOCKED

## Allowed transitions
- READ -> EDIT only after successful lock acquire.
- CREATE -> EDIT only after lock acquire.
- EDIT -> READ after close or lock lost.
- LOCKED -> IDLE after release or lock lost.
