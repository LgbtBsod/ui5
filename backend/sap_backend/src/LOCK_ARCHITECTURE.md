# Canonical Lock Architecture for SAP Gateway

## Overview
Enterprise-grade pessimistic row-level locking for `ztodata_hdr` persistence layer.

**Key Principles:**
- Single source of truth: `ztodata_hdr` columns (no parallel tables)
- Session-based ownership (session_guid mandatory for lifecycle operations)
- 600-second TTL with automatic expiration validation
- Enqueue integration for process-level lock consistency
- Code-to-Data approach: column-embedded lock state

## Lock State Persistence

### ztodata_hdr Columns
| Column | Type | Purpose |
|--------|------|---------|
| `lock_owner` | SYUNAME | User who acquired lock |
| `lock_session` | STRING(36) | Session UUID of lock holder |
| `tab_session_id` | STRING | SAP transaction session ID |
| `lock_expires_at` | TIMESTAMPL | Expiration timestamp (UTC) |
| `last_touch_at` | TIMESTAMPL | Last heartbeat/touch timestamp |
| `last_touch_by` | SYUNAME | User who last refreshed lock |

## Operation Modes (zcl_zodata_lock_constants)

### Acquire (A)
- **Precondition:** No active lock OR force_takeover=true
- **Actions:** ENQUEUE lock → Update ztodata_hdr → COMMIT
- **Result:** Exclusive ownership established
- **Exceptions:** lock_error (ENQUEUE failed), update_error (persistence failed)

### Release (R)
- **Precondition:** Active lock owned by requesting session
- **Actions:** DEQUEUE lock → Clear columns → COMMIT
- **Result:** Lock released for other sessions
- **Exceptions:** lock_error (session mismatch), update_error (DB failure)

### Heartbeat (H)
- **Precondition:** Active lock with matching session_guid
- **Actions:** Update timestamps (last_touch_at, lock_expires_at) → COMMIT
- **Result:** TTL refreshed by 600 seconds
- **Exceptions:** update_error (session mismatch)

### Status (S)
- **Precondition:** None (read-only)
- **Actions:** SELECT lock state
- **Result:** Current lock ownership & expiration status
- **Exceptions:** lock_error (no lock or expired)

### Validate (V)
- **Precondition:** None (read-only)
- **Actions:** SELECT lock state + session match check
- **Result:** Validation pass/fail for requesting session
- **Exceptions:** lock_error (no lock, expired, or session mismatch)

### Touch (T)
- **Precondition:** None (system operation)
- **Actions:** Update timestamps without session validation
- **Result:** Technical refresh (used by background jobs)
- **Exceptions:** update_error (DB failure)

## Session Ownership Semantics

**Mandatory for:**
- Heartbeat (H): Must match `lock_session`
- Release (R): Must match `lock_session`
- Validate (V): Session mismatch raises lock_error
- Status (S): Optional (returns owner info regardless)

**Optional for:**
- Acquire (A): Can be null (anonymous acquisition)
- Touch (T): Ignored (system operation)

## Transaction & Consistency

### ENQUEUE/DEQUEUE Integration
- **Purpose:** Process-level lock consistency across SAP instances
- **Timing:** Enqueue BEFORE update, Dequeue AFTER release
- **Isolation:** Ensures no concurrent modifications within SAP cluster
- **Lock Name:** `EZODATA_LCK` (BO/Object-specific)

### TTL & Expiration Validation
- **Formula:** `lock_expires_at = last_touch_at + 600 seconds`
- **Validation:** All status/heartbeat operations check `lock_expires_at <= now`
- **Auto-expiry:** No cleanup job required; validation enforces TTL at read-time
- **Orphan cleanup:** Optional background job can purge expired rows

## Architecture Components

### zcl_zodata_lock_manager (Main interface)
- **Responsibility:** Public API for Gateway runtime
- **Methods:**
  - `acquire()`: Lock acquisition with optional takeover
  - `release()`: Lock release
  - `heartbeat()`: TTL refresh
  - `status()`: Lock state query
  - `ensure_session_lock()`: Validate ownership
  - `lock()` / `unlock()`: Simplified form
  - `update_last_touch()`: Background job refresh

### ZODATA_LOCK_CONTROL (Function module)
- **Responsibility:** Persistence layer & Enqueue coordination
- **Modes:** A, R, H, S, V, T (via mode parameter)
- **Error handling:** lock_error, update_error, others

### zcl_zodata_lock_constants
- **Responsibility:** Centralized mode/exception constants
- **Usage:** Prevents magic-string errors

### zif_zodata_lock_manager (Interface)
- **Responsibility:** Contract definition for mock/enterprise implementations
- **Versioning:** Stable, backward-compatible

## Security & Authorization

### AUTHORITY-CHECK
- **Applied:** No distributed authority-check in lock functions
- **Rationale:** Gateway API layer handles authorization upstream
- **Assumption:** Caller has already been vetted for object access

### Audit Trail
- **Who:** `last_touch_by` column tracks refresh operations
- **What:** Lock acquire/release logged separately (see LockLog)
- **When:** Timestamps capture precise moment

## Performance Characteristics

### Single-row locking (Code-to-Data)
- **Advantage:** No separate lock table; minimal I/O
- **Disadvantage:** Lock state shares row with entity (cache-warming on read)
- **Index:** Implicit on `(bo_key, object_id)` primary key

### Enqueue as bottleneck
- **Scalability:** Fixed queue; ~1000 concurrent locks per SAP instance
- **Mitigation:** TTL ensures automatic cleanup of orphans
- **Cluster:** Lock name includes BO/Object uniqueness

## Migration & Legacy Notes

- **Removed:** Parallel `zlock_regs` table (Dec 2024)
- **Rationale:** Duplicate maintenance burden; ztodata_hdr sufficient
- **Breaking change:** None (zlock_regs was internal)

## References

- SAP Clean Core: Code-to-Data principle
- ABAP new syntax (7.40+): CASE/WHEN, COND, SELECT INTO with field mapping
- OData v2 pessimistic locking: RFC 5023
