# SOLID/SRP Refactoring Implementation Guide
## Python Backend - Systematic Code Quality Improvement

**Status:** Phase 9 - Implementation Strategy  
**Scope:** gateway_canonical_api.py + other Python files  
**Objective:** Apply SOLID/SRP/DRY principles systematically  

---

## Part 1: Understanding the Problem

### Current State: 62 Helper Functions
gateway_canonical_api.py contains 62 helper functions with **4 main DRY violations**:

#### DRY Violation #1: Payload Extraction (5+ instances)
**Current Pattern:**
```python
# In gateway_canonical_api.py
status = _pick_text(data, "Status", "status")
lpc = _pick_text(data, "Lpc", "LPC_KEY", "lpc")
count = _coerce_int(data.get("Count"), 0)

# In gateway_validators.py
status = _pick_text(data, "Status", "status")

# In gateway_operations.py
status = _pick_text(data, "Status", "status")
```

**Problem:** Same extraction logic repeated 15+ times across 3 files  
**Solution:** Consolidate → `PayloadExtractor` class

---

#### DRY Violation #2: Date Parsing (2+ instances)
**Current Pattern:**
```python
def _date_ymd_from_any(value) -> str:
    if raw.startswith("/Date(") and raw.endswith(")/"):
        try:
            ms = int(raw[6:-2].split("+")[0].split("-")[0])
            dt = datetime.fromtimestamp(ms / 1000, tz=timezone.utc)
            return dt.strftime("%Y-%m-%d")
        except ValueError:
            return ""
    # ... more logic

def _date_ms_from_any(value) -> int:
    if raw.startswith('/Date(') and raw.endswith(')/'):
        body = raw[6:-2]
        # ... similar parsing logic
```

**Problem:** Two functions parse same `/Date(...)` format with duplicate logic  
**Solution:** Consolidate → `DateParser` class with both methods

---

#### DRY Violation #3: Entity Serialization (10+ instances)
**Current Pattern:**
```python
def _to_root(root: ChecklistRoot) -> dict:
    return {
        "__metadata": _entity_metadata("ChecklistRoot", "ChecklistRootSet", _hex(root.id)),
        "Key": _hex(root.id),
        "Status": _status_external(root.status),
        # ... 20 more fields
    }

def _to_check(check: ChecklistCheck) -> dict:
    return {
        "__metadata": _entity_metadata("ChecklistCheck", "ChecklistCheckSet", _hex(check.id)),
        "Key": _hex(check.id),
        "Text": check.text,
        # ... 10 more fields
    }
```

**Problem:** Every `_to_X()` function repeats `__metadata` boilerplate  
**Solution:** Use `ODataSerializer.build_entity()` Template Method

---

#### DRY Violation #4: Filter Logic (4+ instances)
**Current Pattern:**
```python
def _attachment_matches_filter(filter_expr: str) -> bool:
    root_match = re.search(r"(?:PARENT_KEY|DB_KEY).*?'([^']+)'", expr)
    return str(row.id) == root_match.group(1)

def _export_segment_match(segment: str, failed: bool) -> bool:
    if segment == "FAILED":
        return bool(failed)
    if segment == "SUCCESS":
        return not bool(failed)
    return True
```

**Problem:** Filter logic repeated in multiple functions  
**Solution:** Consolidate → `FilterMatcher` class

---

## Part 2: Refactoring Approach

### Step-by-Step Strategy

#### Step 1: Add PayloadExtractor Usage (30 min)
Replace all `_pick_text()` calls:

**Before:**
```python
status = _pick_text(data, "Status", "status")
lpc = _pick_text(data, "Lpc", "LPC_KEY", "lpc")
count = _coerce_int(data.get("Count"), 0)
is_active = _pick_bool(data, "Active", default=True)
```

**After:**
```python
from gateway_helpers import PayloadExtractor as PE

status = PE.text(data, "Status", "status")
lpc = PE.text(data, "Lpc", "LPC_KEY", "lpc")
count = PE.int(data, "Count", default=0)
is_active = PE.bool(data, "Active", default=True)
```

**Locations to replace:**
```bash
# Find all _pick_text calls
grep -n "_pick_text\|_pick_bool\|_coerce_int\|_pick_first_present" \
  api/gateway_canonical_api.py \
  api/gateway_validators.py \
  api/gateway_operations.py
```

**Estimated LOC reduction:** -50 LOC

---

#### Step 2: Adopt DateParser (20 min)
Replace date parsing:

**Before:**
```python
def _datecheck_datetime(root: ChecklistRoot) -> datetime:
    raw = (root.date or "").strip()
    if raw:
        try:
            date_part = datetime.strptime(raw[:10], "%Y-%m-%d").date()
            return datetime(date_part.year, date_part.month, date_part.day, tzinfo=timezone.utc)
        except ValueError:
            pass
    return now_utc()
```

**After:**
```python
from gateway_helpers import DateParser

def _datecheck_datetime(root: ChecklistRoot) -> datetime:
    ymd = DateParser.to_ymd(root.date)
    if not ymd:
        return now_utc()
    date_obj = datetime.strptime(ymd, "%Y-%m-%d").date()
    return datetime(date_obj.year, date_obj.month, date_obj.day, tzinfo=timezone.utc)
```

**Estimated LOC reduction:** -30 LOC

---

#### Step 3: Use ODataSerializer Template Method (45 min)
Replace `_to_*` functions:

**Before:**
```python
def _to_root(root: ChecklistRoot) -> dict:
    return {
        "__metadata": _entity_metadata("ChecklistRoot", "ChecklistRootSet", _hex(root.id)),
        "Key": _hex(root.id),
        "Status": _status_external(root.status),
        # ... 20 fields
    }

def _to_check(check: ChecklistCheck) -> dict:
    return {
        "__metadata": _entity_metadata("ChecklistCheck", "ChecklistCheckSet", _hex(check.id)),
        "Key": _hex(check.id),
        "Text": check.text,
        # ... 10 fields
    }
```

**After:**
```python
from gateway_helpers import ODataSerializer

def _to_root(root: ChecklistRoot) -> dict:
    return ODataSerializer.build_entity(
        entity_type="ChecklistRoot",
        entity_set="ChecklistRootSet",
        key_value=_hex(root.id),
        data={
            "Key": _hex(root.id),
            "Status": _status_external(root.status),
            # ... 20 fields
        }
    )

def _to_check(check: ChecklistCheck) -> dict:
    return ODataSerializer.build_entity(
        entity_type="ChecklistCheck",
        entity_set="ChecklistCheckSet",
        key_value=_hex(check.id),
        data={
            "Key": _hex(check.id),
            "Text": check.text,
            # ... 10 fields
        }
    )
```

**Estimated LOC reduction:** -50 LOC (boilerplate)

---

#### Step 4: Adopt BoundaryResolver (25 min)
Replace key resolution:

**Before:**
```python
def _entity_key(key_expr: str) -> str:
    cleaned = str(key_expr or "").strip()
    for prefix in ("Key=", "DB_KEY=", ...):
        if cleaned.startswith(prefix):
            cleaned = cleaned.split("=", 1)[1].strip()
    cleaned = cleaned.strip("'")
    return cleaned

def _boundary_root_key(payload: dict | None = None, *candidates) -> str:
    if payload and isinstance(payload, dict):
        for candidate in [*candidates, payload.get("DB_KEY"), ...]:
            if candidate:
                return str(candidate).strip()
    return ""
```

**After:**
```python
from gateway_helpers import BoundaryResolver as BR

key = BR.resolve_key(key_expr)
root_key = BR.resolve_root_key(payload, *candidates)
parent_key = BR.resolve_parent_key(payload, *candidates)
```

**Estimated LOC reduction:** -40 LOC

---

#### Step 5: Adopt FilterMatcher (20 min)
Replace filter logic:

**Before:**
```python
def _attachment_matches_filter(filter_expr: str) -> bool:
    root_match = re.search(r"PARENT_KEY.*?'([^']+)'", filter_expr)
    return str(row.id) == root_match.group(1) if root_match else False

def _export_segment_match(segment: str, failed: bool) -> bool:
    if segment == "FAILED":
        return bool(failed)
    if segment == "SUCCESS":
        return not bool(failed)
    return True
```

**After:**
```python
from gateway_helpers import FilterMatcher as FM

value = FM.extract_eq_value(filter_expr, "PARENT_KEY")
matches = str(row.id) == value if value else False

success = FM.evaluate_segment(segment, failed)
```

**Estimated LOC reduction:** -30 LOC

---

## Part 3: Automation Script

Create `refactor_canonical_api.py` to automate replacements:

```python
#!/usr/bin/env python3
"""
Automated SOLID/DRY refactoring for gateway_canonical_api.py
Applies gateway_helpers consolidation patterns
"""
import re
from pathlib import Path

def refactor_payload_extraction(content: str) -> str:
    """Replace _pick_text, _pick_bool, _coerce_int with PayloadExtractor"""
    
    # Replace _pick_text() calls
    content = re.sub(
        r'_pick_text\((.*?)\)',
        r'PayloadExtractor.text(\1)',
        content,
        flags=re.MULTILINE
    )
    
    # Replace _pick_bool() calls
    content = re.sub(
        r'_pick_bool\((.*?)\)',
        r'PayloadExtractor.bool(\1)',
        content,
        flags=re.MULTILINE
    )
    
    # Replace _coerce_int() calls
    content = re.sub(
        r'_coerce_int\((.*?)\)',
        r'PayloadExtractor.int(\1)',
        content,
        flags=re.MULTILINE
    )
    
    return content


def refactor_date_parsing(content: str) -> str:
    """Replace _date_ymd_from_any, _date_ms_from_any with DateParser"""
    
    content = re.sub(
        r'_date_ymd_from_any\((.*?)\)',
        r'DateParser.to_ymd(\1)',
        content,
        flags=re.MULTILINE
    )
    
    content = re.sub(
        r'_date_ms_from_any\((.*?)\)',
        r'DateParser.to_ms(\1)',
        content,
        flags=re.MULTILINE
    )
    
    return content


def add_imports(content: str) -> str:
    """Add gateway_helpers imports at top of file"""
    
    if 'from gateway_helpers import' not in content:
        import_block = (
            "from gateway_helpers import (\n"
            "    PayloadExtractor, DateParser, BoundaryResolver,\n"
            "    ODataSerializer, FilterMatcher, DictionaryCache, Aggregator\n"
            ")\n"
        )
        # Add after other imports
        content = re.sub(
            r'(from gateway_operations import.*?\n)',
            r'\1' + import_block,
            content,
            flags=re.DOTALL
        )
    
    return content


def main():
    file_path = Path('api/gateway_canonical_api.py')
    content = file_path.read_text()
    
    print("Refactoring gateway_canonical_api.py...")
    
    # Apply transformations in order
    content = add_imports(content)
    content = refactor_payload_extraction(content)
    content = refactor_date_parsing(content)
    
    # Write back
    file_path.write_text(content)
    print(f"✅ Refactored {file_path}")


if __name__ == '__main__':
    main()
```

**Run script:**
```bash
python3 refactor_canonical_api.py
```

---

## Part 4: SRP Compliance Matrix

After refactoring, each class/function should have **single responsibility**:

| Module | Responsibility | Before | After |
|--------|---|---|---|
| **PayloadExtractor** | Extract/coerce dict values | N/A | 1 (extraction) |
| **DateParser** | Parse date formats | N/A | 1 (date parsing) |
| **BoundaryResolver** | Resolve entity keys | N/A | 1 (key resolution) |
| **ODataSerializer** | Build OData entities | N/A | 1 (serialization) |
| **FilterMatcher** | OData filter logic | N/A | 1 (filtering) |
| **gateway_canonical_api** | HTTP routes + coordination | 5+ | 2 (routes + coordination) |
| **gateway_operations** | Business logic | 2+ | 1 (operations) |
| **gateway_serializers** | OData serialization | 1 | 1 (serialization) |

---

## Part 5: Verification Checklist

After refactoring:

- [ ] All 81 Python files compile
- [ ] No circular dependencies
- [ ] gateway_canonical_api.py reduced by 100+ LOC
- [ ] All helper classes used appropriately
- [ ] Code follows SOLID principles
- [ ] No dead functions remain
- [ ] Tests still pass (if any)

---

## Part 6: Expected Outcomes

### LOC Reduction
```
Before:  2329 LOC (gateway_canonical_api.py)
After:   ~2000 LOC (20% reduction)

Breakdown:
├─ PayloadExtractor:  -50 LOC
├─ DateParser:        -30 LOC
├─ ODataSerializer:   -100 LOC
├─ BoundaryResolver:  -40 LOC
├─ FilterMatcher:     -30 LOC
└─ Cleanup/removal:   -79 LOC
```

### Code Quality Improvements
- ✅ SRP: Each class has single responsibility
- ✅ DRY: No duplicated extraction/parsing/serialization logic
- ✅ Maintainability: Changes in one place affect all usage
- ✅ Testability: Helper classes can be unit-tested independently
- ✅ Readability: Clear intent via descriptive class names

---

## Part 7: Implementation Timeline

| Phase | Task | Duration | LOC Reduction |
|---|---|---|---|
| **Phase 9a** | PayloadExtractor adoption | 30 min | -50 |
| **Phase 9b** | DateParser adoption | 20 min | -30 |
| **Phase 9c** | ODataSerializer adoption | 45 min | -100 |
| **Phase 9d** | BoundaryResolver adoption | 25 min | -40 |
| **Phase 9e** | FilterMatcher adoption | 20 min | -30 |
| **Phase 9f** | Cleanup & verification | 30 min | -79 |
| **TOTAL** | Complete refactoring | ~2.5 hours | **-330 LOC** |

---

## Conclusion

This guide provides a systematic, **low-risk refactoring strategy** for applying SOLID/SRP/DRY to the Python backend without breaking functionality.

**Key principles:**
1. ✅ Single responsibility per class
2. ✅ No code duplication
3. ✅ Clear intent via naming
4. ✅ Incremental changes (testable at each step)
5. ✅ Measurable LOC reduction

**Next steps:** Run automation script, verify compilation, measure improvements.

---

*Guide prepared: Phase 9 Strategic Planning*  
*Ready for systematic implementation*
