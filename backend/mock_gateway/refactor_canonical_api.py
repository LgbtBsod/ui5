#!/usr/bin/env python3
"""
Automated SOLID/DRY refactoring for gateway_canonical_api.py
Applies gateway_helpers consolidation patterns systematically.
"""
import re
from pathlib import Path
import sys


def refactor_payload_extraction(content: str) -> tuple[str, int]:
    """Replace _pick_text, _pick_bool, _coerce_int with PayloadExtractor. Returns (new_content, replacements)."""
    original = content

    # Replace _pick_text() calls - handle multi-line
    content = re.sub(
        r'_pick_text\(([\s\S]*?)\)',
        lambda m: f'PayloadExtractor.text({m.group(1)})',
        content,
        count=100
    )

    # Replace _pick_bool() calls
    content = re.sub(
        r'_pick_bool\(([\s\S]*?)\)',
        lambda m: f'PayloadExtractor.bool({m.group(1)})',
        content,
        count=100
    )

    # Replace _coerce_int() calls
    content = re.sub(
        r'_coerce_int\(([\s\S]*?)\)',
        lambda m: f'PayloadExtractor.int({m.group(1)})',
        content,
        count=100
    )

    # Count replacements
    replacements = (
        len(re.findall(r'PayloadExtractor\.text\(', content)) +
        len(re.findall(r'PayloadExtractor\.bool\(', content)) +
        len(re.findall(r'PayloadExtractor\.int\(', content))
    )

    return content, replacements


def refactor_date_parsing(content: str) -> tuple[str, int]:
    """Replace _date_ymd_from_any, _date_ms_from_any with DateParser."""
    original = content

    content = re.sub(
        r'_date_ymd_from_any\(([\s\S]*?)\)',
        lambda m: f'DateParser.to_ymd({m.group(1)})',
        content,
        count=100
    )

    content = re.sub(
        r'_date_ms_from_any\(([\s\S]*?)\)',
        lambda m: f'DateParser.to_ms({m.group(1)})',
        content,
        count=100
    )

    replacements = (
        len(re.findall(r'DateParser\.to_ymd\(', content)) +
        len(re.findall(r'DateParser\.to_ms\(', content))
    )

    return content, replacements


def refactor_key_resolution(content: str) -> tuple[str, int]:
    """Replace _entity_key, _boundary_root_key, _boundary_parent_key with BoundaryResolver."""
    original = content

    content = re.sub(
        r'_entity_key\(([\s\S]*?)\)',
        lambda m: f'BoundaryResolver.resolve_key({m.group(1)})',
        content,
        count=100
    )

    content = re.sub(
        r'_boundary_root_key\(([\s\S]*?)\)',
        lambda m: f'BoundaryResolver.resolve_root_key({m.group(1)})',
        content,
        count=100
    )

    content = re.sub(
        r'_boundary_parent_key\(([\s\S]*?)\)',
        lambda m: f'BoundaryResolver.resolve_parent_key({m.group(1)})',
        content,
        count=100
    )

    replacements = (
        len(re.findall(r'BoundaryResolver\.resolve_key\(', content)) +
        len(re.findall(r'BoundaryResolver\.resolve_root_key\(', content)) +
        len(re.findall(r'BoundaryResolver\.resolve_parent_key\(', content))
    )

    return content, replacements


def refactor_filter_logic(content: str) -> tuple[str, int]:
    """Replace _export_segment_match with FilterMatcher."""
    original = content

    # Replace _export_segment_match calls
    content = re.sub(
        r'_export_segment_match\(([\s\S]*?)\)',
        lambda m: f'FilterMatcher.evaluate_segment({m.group(1)})',
        content,
        count=100
    )

    replacements = len(re.findall(r'FilterMatcher\.evaluate_segment\(', content))

    return content, replacements


def add_imports(content: str) -> bool:
    """Add gateway_helpers imports. Returns True if added."""
    if 'from gateway_helpers import' not in content:
        import_block = (
            "from gateway_helpers import (\n"
            "    PayloadExtractor, DateParser, BoundaryResolver,\n"
            "    ODataSerializer, FilterMatcher, DictionaryCache, Aggregator\n"
            ")\n"
        )

        # Add after gateway_operations import
        if 'from gateway_operations import' in content:
            content = re.sub(
                r'(from gateway_operations import[\s\S]*?\n)',
                r'\1' + import_block,
                content
            )
            return True

    return False


def remove_old_definitions(content: str) -> int:
    """Remove old helper function definitions. Returns count of removed functions."""

    functions_to_remove = [
        '_pick_text',
        '_pick_bool',
        '_pick_first_present',
        '_coerce_int',
        '_date_ymd_from_any',
        '_date_ms_from_any',
        '_entity_key',
        '_boundary_root_key',
        '_boundary_parent_key',
        '_export_segment_match',
    ]

    removed = 0
    for func in functions_to_remove:
        # Find function definition and remove it
        pattern = rf'^def {func}\(.*?\):\n(?:.*?\n)*?(?=^def |\Z)'
        if re.search(pattern, content, re.MULTILINE):
            content = re.sub(pattern, '', content, flags=re.MULTILINE)
            removed += 1

    return removed


def main():
    file_path = Path('api/gateway_canonical_api.py')

    if not file_path.exists():
        print(f"ERROR: File not found: {file_path}")
        sys.exit(1)

    print(f"[*] Reading {file_path}...")
    original_content = file_path.read_text()
    original_lines = len(original_content.split('\n'))

    content = original_content
    total_replacements = 0

    print("\n[*] Applying refactorings...\n")

    # Phase 1: Add imports
    print("  [1/5] Adding gateway_helpers imports...")
    add_imports(content)

    # Phase 2: PayloadExtractor
    print("  [2/5] Refactoring payload extraction...")
    content, count = refactor_payload_extraction(content)
    print(f"       [OK] {count} replacements")
    total_replacements += count

    # Phase 3: DateParser
    print("  [3/5] Refactoring date parsing...")
    content, count = refactor_date_parsing(content)
    print(f"       [OK] {count} replacements")
    total_replacements += count

    # Phase 4: BoundaryResolver
    print("  [4/5] Refactoring key resolution...")
    content, count = refactor_key_resolution(content)
    print(f"       [OK] {count} replacements")
    total_replacements += count

    # Phase 5: FilterMatcher
    print("  [5/5] Refactoring filter logic...")
    content, count = refactor_filter_logic(content)
    print(f"       [OK] {count} replacements")
    total_replacements += count

    # Write back
    print(f"\n[*] Writing refactored code...")
    file_path.write_text(content)

    new_lines = len(content.split('\n'))
    reduction = original_lines - new_lines

    print(f"\n[SUCCESS] Refactoring complete!")
    print(f"   Total replacements: {total_replacements}")
    print(f"   LOC reduction: {reduction} lines")
    print(f"   Before: {original_lines} LOC")
    print(f"   After: {new_lines} LOC")


if __name__ == '__main__':
    main()
