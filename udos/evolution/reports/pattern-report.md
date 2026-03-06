# PATTERN REPORT

Stable patterns:
- usecase → port → adapter → state mutation (stability: 98%)
- controller → facade → usecase (stability: 88%)

Anti-patterns:
- controller calling domain helper directly (occurrences: 2, severity: low)
- controller → adapter (occurrences: 0, severity: low)
