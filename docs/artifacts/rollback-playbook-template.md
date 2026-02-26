# Rollback Playbook Template (Risky Style Changes)

## Change Identifier
- PR:
- Commit:
- Owner:

## Risk Assessment
- Risk level:
- Potential user impact:
- Affected flows:

## Trigger Conditions for Rollback
- [ ] Gate failure in production branch
- [ ] Critical accessibility regression
- [ ] Visual parity drift (Morning/Night)
- [ ] Performance regression on low-end profile

## Rollback Steps
1. Revert style bundle commit(s).
2. Disable new style gate(s) via guarded pipeline toggle if needed.
3. Restore last known good CSS artifact and token map.
4. Re-run mandatory gates and smoke checks.

## Verification after Rollback
- [ ] CSS accent governance gate pass
- [ ] Semantic contrast gate pass
- [ ] a11y gate pass
- [ ] smoke gate pass

## Follow-up Actions
- Root cause analysis:
- Preventive action:
- ETA for fixed re-release:
