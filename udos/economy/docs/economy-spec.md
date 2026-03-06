# Architecture Economy Spec

## Units
- **ArchCredits**: earned by debt reduction and quality improvements.
- **DebtTax**: tax for architecture debt growth.
- **RiskPremium**: risk surcharge (ADT + historical risk).
- **BudgetBalance**: available architecture budget balance.

## Transparent formulas
- `BaseCost = files*fileCost + locDelta*locCost + risk-touch adders`
- `TotalCost = BaseCost + RiskPremium + DebtTax`
- `NetCost = TotalCost - CreditsEarned`
- Decision:
  - if `NetCost <= Balance` => CLEAR / CLEAR WITH CONDITIONS
  - else => DENY (with exit plan)

## Credits
Credits are earned for:
- duplication reduction
- complexity reduction
- violation fixes
- ADT score increase
- invariant proof scenarios

## Balance updates
- Balance decreases by `NetCost`
- Emergency override may force merge but applies heavy extra debt and repayment mission.

## Bankruptcy handling
If balance is insufficient:
- DENY by default,
- suggest split + mission + proof scenarios,
- optional `EmergencyOverrideToken` (env `UDOS_EMERGENCY_OVERRIDE_TOKEN`) allows temporary override with debt penalty.
