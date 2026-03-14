# P2 / P3 Repo Closeout

Date: 2026-03-14

Purpose: document what is closed for `P2` and `P3` inside the repository, and what still requires runtime or landscape validation.

## P2 Repo-Side Status

- Status: `CLOSED_IN_REPO`
- Closed scope:
  - sticky search/detail layout hardening
  - scoped focus restoration for search runtime
  - route-aware `search -> detail -> search` mock contour scenario
  - sticky search/detail accessibility semantics in views/fragments
  - focused checklist for keyboard/mobile sticky validation
  - shell/settings communication for productive constraints
- Remaining external scope:
  - live Gateway metadata validation for final value help/ordering proof
  - FLP-hosted keyboard/mobile walkthrough mapped to `EV-010`

## P3 Repo-Side Status

- Status: `CLOSED_IN_REPO`
- Closed scope:
  - light theme preserved
  - dark `oil-slick` profile ported and controlled by dev override
  - QUnit/OPA expanded beyond scaffold to runtime contracts and route-aware smoke
  - governance artifacts synchronized across remediation plan, evidence matrix, sign-off tracker, and proof records
- Remaining external scope:
  - final product decision whether dark mode stays internal or becomes exposed UX
  - live owner sign-off after SAP landscape execution

## Completion Rule

`P2` and `P3` are treated as closed for repository implementation, but not as commercial SAP-sale closure, until:

- `EV-010` is accepted
- Gateway / FLP validation is complete
- final owner sign-offs move from `IN_PROGRESS` or `OPEN` to `ACCEPTED`
