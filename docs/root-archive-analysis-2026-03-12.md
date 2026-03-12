# Root Archive Analysis (2026-03-12)

## Scope

Reviewed `files.zip` and root companion assets:

- `App.view.xml`
- `Search.view.xml`
- `00_tokens.css`
- `21_controls.css`
- `DetailControlRail.fragment.xml`
- `zcl_zodata_*.abap`
- `zif_zodata_bopf_mapper.intf.abap`
- `oil-slick-bg-dev.html`
- access denied SVG assets

## Applied to live project

- Merged accessibility landmarks and aria contracts from archive XML into:
  - `app/view/App.view.xml`
  - `app/view/Search.view.xml`
- Added missing design tokens/fallback tokens from archive into:
  - `app/css/modules/00_tokens.css`
- Root access denied SVG assets were already productized earlier into:
  - `app/assets/illustrations/detail-access-denied-light.svg`
  - `app/assets/illustrations/detail-access-denied-dark.svg`
- Root background demo `oil-slick-bg-dev.html` was already decomposed earlier into the live background runtime and CSS module.

## Intentionally not copied as-is

- `21_controls.css`
  - Live project already contains an evolved controls layer with overlapping selectors and additional follow-up fixes. Blind replacement would create regression risk and duplicate rules.
- `DetailControlRail.fragment.xml`
  - No higher-value delta over the current live fragment was identified during this pass.
- ABAP source files from archive
  - These are already represented in `backend/sap_backend/src`.
  - Archive copies are useful as provenance, but not as runtime changes.

## Architectural conclusion

The archive contained useful source material, but not a clean drop-in patch set.
The high-value improvements were selective:

- stronger a11y semantics,
- missing design tokens,
- productized root assets already integrated earlier,
- ABAP artifacts confirmed as duplicates of the canonical SAP backend source folder.

This keeps the UI5 frontend aligned with SAP Gateway migration goals without introducing parallel implementations or non-canonical runtime paths.
