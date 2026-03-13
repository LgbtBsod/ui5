# Final Bundle Ownership Map

Date: 2026-03-13

## Eager Bundle

- `App.view.xml`
- shell runtime and app controller surface
- search critical fragments
- search critical CSS

## Lazy Detail Bundle

- `Detail.view.xml`
- detail route fragments
- detail page CSS aggregators and zoned detail style modules

## Lazy Analytics Bundle

- `Analytics.view.xml`
- analytics drilldown and report fragments
- analytics CSS

## Deferred Dialog Bundle

- search sort/group dialogs
- analytics report dialog
- year-picker and value-help heavy UI

## Structural Invariants

- page-level CSS aggregators only import zoned submodules
- fragment-heavy screens are split so that bundle boundaries match feature boundaries
- sanctioned bundle entry shells may remain even when structurally thin
