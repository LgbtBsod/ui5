# shadow-dup.duplicate-critical-module

Detects duplicate critical runtime module basenames (e.g. `*Facade.js`, `*UseCase.js`, `*Adapter.js`, `*Manager.js`, `*Coordinator.js`, `*Loader.js`, `*Policy.js`, `*Builder.js`, `*Mapper.js`).

## Severity rules
- HIGH when one duplicate exists under shadow legacy folders (`service/search`, `service/detail`, `service/autosave`).
- MEDIUM otherwise.

## Fix
- Keep one canonical implementation.
- Migrate importers to canonical module.
- Remove duplicate only after reverse-deps proof + full QA PASS.
