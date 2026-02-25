# SAP Lead Architect Audit (UI5 Checklist App)

## 1) Executive summary

Проект демонстрирует хороший задел по архитектуре (разделение fake/real backend, lock lifecycle, autosave, routing guard), но требует выравнивания под production-профиль SAP:
- переход на нативные Smart controls на поиске,
- унификация lifecycle-статусов и overall-result,
- усиление валидатора бизнес-обязательности,
- улучшение backend-driven конфигурации (required fields, timers, feature flags).

## 2) Strong sides

- Четкое разделение backend adapter/fake/real и возможность эмуляции SAP flow.
- Есть lock heartbeat/status/release, navigation guard, autosave.
- JSON-model first подход удобен для локальной отладки.
- Подготовлена основа для smart-search и frontend config с бэка.

## 3) Weak sides / risks

- Много UI-логики в контроллерах (частично решено, но нужно дальше выносить в сервисы).
- Смешаны 2 типа статусов: lifecycle (`DRAFT/REGISTERED/CLOSED`) и quality-result (`true/false`).
- Required-fields до этого были hardcoded/не backend-driven.
- На search для production SAP нужно полагаться на `SmartFilterBar/SmartTable` как primary flow.
- Нужны четкие бизнес-правила по barrier visibility для low LPC (lpc000/lpc001).

## 4) UI/UX audit for object page

Что хорошо:
- Карточки и sticky control rail ускоряют работу оператора.
- Lock/unsaved workflows в целом консистентны.

Что улучшить дальше:
- Добавить явный блок workflow actions (Validate / Change status / Save) с подсказками.
- Добавить audit trail (кто/когда сменил статус).
- Сделать progress индикатор валидации обязательных полей по секциям.

## 5) Target architecture plan (SAP backend)

### Phase A (short-term)
1. Backend function imports:
   - `ChangeChecklistStatus(id,status)`
   - `ValidateChecklist(id)`
2. Backend config payload at app start:
   - `requiredFields[]`
   - `timers{ heartbeatMs, lockStatusMs, cacheValidMs }`
   - feature flags for UI.
3. Dictionary quality gates:
   - LPC `lpc000..lpc009`
   - PROF `prof000..prof011`

### Phase B (mid-term)
1. Перевести search на full Smart metadata-driven columns/filters + variants transport.
2. Вынести object workflow в domain service (state machine):
   - `Draft -> Registered -> Closed`
   - guards + side effects.
3. Добавить optimistic version field в payload и диагностику 409.

### Phase C (enterprise-hardening)
1. ABAP CDS + OData annotations для Smart controls.
2. KPI мониторинг: save latency, lock conflicts, validation failures.
3. Test strategy:
   - unit for domain validators,
   - OPA5 e2e для lock/status/unsaved flows,
   - contract tests adapter<->backend.

## 6) Recommendations

- Разделить lifecycle статус и overall result в UI всегда как отдельные сущности.
- Оставить frontend fallback, но в productive mode включать strict smart-only.
- Добавить centralized i18n domain map для status text/state.

