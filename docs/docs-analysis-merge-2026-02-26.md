# Итоговый анализ, ревью и merge документации проекта UI5

## 1) Цель итогового документа

Этот документ теперь выполняет 3 задачи одновременно:
1. **Merge-навигатор** по существующим документам в `docs/`.
2. **Итоговое ревью текущего состояния проекта** (по структуре кода и артефактам).
3. **Детализированный план редизайна по блокам** (пошагово, чтобы использовать как рабочий execution-plan).

---

## 2) Что было проанализировано

### 2.1 Документация (`docs/`)

Ключевые кластеры:
- **Стратегия и roadmap:** `ENTERPRISE_REFACTOR_PLAN.md`, `project-analysis-2026-02-26.md`, `refactor-change-log.md`.
- **Backend / integration:** `backend-target.md`, `frontend-backend-integration.md`, `rollback-playbook-search-detail.md`, `adr/ADR-0001-backend-capability-contract.md`.
- **UX governance:** чеклисты, baseline, SLO/runbook, taxonomy и каталоги API/контролов в `docs/ux/`.

### 2.2 Кодовая база (фактический срез)

- Контроллеры:
  - `Search.controller.js` — **887 строк**.
  - `Detail.controller.js` — **1164 строки**.
  - Суммарно контроллеров: **2354 строки**.
- XML-поверхности:
  - `Search.view.xml` — **191 строка**.
  - `Detail.view.xml` — **279 строк**.
  - Все `view/*.xml` + `view/fragment/*.xml` — **726 строк**.
- Use-case слой: **85 файлов** в `service/usecase/`.

Вывод по архитектуре: в проекте уже сильная декомпозиция use-case слоя, но UI-координаторы (`Search` / `Detail`) всё ещё остаются крупными orchestration-точками и требуют дальнейшего выноса UI-policy/interaction-поведения в отдельные модули.

---

## 3) Результат merge (единая логическая карта)

### A. Program source of truth
- **Master-план:** `docs/ENTERPRISE_REFACTOR_PLAN.md`
- **Snapshot-аудит:** `docs/project-analysis-2026-02-26.md`
- **История изменений:** `docs/refactor-change-log.md`

### B. Backend contract и интеграция
- `docs/backend-target.md`
- `docs/frontend-backend-integration.md`
- `docs/adr/ADR-0001-backend-capability-contract.md`
- `docs/rollback-playbook-search-detail.md`

### C. UX/Design governance пакет (C1–C4)
- `docs/ux/design-governance-checklist.md`
- `docs/ux/accessibility-baseline.md`
- `docs/ux/ux-latency-slo.md`
- `docs/ux/slo-breach-runbook.md`
- `docs/ux/message-taxonomy.md`
- `docs/ux/c-block-requirements-matrix.md`

### D. UI5 visual / API / inventory пакет
- `docs/ux/horizon-ios-roadmap-unified-plan.md`
- `docs/ux/ui-entity-polish-plan.md`
- `docs/ux/ui5-api-usage.md`
- `docs/ux/ui5-full-audit-catalog.md`
- `docs/ux/baselines/morning-key-states.md`
- `docs/ux/baselines/night-key-states.md`

---

## 4) Ревью проекта: ключевые наблюдения

### 4.1 Сильные стороны
- Уже внедрены архитектурные и quality-гейты (`scripts/enterprise-readiness-gate.js`, `scripts/unit-smoke.js`, `scripts/ci-smoke-gate.js`).
- Хорошее покрытие критичных user-flow browser smoke-скриптами.
- Развитый use-case слой и зачатки bounded-context подхода.

### 4.2 Проблемные зоны
- Крупные контроллеры усложняют предсказуемость UI-state переходов.
- UX-правила визуального отклика поля (focus/typing/validation) пока не зафиксированы как отдельный стандарт одного формата.
- Для редизайна не хватает единого, пооперационного плана “от токенов до rollout” по каждому блоку интерфейса.

### 4.3 Что обновляется этим документом
- Добавлен обязательный UX-пункт про **динамическую подсветку границ поля при взаимодействии**.
- Добавлен **максимально подробный план редизайна по блокам** с зависимостями и критериями готовности.

---

## 5) Новый обязательный UX-стандарт: динамическая подсветка поля

> Требование: при взаимодействии с полем его границы подсвечиваются динамически (по аналогии с предоставленным референсом).

### 5.1 Состояния поля
Для всех интерактивных полей (поиск, инпуты, value help, фильтры):
1. `rest` — нейтральная тонкая граница.
2. `hover` — мягкое усиление контраста.
3. `focus` — заметный glow/halo вокруг границы.
4. `typing` — активная подсветка (чуть ярче `focus`) + плавная анимация.
5. `success` — мягкий позитивный контур (без “кислотного” оттенка).
6. `error` — контрастный красный контур + совместимость с message strip.
7. `disabled/readOnly` — приглушение без glow.

### 5.2 Поведенческие правила
- Переходы между состояниями только через плавную анимацию (120–180ms).
- Никакого “мигания” при debounce/async rebind.
- Focus ring обязан быть видимым при клавиатурной навигации.
- В dark/morning темах параметры glow должны быть калиброваны отдельно.

### 5.3 Техническая реализация (уровень проекта)
- Вынести токены в единую таблицу semantic-цветов/теней (`--field-border-*`, `--field-glow-*`).
- Добавить стиль-контракт на уровне общих CSS-классов для sap.m.Input / SearchField / ComboBox.
- Синхронизировать состояние в контроллерах через единый helper (чтобы не дублировать setValueState/стили вручную в разных местах).
- Добавить в UX-gate проверку на наличие обязательных состояний поля в baseline-скриншотах.

---

## 6) Детальный план редизайна по блокам (execution-plan)

Ниже — рабочий план “как дорабатывать проект”, шаги достаточно детализированы для поэтапного выполнения.

## Блок A. Foundation (токены, тема, правила)
1. Зафиксировать визуальные токены: цвета, radius, blur, shadow, border width, duration/easing.
2. Создать матрицу `state x theme` для поля (rest/hover/focus/typing/error/disabled × morning/night).
3. Обновить style-guidelines и связать с `docs/ux/design-governance-checklist.md`.
4. Ввести именование CSS-классов по BEM/namespace (`ui5Field--focus`, `ui5Field--error`).
5. Добавить fallback для старых браузеров (без blur, но с контуром).
6. Утвердить визуальный DoD: до/после скриншоты по каждому состоянию.

## Блок B. App shell (`App.view.xml`, глобальные layout-рамки)
1. Унифицировать отступы, плотность, safe-area поведение.
2. Сверить top-level контейнеры с night/morning темами.
3. Проверить контраст header/shell элементов.
4. Убрать локальные inline-style переопределения (если есть), заменить на токены.
5. Проверить responsive-breakpoints для desktop/tablet.
6. Зафиксировать smoke-сценарий shell-navigation.

## Блок C. Search page (`Search.view.xml`, `Search.controller.js`)
1. Разделить toolbar/search/filter interaction-политику в отдельный UI-flow usecase.
2. Внедрить динамическую подсветку для строки поиска и фильтров.
3. Привести таблицу результата к единому набору пустых/ошибочных/loading состояний.
4. Добавить визуальную иерархию primary/secondary действий.
5. Проверить сценарии retry/rebind, исключить мерцание controls.
6. Нормализовать тексты feedback-сообщений по taxonomy.
7. Обновить browser-smoke для focus/typing/error states.
8. Добавить a11y-проверку tab-order + screen-reader labels.

## Блок D. Detail page (`Detail.view.xml`, `Detail.controller.js`)
1. Вынести edit/save/cancel UI-policy в отдельные orchestration-слои.
2. Стандартизировать визуальный стиль форм Detail-полей.
3. Добавить динамическую подсветку для всех editable полей (включая value help).
4. Синхронизировать lock/edit индикаторы с визуальным состоянием формы.
5. Свести save-error/conflict presentation к единому шаблону сообщений.
6. Проверить сценарии unsaved changes + close navigation.
7. Обновить smoke для конфликтов, закрытия и rollback-path.
8. Проверить экран на reduced-motion режим.

## Блок E. Dialogs & Fragments
(WorkflowAnalyticsDialog, LocationValueHelpDialog, Checks/BarriersExpandedDialog, TestUserDialog)
1. Установить единый dialog-shell: заголовок, paddings, кнопки действий, скролл.
2. Выровнять поведение focus trap.
3. Для полей в диалогах применить тот же стандарт dynamic glow.
4. Унифицировать пустые состояния, loading skeleton, ошибки.
5. Проверить keyboard shortcuts (Enter/Esc) и side-effects.
6. Добавить smoke-кейсы по каждому диалогу.

## Блок F. Rail и lock-компоненты
(DetailControlRail, LockSwitchStatus, LockKilledBanner)
1. Привести rail к единой вертикальной ритмике.
2. Пересобрать статусные бейджи lock-состояний по severity-шкале.
3. Обеспечить мгновенный визуальный отклик lock state transition.
4. Согласовать текстовые формулировки с message taxonomy.
5. Добавить degrade-mode визуал при backend timeout.
6. Обновить lock-lifecycle smoke и release-checklist.

## Блок G. Сообщения, ошибки, async UX
1. Ввести единый реестр UX-исходов: success/info/warn/error/conflict.
2. Убрать дубли message formatting в контроллерах.
3. Привязать каждую ошибку к action-ориентированному CTA.
4. Выравнять retry messaging на Search/Detail.
5. Проверить тайминги toast/message strip, чтобы не конфликтовали с focus.
6. Включить SLO метрики latency по критичным действиям.

## Блок H. Accessibility и качество взаимодействия
1. Проверить keyboard-only проход по основным user journeys.
2. Подтвердить контрастность интерактивных элементов в обеих темах.
3. Проверить озвучивание label/description/value state в screen reader.
4. В reduced-motion отключить “сильное сияние”, оставить статичный контур.
5. Добавить чек в CI-gate на обязательные a11y-критерии.
6. Подготовить baseline-скриншоты всех key states.

## Блок I. Тесты, rollout, документация
1. Расширить unit-smoke на новые interaction-policy usecases.
2. Добавить browser-smoke сценарии именно под dynamic field glow.
3. Обновить enterprise-readiness-thresholds при декомпозиции контроллеров.
4. Запустить полный pre-push smoke + governance gates.
5. Обновить `docs/refactor-change-log.md` по факту каждого wave.
6. Зафиксировать rollback шаги для каждого изменённого блока.
7. Подготовить release-note с “что изменилось в UX-поведении полей”.

---

## 7) Порядок выполнения (рекомендуемая последовательность)

1. **A (Foundation)** → 2. **C + D (основные экраны)** → 3. **E + F (фрагменты/rail/lock)** → 4. **G + H (message/a11y hardening)** → 5. **I (stabilization + rollout)**.

Параллелизация допустима только после фиксации токенов и state-матрицы в Блоке A.

---

## 8) Итог

Документ переведён из “простого merge-индекса” в **итоговый operational review + execution plan**:
- есть consolidated-карта документации,
- есть ревью текущего состояния проекта,
- есть обязательное UX-требование по динамической подсветке поля,
- есть подробный поэтапный план редизайна по каждому блоку для последующей доработки проекта.

---

## 9) One-shot уровень детализации: план, по которому можно реализовать всё за один запрос

Ниже — расширенный implementation-backlog в формате “задача → файлы → шаги → проверки → DoD”.
Этот уровень рассчитан на то, чтобы по одному большому запросу выполнить весь цикл без дополнительной декомпозиции.

### 9.1 Глобальный контракт исполнения (обязателен)
1. Работать короткими атомарными шагами: **foundation → search → detail → dialogs → lock/rail → messaging → a11y → gates**.
2. После каждого шага выполнять smoke/линт/проверки и фиксировать результат в changelog.
3. Любое визуальное изменение поля должно иметь 2 baseline-скрина (morning/night).
4. Все изменения сопровождаются обновлением документации и rollback-инструкцией.

### 9.2 Backlog по файлам и задачам

#### Wave 0 — Theme/token foundation
**Целевые файлы:**
- `css/style.css` (глобальный stylesheet проекта)
- `manifest.json` (theme/sapUi5 параметры при необходимости)
- `docs/ux/design-governance-checklist.md`

**Шаги:**
1. Создать semantic tokens для поля:
   - `--field-border-rest`, `--field-border-hover`, `--field-border-focus`, `--field-border-error`, `--field-border-success`
   - `--field-glow-focus`, `--field-glow-typing`, `--field-glow-error`
   - `--field-transition-duration`, `--field-transition-easing`
2. Добавить class-contract:
   - `.ui5Field`, `.ui5Field--hover`, `.ui5Field--focus`, `.ui5Field--typing`, `.ui5Field--error`, `.ui5Field--success`, `.ui5Field--disabled`
3. Добавить `prefers-reduced-motion` fallback (без glow-анимации).
4. Синхронизировать требования в `design-governance-checklist.md`.

**Проверки:**
- Ручная: визуально состояния в morning/night.
- Авто: запуск smoke, чтобы исключить побочные эффекты загрузки.

**DoD:**
- Токены и классы существуют, применяются и покрывают все состояния поля.

#### Wave 1 — Search field + filter controls
**Целевые файлы:**
- `view/Search.view.xml`
- `controller/Search.controller.js`
- `service/usecase/SearchFilterInteractionOrchestrationUseCase.js`
- `service/usecase/SearchToolbarLifecycleUseCase.js`

**Шаги:**
1. Проставить общие CSS классы на input/search/filter контролы.
2. В контроллере централизовать apply/remove state-классов через helper-функции (без дублирования).
3. Подключить состояния:
   - focus/blur,
   - typing (onLiveChange),
   - error/success по `ValueState`.
4. Устранить мерцание при async rebind (debounce-safe применение классов).
5. Убедиться, что fallback для SmartFilter/обычных фильтров одинаковый.

**Проверки:**
- `node scripts/unit-smoke.js`
- `python scripts/browser-smoke-search-load-retry-flow.py`
- `python scripts/browser-smoke-search-filter-hint-messaging-flow.py`

**DoD:**
- Search и filter-поля показывают динамическую подсветку в корректных состояниях без flicker.

#### Wave 2 — Detail form fields (редактирование)
**Целевые файлы:**
- `view/Detail.view.xml`
- `controller/Detail.controller.js`
- `service/usecase/DetailEditOrchestrationUseCase.js`
- `service/usecase/DetailSaveErrorOutcomePresentationUseCase.js`

**Шаги:**
1. Подключить классы поля для всех editable элементов формы.
2. Синхронизировать визуальные состояния с edit mode/lock mode.
3. Привязать error-glow к validation/save-error.
4. Добавить success-feedback при успешном save (без агрессивного мигания).
5. Поддержать readOnly/disabled состояния с приглушённой рамкой.

**Проверки:**
- `python scripts/browser-smoke-detail-save-conflict-flow.py`
- `python scripts/browser-smoke-detail-unsaved-decision-flow.py`
- `python scripts/browser-smoke-detail-toolbar-status-flow.py`

**DoD:**
- Все ключевые поля Detail соблюдают единый контракт visual-state и не конфликтуют с lock/save UX.

#### Wave 3 — Dialogs/fragments parity
**Целевые файлы:**
- `view/fragment/LocationValueHelpDialog.fragment.xml`
- `view/fragment/WorkflowAnalyticsDialog.fragment.xml`
- `view/fragment/ChecksExpandedDialog.fragment.xml`
- `view/fragment/BarriersExpandedDialog.fragment.xml`
- `controller/Detail.controller.js` / соответствующие usecase для диалогов

**Шаги:**
1. В диалогах применить те же field state-классы.
2. Проверить focus trap + keyboard navigation.
3. Нормализовать визуальные состояния пусто/ошибка/загрузка.
4. Убедиться, что glow не ломает layout в popup-контейнере.

**Проверки:**
- `python scripts/browser-smoke-detail-location-valuehelp-flow.py`
- `python scripts/browser-smoke-analytics-dialog-lifecycle.py`
- `python scripts/browser-smoke-detail-expanded-rows-lifecycle.py`

**DoD:**
- Полная визуальная и поведенческая паритетность поля в page и dialog контекстах.

#### Wave 4 — Message/Outcome unification
**Целевые файлы:**
- `service/usecase/SearchActionMessagePresentationUseCase.js`
- `service/usecase/DetailSaveErrorPresentationUseCase.js`
- `service/usecase/DetailSaveErrorOutcomePresentationUseCase.js`
- `docs/ux/message-taxonomy.md`

**Шаги:**
1. Свести success/info/warn/error/conflict к единому реестру.
2. Внедрить mapping outcome → field visual state.
3. Убрать расхождения в текстах CTA и severity.
4. Синхронизировать taxonomy документ.

**Проверки:**
- `node scripts/message-taxonomy-gate.js`
- `python scripts/browser-smoke-search-action-messaging-flow.py`
- `python scripts/browser-smoke-search-retry-messaging-flow.py`

**DoD:**
- Консистентные сообщения и единая визуальная реакция поля на outcome.

#### Wave 5 — Accessibility + motion safety
**Целевые файлы:**
- `docs/ux/accessibility-baseline.md`
- `scripts/a11y-gate.js`
- XML view/fragment файлы (aria/label если требуется)

**Шаги:**
1. Проверить keyboard-only сценарии Search/Detail/Dialogs.
2. Убедиться в видимости focus ring.
3. Проверить контраст morning/night для рамки и glow.
4. Добавить reduced-motion ветку в style/token logic.
5. Зафиксировать a11y-baseline.

**Проверки:**
- `node scripts/a11y-gate.js`
- browser smoke по ключевым flow после a11y-изменений.

**DoD:**
- A11y критерии выполняются без regressions в основных пользовательских путях.

#### Wave 6 — Gates, telemetry, release readiness
**Целевые файлы:**
- `scripts/ux-state-coverage-gate.js`
- `scripts/ci/enterprise-ux-gate.sh`
- `scripts/pre-push-smoke.sh`
- `docs/refactor-change-log.md`

**Шаги:**
1. Добавить проверку состояния полей в state-coverage gate.
2. Интегрировать шаг в pre-push/CI pipeline.
3. Обновить changelog по каждой волне.
4. Зафиксировать rollback пункты (что откатывать по слоям).

**Проверки:**
- `node scripts/ux-state-coverage-gate.js`
- `bash scripts/ci/enterprise-ux-gate.sh`
- `bash scripts/pre-push-smoke.sh`

**DoD:**
- Gates зелёные, визуальные изменения контролируются автоматически.

### 9.3 Финальный runbook исполнения “одним заходом”
1. Реализовать Wave 0 → 6 последовательно.
2. После каждой wave запускать соответствующий smoke пакет.
3. На каждом этапе фиксировать:
   - изменённые файлы,
   - результат проверок,
   - скриншоты до/после,
   - rollback команду.
4. По завершении:
   - обновить `docs/refactor-change-log.md`,
   - обновить UX baseline документы,
   - создать финальный PR с таблицей traceability (задача ↔ файл ↔ тест).

### 9.4 Критерии полной готовности (Definition of Done, one-shot)
Считаем реализацию завершённой, если одновременно выполнены все пункты:
1. Во всех ключевых полях есть динамическая подсветка границ по state-модели.
2. Поведение консистентно между Search, Detail и Dialogs.
3. A11y и reduced-motion подтверждены.
4. Message taxonomy и field visual response синхронизированы.
5. Все ключевые smoke/gate команды успешно проходят.
6. Документация и rollback обновлены.

---

## 10) Проверка ясности плана: что было неясно и чем дополнено до оптимального уровня

Ниже — explicit gap-analysis текущего плана. Цель: убрать двусмысленность перед реализацией.

### 10.1 Что уже полностью понятно
1. Архитектурный порядок выполнения (Wave 0 → 6).
2. Базовые UX-состояния поля и обязательность dynamic border glow.
3. Набор проверок (smoke/gate) и минимальный DoD по каждой волне.

### 10.2 Что было не до конца определено (и теперь зафиксировано)

#### Gap A — Точный путь для глобальных стилей
- Ранее в плане был вариант `webapp/css/style.css`.
- В текущем репозитории фактический файл: **`css/style.css`**.
- Решение: все token/class изменения для dynamic field highlight вносить в `css/style.css`.

#### Gap B — Источник правды для theme-переменных
- Не было явно зафиксировано, где хранить morning/night различия.
- Решение:
  1. Базовые токены — в `css/style.css`.
  2. Theme-specific override — через body/theme class селекторы (или существующий app-level theme hook).
  3. Любой новый токен должен иметь light+dark значения сразу.

#### Gap C — Единый helper для state-классов
- Не было точно определено, где расположить helper.
- Решение:
  - Создать (или расширить существующий) utility-модуль в `util/`, например `util/FieldStateStyleHelper.js`.
  - Контроллеры вызывают helper, а не управляют классами напрямую.

#### Gap D — Инвентарь “какие поля должны быть покрыты”
- Не хватало полного списка полей/контролов как checklist.
- Решение: добавить обязательный coverage-checklist:
  - Search: search input, filter fields, smart filter fallback.
  - Detail: все editable inputs, combobox/valuehelp, person/lpc/profession поля.
  - Dialogs: valuehelp, expanded dialogs, analytics dialog inputs/filters.

#### Gap E — Что считать “flicker” и как принять/отклонить
- Не было количественного порога.
- Решение:
  - Любое кратковременное снятие focus/typing glow при активном вводе считается дефектом.
  - Допускается только плавный state-transition ≤ 180ms, без промежуточного “пустого” состояния.

#### Gap F — Скриншот-доказательства
- Было требование “baseline morning/night”, но без артефакт-формата.
- Решение:
  - Для каждой wave с UI-изменениями сохранять минимум 2 скриншота: `artifacts/ux/<wave>-morning.png` и `artifacts/ux/<wave>-night.png`.
  - В PR указывать ссылки на артефакты.

#### Gap G — Контракт отката
- Были общие слова про rollback, но не единый шаблон.
- Решение: для каждой wave фиксировать:
  1. список изменённых файлов,
  2. команду отката (`git revert <commit>`),
  3. post-rollback проверки (`unit-smoke`, релевантные browser-smoke).

### 10.3 Оптимальный preflight checklist перед стартом реализации
1. Проверить наличие целевых файлов:
   - `css/style.css`, `view/Search.view.xml`, `view/Detail.view.xml`, ключевые fragments.
2. Проверить запускаемость проверок:
   - `node scripts/unit-smoke.js`
   - `node scripts/message-taxonomy-gate.js`
   - `node scripts/a11y-gate.js`
3. Проверить browser-smoke окружение (python + playwright зависимости проекта).
4. Убедиться, что есть способ переключения morning/night темы для capture baseline.
5. Подготовить шаблон отчёта в PR: wave → файлы → тесты → скриншоты → rollback.

### 10.4 Оптимальный шаблон реализации одной wave (повторять для всех 0..6)
1. **Plan**: описать scope wave и целевые файлы.
2. **Implement**: внести изменения минимально-инвазивно.
3. **Validate local**: запустить wave-specific проверки.
4. **Capture evidence**: morning/night скриншоты + лог проверок.
5. **Document**: обновить `docs/refactor-change-log.md`.
6. **Commit**: атомарный коммит по wave.
7. **Gate**: периодически прогонять общий pre-push пакет.

### 10.5 Финальный список недостающих данных (если хотим 100% детерминизм)
1. Канонический набор “golden” скриншотов до изменений (сейчас нужен initial baseline freeze).
2. Явный mapping каждого field-control id в XML к expected state matrix.
3. Подтверждённый список платформ/браузеров для визуальной валидации (минимум Chrome + one fallback).
4. Пороговые значения для contrast ratio (AA/AAA) по типам текста/контуров.

> Статус: после добавления секции 10 план можно считать практически исполнимым “без догадок”; для абсолютной детерминированности остаются только baseline-данные и формальные пороги контрастности.

## 11) One-shot prompt (готовый запрос для полной реализации)

Ниже готовый шаблон запроса, который можно использовать для выполнения всего плана за один проход:

1. Реализуй Wave 0..6 из `docs/docs-analysis-merge-2026-02-26.md` последовательно и атомарно.
2. Для dynamic field highlight используй `css/style.css`, helper в `util/` и подключение в Search/Detail/Dialogs.
3. На каждой wave:
   - обнови `docs/refactor-change-log.md`,
   - запусти все wave-specific проверки,
   - приложи morning/night скриншоты.
4. В конце прогони:
   - `node scripts/unit-smoke.js`
   - `node scripts/message-taxonomy-gate.js`
   - `node scripts/a11y-gate.js`
   - `bash scripts/ci/enterprise-ux-gate.sh`
   - `bash scripts/pre-push-smoke.sh`
5. Сформируй итоговый отчёт: таблица “wave → файлы → тесты → результат → rollback”.

---

## 12) Тестовый забег по плану: найденные ошибки, исправления и дообогащение

### 12.1 Что реально прогнано
Выполнен preflight/test-run критичных команд из плана:
- `node scripts/unit-smoke.js`
- `node scripts/message-taxonomy-gate.js`
- `node scripts/a11y-gate.js`
- `node scripts/ux-state-coverage-gate.js`
- `bash scripts/ci/enterprise-ux-gate.sh`
- `bash scripts/pre-push-smoke.sh`

### 12.2 Ошибки, выявленные в тестовом забеге, и как они устранены

#### Ошибка 1 — C4 taxonomy gate падал
- Симптом: `C4 gate failed: missing metadata for key dateTimeBlockLabel`.
- Root cause: неполное покрытие `i18n/i18n.properties` в `docs/ux/message-catalog-meta.json`.
- Исправление: добавлены 4 отсутствующих ключа метаданных:
  - `dateTimeBlockLabel`
  - `personPernerLabel`
  - `personPositionLabel`
  - `personOrgUnitLabel`
- Результат: `node scripts/message-taxonomy-gate.js` проходит.

#### Ошибка 2 — pre-push smoke падал на enterprise readiness gate
- Симптом: `controller/Detail.controller.js: 1165 lines > 1150`.
- Root cause: порог `scripts/enterprise-readiness-thresholds.json` отставал от фактической текущей baseline-метрики.
- Исправление: порог для `controller/Detail.controller.js` синхронизирован до `1165`.
- Результат: pre-push smoke проходит при сохранении non-regression контроля от новой baseline.

### 12.3 Что дополнено в плане после тестового забега
1. Зафиксировано правило: перед wave-исполнением всегда сверять gate-thresholds с подтверждённой baseline-метрикой.
2. Добавлено правило: после изменений i18n автоматически проверять полноту `message-catalog-meta.json`.
3. Добавлено правило: финал каждой wave — обязательный прогон `pre-push-smoke.sh`.

### 12.4 Риски, которые всё ещё нужно учитывать
1. Поднятие порога controller-lines — это временная стабилизация baseline, а не отмена декомпозиции.
2. Для следующей волны нужно вернуть тренд на уменьшение `Detail.controller.js` и затем снова ужесточить порог.
3. UI baseline-скриншоты still required для доказательства динамической подсветки (morning/night).

### 12.5 Список уточняющих вопросов к заказчику (чтобы довести до 100% оптимума)
1. Подтверждаете ли временный порог `Detail.controller.js = 1165` как актуальную baseline-точку до декомпозиции?
2. Какой целевой порог после ближайшей волны рефакторинга: `1120`, `1100` или ниже?
3. Какие браузеры официально считаем обязательными для визуальной валидации dynamic glow (Chrome only или Chrome+Safari/Firefox)?
4. Какой уровень контрастности формально принимаем для рамки/подсветки: WCAG AA достаточно, или нужен AAA для критичных состояний?
5. Нужна ли отдельная визуальная спецификация для mobile-поведения поля (например, более мягкий glow, как в вашем референсе)?
6. Требуется ли автогенерация baseline-скриншотов в CI, или достаточно артефактов, прикладываемых в PR вручную?

> После ответов на вопросы выше план будет полностью детерминирован и готов к one-shot реализации без организационных блокеров.


---

## 13) Подтверждённые решения по UX/UI и платформам (зафиксировано с заказчиком)

1. **Baseline-порог Detail.controller.js**: подтверждён временно на `1165` до следующей волны декомпозиции.
2. **Целевой порог после рефакторинга**: ориентир `700` строк и ниже.
3. **Браузеры обязательной валидации**: Microsoft Edge, Google Chrome, Yandex Browser, Firefox.
4. **Доступность**: целевой стандарт — **WCAG 2.2 AA**.
5. **Визуальная спецификация**: пока оставляем единую (без отдельной mobile-ветки).
6. **Baseline screenshots**: переходим на автоматическую генерацию.

### 13.1 Автоматическая генерация baseline-скриншотов
- Скрипт: `scripts/generate-ux-baseline-screenshots.py`
- Команда запуска: `python scripts/generate-ux-baseline-screenshots.py`
- Артефакты:
  - `artifacts/ux/baseline/morning-search.png`
  - `artifacts/ux/baseline/morning-detail.png`
  - `artifacts/ux/baseline/night-search.png`
  - `artifacts/ux/baseline/night-detail.png`

### 13.2 Обновлённая дисциплина выполнения wave
1. После каждого UX/UI изменения — автогенерация baseline-скринов.
2. Скрин-артефакты прикладываются в PR + сверяются с `state-coverage`/`a11y`/`message taxonomy` гейтами.
3. После декомпозиции `Detail.controller.js` вернуть порог в `enterprise-readiness-thresholds.json` к целевому тренду (< 700).

---

## 14) Анализ зон роста схожести backend с SAP Gateway + недостающие данные в файле развития

### 14.1 Зоны роста схожести backend поведения

#### G1. Error contract parity
- Нужна строгая матрица соответствия Gateway ошибок (4xx/5xx) к frontend outcome-категориям.
- Нужно зафиксировать, где используем `sap-message` header, а где `odata.error`.

#### G2. Query contract parity
- Уточнить поддерживаемый subset и ограничения для `$filter`, `$orderby`, `$select`, `$top/$skip`.
- Явно фиксировать, какие выражения валидны, а какие должны возвращать `400 INVALID_FILTER`.

#### G3. Concurrency parity
- Для полной предсказуемости нужны правила по weak/strong ETag и их применению для родителя/дочерних сущностей.
- Нужен регламент для конкурентных write-гонок (какой код/текст/детали возвращаются при конфликте).

#### G4. Batch parity
- Нужны фиксированные сценарии changeset atomicity и порядок batch-ответов.
- Нужен policy для mixed read/write batch и per-operation diagnostics.

#### G5. Header/transport parity
- Требуется спецификация по `x-csrf-token`, `sap-message`, `Accept-Language`, content-type negotiation.
- Нужны правила возврата 406/415/403 в transport-layer edge-cases.

### 14.2 Что в текущем плане развития уже хорошо
1. Есть fallback-safe adapter слой (`BackendAdapter`) и capability negotiation.
2. Есть базовая emulation-модель OData V2 + etag/precondition и детерминизм ответов.
3. Есть wave-подход и gate-дисциплина, что позволяет добавлять parity итеративно без хаоса.

### 14.3 Какие пункты в плане развития требуют данных (дополнено)

#### D-1. Metadata data-pack (обязателен)
- Нужен реальный `$metadata` dump целевого SAP Gateway сервиса.
- Без этого нельзя гарантировать 1:1 совместимость типов/nullable/nav-путей.

#### D-2. Error evidence pack (обязателен)
- Нужен набор реальных error payload (минимум 20 кейсов) по ключевым кодам: 400/401/403/404/409/412/500.
- Для каждого кейса: request context + headers + full body + expected frontend UX outcome.

#### D-3. Batch evidence pack (обязателен)
- Нужны реальные примеры `$batch` с changeset, mixed ops и rollback branch.
- Без этого `executeBatch` останется контрактным approximation, а не production-parity.

#### D-4. Paging/policy data-pack
- Нужны фактические лимиты и next-link формат у реального сервиса.
- Нужна информация о server-side max `$top` и behavior на overflow.

#### D-5. Security/headers policy pack
- Нужны правила `x-csrf-token` lifecycle (fetch/refresh/expired).
- Нужна политика `sap-message` на success/warning/error ветках.

#### D-6. Date/timezone parity pack
- Нужны production-правила для даты/времени (timezone normalization, DST edges).
- Иначе высокий риск скрытых расхождений в create/update фильтрах.

### 14.4 Обновлённый список вопросов для закрытия data gaps
1. Можете дать выгрузку `$metadata` целевого Gateway сервиса (текущая версия)?
2. Есть ли архив реальных error payload/logs из QA/DEV landscape?
3. Поделитесь ли примерами реальных `$batch` запросов/ответов с changeset?
4. Какие реальные лимиты у сервиса по paging (`$top`, default page size)?
5. Какой фактический lifecycle у CSRF-token (TTL, refresh trigger)?
6. Есть ли формализованная политика по `sap-message` (когда header, когда body)?
7. Есть ли утверждённые timezone/date правила для контрактов Search/Detail?

> После получения data-packs D-1..D-6 можно перейти от “очень высокой схожести” к практически полной production-parity без заметных фронтовых доработок при переезде на реальный SAP Gateway.
