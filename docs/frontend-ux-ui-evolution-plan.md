# Frontend evolution plan (combined from architecture + ТЗ audit)

## Цель
Свести воедино предыдущие аналитики и ТЗ-аудит в практический фронтенд-план: сначала закрыть UX/flow-риски, затем перейти к системным улучшениям UI5-компонентов и фрагментации.

## Приоритеты развития

### P0 — консистентность UX состояния
- Единый busy-path: использовать `state>/isLoading` во всех пользовательских операциях.
- Явные loading-переходы на Search/Detail/Object экранах.

### P1 — переиспользование UI через фрагменты
- Вынос повторяющихся lock-элементов в фрагменты:
  - banner о `is_killed`;
  - switch + pending indicator + status.
- Единая точка UX-изменений для lock-state поведения.

### P2 — search UX hardening
- Переход к более enterprise-подходу:
  - либо внедрить `SmartFilterBar/SmartTable`,
  - либо зафиксировать эквивалентный контракт кастомных фильтров с test coverage.

### P3 — flow quality gates
- Добавить smoke-набор по сценариям A–H:
  - read/open,
  - edit/lock,
  - autosave/fullsave,
  - unload/release,
  - steal/session-killed.

---

## UX/UI best practices (для этого проекта)

1. **Один источник визуальной правды по состоянию**
   - Все busy/processing состояния — через `stateModel`, без разнобоя `isBusy/isLoading`.

2. **Прогрессивное раскрытие**
   - В карточке: быстрый first paint (root/basic), затем отдельная ленивaя подгрузка checks/barriers.

3. **Предсказуемый lock UX**
   - Любая lock-операция отображает pending + текстовый статус + итоговый state.
   - При `is_killed` интерфейс должен безусловно переходить в безопасный read-only.

4. **Фрагменты вместо копипаста**
   - Общие UX-элементы (lock/banner/status chips) выносить в XML fragments и переиспользовать.

5. **Минимум скрытой магии**
   - Если поведение критично (GCD reset only on full save), закреплять это и в коде событий, и в UX copy.

---

## Что уже доработано в этой итерации

1. Унифицирован busy-state на `isLoading` для сохранений в Object/Detail.
2. Добавлен busy binding страницы Detail (`state>/isLoading`).
3. Реализованы и подключены переиспользуемые фрагменты:
   - `LockKilledBanner.fragment.xml`
   - `LockSwitchStatus.fragment.xml`
4. Object/Detail переведены на использование этих фрагментов.
5. В Detail добавлен алиас-обработчик `onToggleEdit` для унификации fragment contract.

---

## Следующий шаг (предлагаемый)

1. Сделать строгую pre-edit freshness проверку (before lock acquire).
2. Доработать copy-flow до полного backend-led сценария.
3. Вынести status/error panel Search-экрана в отдельный fragment.
4. Добавить базовые UI smoke-тесты на lock/timer/navigation flows.
