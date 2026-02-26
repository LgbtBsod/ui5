# Анализ проекта UI5 (2026-02-26)

## 1) Что это за проект
- Это enterprise-приложение на SAP UI5 для управления чек-листами с двухколоночным сценарным UX (Search → Detail) на базе `sap.f.FlexibleColumnLayout` и маршрутов `search` / `detail/{id}`.
- В манифесте явно заданы несколько моделей состояния (`data`, `state`, `layout`, `cache`, `masterData`, `mpl`) и backend-режимы через конфиг `backendMode`.
- В репозитории присутствуют как фронтенд, так и локальный Python backend (`mock_gate_way`) для интеграционного прогона и эмуляции OData-подобных API.

## 2) Архитектурная картина (фактическая)
### Слои
1. **UI слой**: `view/*.view.xml`, `controller/*.controller.js`.
2. **Оркестрация/Use-case слой**: `service/usecase/*.js` (высокая декомпозиция по сценариям).
3. **Backend-адаптеры**: `service/backend/*` + `service/SmartSearchAdapter.js`.
4. **Доменные/утилитарные сервисы**: `domain`, `util`, `manager`.
5. **Тестово-операционный слой**: `scripts/*` (smoke/gates/reports) + документация в `docs/*`.

### Наблюдения по модульности
- Сильная сторона: бизнес-сценарии вынесены в use-case классы/модули (85 файлов), что снижает связность UI-кода с backend деталями.
- Ограничение: контроллеры остаются «толстыми» (`Search.controller.js` и `Detail.controller.js`), что увеличивает стоимость изменений и риск регрессий.
- Архитектурные метрики и gate уже заведены и реально исполняются локально.

## 3) Количественные метрики (срез)
- Use-case модулей: **85**.
- Util модулей: **13**.
- Крупнейшие контроллеры:
  - `controller/Search.controller.js`: **888 строк**.
  - `controller/Detail.controller.js`: **1147 строк**.
- По количеству файлов (основные зоны):
  - `service`: 91
  - `mock_gate_way`: 67
  - `scripts`: 51
  - `docs`: 23

## 4) Качество и инженерные практики
### Что уже хорошо
- Рабочий smoke-gate с покрытием ключевых сценариев (локально проходит 113 тестов).
- Есть enterprise-readiness gate с проверками архитектурного «дрейфа».
- Подготовлена roadmap-документация и чек-листы UX/governance.

### Риски
1. **Сложность сопровождения контроллеров**: высокая когнитивная нагрузка в `Search/Detail.controller.js`.
2. **Технический долг в backlog**: часть enterprise-блоков помечены как planned (security/compliance, bounded contexts, расширение тестовой стратегии).
3. **Риск регрессий при фич-развитии**: при росте объема UI-оркестрации без дальнейшей декомпозиции.

## 5) Приоритетные рекомендации (короткий план)
1. **P1: Декомпозиция контроллеров**
   - Дожать A3/A4 из refactor plan: оставить в контроллерах composition root и навигацию, остальное вынести в coordinator/presenter/use-case прослойки.
2. **P1: Формализация bounded contexts**
   - Ввести явные `domain/search`, `domain/detail`, `domain/lock`, `domain/analytics` + правила импортов.
3. **P1: Security блок D1-D3**
   - Threat model для lock/save/export; CSP/headers стратегия; зависимость+license аудит в CI.
4. **P2: Тестовая эволюция**
   - Добавить unit/contract coverage по top-risk веткам (save conflict, retry, lock kill) и mutation/static gates.
5. **P2: Release/operations**
   - Go/No-Go checklist + runbooks (инциденты lock, деградация backend, mismatch данных).

## 6) Вывод
Проект уже выглядит как зрелая enterprise-основа: есть слой use-case оркестрации, backend abstraction, quality gates и продуктовые UX-документы. Ключевая точка роста — снизить сложность контроллеров и закрыть security/ops блоки из плана, чтобы ускорить поставку без роста регрессионных рисков.
