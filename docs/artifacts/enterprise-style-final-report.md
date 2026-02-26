# Enterprise Style Final Report (Premium Air / Glass V2)

## Что улучшено
- Увеличена «воздушность» между ключевыми блоками через единый токен `--premium-air-gap`.
- Унифицировано стекло для всех основных enterprise-поверхностей: `glassCard`, `filterCard`, `smartTableGlass`, `workflowStageCard`, `kpiCard`.
- Стабилизированы параметры прозрачности и бордера через токены `--premium-glass-alpha` и `--premium-glass-border-alpha`.
- Усилена визуальная глубина (контраст теней) в тёмной теме для читаемости и премиального ощущения.
- Выравнены вертикальные отступы тулбаров/мета-рейла и статусов для более спокойной визуальной ритмики.
- Улучшен визуальный контейнер скролл-областей таблиц (`tableScrollWrap`) — мягче фон/граница.

## Что убрано в предыдущем проходе и теперь закреплено
- Конфликтные legacy-ветки detail rail (разные модели позиционирования и дубли-правила).
- Взаимоисключающие стиль-пакеты, которые перетирали друг друга и ломали предсказуемость каскада.

## Ожидаемый UX-эффект
- Более «дорогой» визуальный тон (premium enterprise), без перегруза свечением.
- Более плавное восприятие компоновки за счёт воздуха и консистентного glass-layer.
- Меньше визуального шума и регрессий при дальнейших точечных правках.

## Дополнительно (Horizon Morning/Night + macOS/iOS философия)
- CSS переведён в модульный бандл:
  - `css/modules/style-core.css`
  - `css/modules/style-components.css`
  - `css/modules/style-overrides.css`
  - entrypoint: `css/style.css` (через `@import`).
- Добавлен деликатный анимированный фон-градиент `philosophyGradientDrift` с разной прозрачностью для light/dark, чтобы подчеркнуть «живую» философию продукта без визуального шума.

- Gradient works in both theme modes: lower opacity in Morning/Light and slightly richer depth in Night/Dark.
