# Custom Style Prompt Contract

## Как правильно называется этот "блок"
Это лучше называть:
- **Style Prompt Contract** (контракт промта на стиль)
- или **Design Update Brief** (бриф на обновление дизайна)

В проекте используем термин: **Custom Style Prompt Contract**.

---

## Зачем нужен контракт
Чтобы каждое обновление стиля выполнялось одинаково:
- без конфликтов и дублей в `css/style.css`
- с сохранением enterprise/premium визуального языка
- с проверкой, что UI не деградировал

---

## Важно: можно ли "перенести весь CSS в блок промта"
Коротко: **нет, полностью переносить CSS в промт не нужно**.

Правильный подход:
- CSS остаётся **источником истины** в `css/style.css`.
- В промт-контракте описываются:
  - правила,
  - стиль-токены,
  - ожидаемые эффекты,
  - ограничения,
  - чеклист проверки.

То есть блок = **спецификация изменений**, а не замена CSS-файла.

---

## Готовый блок для запроса к Codex
Копируй и используй как шаблон.

```md
# STYLE_UPDATE_CONTRACT

## Scope
- Update only visual system and CSS architecture.
- Keep SmartFilter/SmartTable UX priority.
- Do not re-introduce legacy fallback visual branches.

## Style direction
- Enterprise premium glass
- More air / spacing rhythm
- Higher readability in dark mode
- Controlled transparency (no over-glow)

## Technical requirements
1. Remove CSS duplications and contradictory selectors.
2. Keep one source of truth per component block.
3. Prefer tokenized variables in `:root`.
4. Preserve existing behavior and handlers in XML/JS.
5. If adding new visual rules, group them in a clearly named section.

## Constraints
- No random `!important` spam unless needed for UI5 override.
- No dead selectors for removed markup.
- No fallback table/toolbar comeback unless explicitly requested.

## Deliverables
- Updated `css/style.css`
- Short report in `docs/artifacts/`:
  - what changed
  - what conflicts removed
  - expected visual impact

## Validation
- Run syntax/sanity checks
- Run local visual smoke
- Provide screenshot artifact
```

---

## Мини-чеклист перед каждым стилевым PR
- [ ] Нет ли повторных определений одного и того же блока с разной логикой?
- [ ] Нет ли orphan-селекторов под удалённую разметку?
- [ ] Новые токены вынесены в `:root`?
- [ ] Есть ли короткий artifact-report в `docs/artifacts/`?
- [ ] Есть ли скриншот после изменений?

---

## Рекомендация на следующий этап
Если хочешь, можно сделать шаг 2:
- разбить `css/style.css` на модульные файлы (`tokens.css`, `layout.css`, `components.css`, `overrides-ui5.css`) 
- и собирать их в один бандл.

Это даст ещё меньше конфликтов и быстрее итерации по стилю.
