# QA Test Cases

Last updated: 2026-03-04

Run every case in both themes: `Morning` and `Night`.

## TC-01 App Startup

1. Start backend and UI with `scripts/start-local-env.ps1`.
2. Open `http://127.0.0.1:8080/index.html`.
3. Verify the shell header shows brand, route chip, layout chip, mode chip, global actions, and user entry.
4. Verify desktop shell utility actions show visible labels and collapse safely at narrow widths.
5. Verify shell actions open notifications, help, settings, and user popovers from the header.
6. Verify search route appears in single-pane mode.
7. Verify theme switch changes shell density and control emphasis, not just colors.
8. Verify segmented quick filters have one shared contour, no inner borders, and semantic fills for `All`, `Failed`, and `Success`.

## TC-02 Search Analytics

1. Open the home route.
2. Verify KPI cards render.
3. Open workflow analytics.
4. Verify dialog title, KPI summary, charts, and close action.
5. Close the dialog and verify focus returns to the trigger.

## TC-03 Create Checklist

1. Open the home route.
2. Press `Create`.
3. Verify splitter enters split mode.
4. Verify create draft banner, sticky control rail, and empty states.
5. Verify attachment area behavior matches save-before-upload rules.

## TC-04 Open Existing Checklist

1. Navigate to an existing checklist from search or by route hash.
2. Verify hero card, workflow chips, info cards, checks, barriers, and attachments.
3. Verify close action returns to search-only shell.

## TC-05 Edit and Lock

1. Open an existing checklist.
2. Toggle edit mode on.
3. Verify lock state, autosave state, and action availability update.
4. Edit one field and verify dirty state appears.
5. Verify `Delete` is available only in edit mode for persisted checklists and is absent from the search toolbar.
6. Save and verify states return to synced.

## TC-06 Checks and Barriers Editors

1. Open an editable checklist.
2. Add a check row and change result/comment.
3. Expand checks dialog and verify row editing works.
4. Repeat for barriers when LPC enables that section.
5. On narrow viewport, verify mobile table fallback replaces desktop grid.
6. Verify row actions show visible text labels and remain keyboard-focusable in both inline tables and expanded dialogs.

## TC-07 Location Value Help

1. Open an editable checklist.
2. Trigger location value help.
3. Search in the dialog.
4. Select a node and confirm.
5. Verify selection returns to the originating field.
6. Type quickly into search and verify the tree table remains rendered, selection clears cleanly, and the dialog does not jitter.

## TC-08 Attachments

1. Open an existing checklist in edit mode.
2. Upload a file.
3. Verify it appears in the attachment table.
4. Open/download the file.
5. Delete the file and verify removal.

## TC-09 Responsive Shell

1. Test at desktop width, tablet width, and narrow mobile width.
2. Verify shell header, split layout, dialogs, tables, and sticky rail behavior.
3. Verify sticky detail controls pin below the shell header instead of colliding with it.
4. Verify the Search page can scroll vertically when filter and table content exceed the viewport.
5. Verify no clipped actions, overlapping rails, or hidden critical controls.

## TC-10 Accessibility Basics

1. Navigate shell, search actions, detail actions, and row-level action buttons by keyboard only.
2. Verify visible focus on icon-only and text buttons.
3. Open and close dialogs with keyboard.
4. Verify focus trap and focus return.
5. Verify readable contrast on Morning and Night.
6. Verify row-level action buttons expose visible labels or explicit accessible names instead of hover-only meaning.
7. Verify location value help focuses the search field on open.
8. Verify checks and barriers expanded dialogs return focus to the originating `Expand table` button on close.
9. Verify the detail edit switch enters and exits edit mode from keyboard with `Space` and `Enter`.
