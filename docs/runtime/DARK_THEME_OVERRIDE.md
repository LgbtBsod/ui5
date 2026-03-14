# Dark Theme Override

The productive default remains the current light theme.

For development and QA only, the application now supports a controlled dark-mode override without re-enabling the theme switch in the UI.

## Enable Dark Override

Open the app with:

```text
?themeOverride=dark
```

This stores `night` under `checklist_app_theme_dev_override` in local storage.

## Force Light Override

Open the app with:

```text
?themeOverride=light
```

## Clear Override

Open the app with:

```text
?themeOverride=clear
```

## Policy

- This override is for local development and QA only.
- It does not change the productive default.
- It does not re-enable the public theme toggle in the shell.
