sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/DialogConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/LocationValueHelpConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeTimingConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchUiConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ShellPaneConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailPersistenceConstants"
], function (
    DialogConstants,
    AnalyticsConstants,
    FrontendConfigConstants,
    ModelConstants,
    LocationValueHelpConstants,
    NavigationConstants,
    OperationSourceConstants,
    RuntimeTimingConstants,
    SearchRuntimeConstants,
    SearchUiConstants,
    ShellPaneConstants,
    WorkflowConstants,
    DetailPersistenceConstants
) {
    "use strict";

    return Object.freeze({
        ANALYTICS: AnalyticsConstants,
        DIALOG: DialogConstants,
        FRONTEND_CONFIG: FrontendConfigConstants,
        LOCATION_VALUE_HELP: LocationValueHelpConstants,
        MODEL: ModelConstants,
        NAVIGATION: NavigationConstants,
        OPERATION_SOURCE: OperationSourceConstants,
        RUNTIME_TIMING: RuntimeTimingConstants,
        SEARCH_RUNTIME: SearchRuntimeConstants,
        SEARCH_UI: SearchUiConstants,
        SHELL_PANE: ShellPaneConstants,
        WORKFLOW: WorkflowConstants,
        DETAIL_PERSISTENCE: DetailPersistenceConstants
    });
});
