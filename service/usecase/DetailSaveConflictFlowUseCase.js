sap.ui.define([
    "sap_ui5/service/usecase/DetailSaveConflictUseCase"
], function (DetailSaveConflictUseCase) {
    "use strict";

    function createConflictActions(mArgs) {
        return {
            reloadLabel: mArgs.reloadLabel,
            overwriteLabel: mArgs.overwriteLabel,
            onReload: mArgs.onReload,
            onOverwrite: mArgs.onOverwrite
        };
    }

    function buildConflictHandler(mArgs) {
        var mActions = createConflictActions(mArgs);
        return function (sChoice) {
            return DetailSaveConflictUseCase.handleConflictChoice(sChoice, mActions);
        };
    }

    return {
        createConflictActions: createConflictActions,
        buildConflictHandler: buildConflictHandler
    };
});
