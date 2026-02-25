sap.ui.define([], function () {
    "use strict";

    function setReadUnlocked(oStateModel) {
        oStateModel.setProperty("/mode", "READ");
        oStateModel.setProperty("/isLocked", false);
    }

    function setEditLocked(oStateModel) {
        oStateModel.setProperty("/mode", "EDIT");
        oStateModel.setProperty("/isLocked", true);
    }

    function resetDirty(oStateModel) {
        oStateModel.setProperty("/isDirty", false);
    }

    function prepareCloseNavigation(oStateModel) {
        oStateModel.setProperty("/layout", "OneColumn");
        oStateModel.setProperty("/activeObjectId", null);
        resetDirty(oStateModel);
    }

    function keepEditModeAfterSave(oStateModel) {
        if (oStateModel.getProperty("/isLocked")) {
            oStateModel.setProperty("/mode", "EDIT");
        }
        resetDirty(oStateModel);
    }

    return {
        setReadUnlocked: setReadUnlocked,
        setEditLocked: setEditLocked,
        resetDirty: resetDirty,
        prepareCloseNavigation: prepareCloseNavigation,
        keepEditModeAfterSave: keepEditModeAfterSave
    };
});
