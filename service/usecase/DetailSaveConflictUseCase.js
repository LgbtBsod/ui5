sap.ui.define([], function () {
    "use strict";

    function shouldReloadChoice(sChoice, sReloadLabel) {
        return sChoice === sReloadLabel;
    }

    function shouldOverwriteChoice(sChoice, sOverwriteLabel) {
        return sChoice === sOverwriteLabel;
    }

    function handleConflictChoice(sChoice, mOptions) {
        var sReloadLabel = mOptions.reloadLabel;
        var sOverwriteLabel = mOptions.overwriteLabel;

        if (shouldReloadChoice(sChoice, sReloadLabel)) {
            return mOptions.onReload();
        }
        if (shouldOverwriteChoice(sChoice, sOverwriteLabel)) {
            return mOptions.onOverwrite();
        }
        return Promise.resolve(null);
    }

    return {
        shouldReloadChoice: shouldReloadChoice,
        shouldOverwriteChoice: shouldOverwriteChoice,
        handleConflictChoice: handleConflictChoice
    };
});
