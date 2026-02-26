sap.ui.define([], function () {
    "use strict";

    return {
        createInfoCards: function (oBundle) {
            return [
                { key: "datetime", title: oBundle.getText("dateTimeBlockLabel") },
                { key: "equipment", title: oBundle.getText("equipmentLabel") },
                { key: "observer", title: oBundle.getText("observerLabel") },
                { key: "observed", title: oBundle.getText("observedLabel") },
                { key: "location", title: oBundle.getText("locationLabel") },
                { key: "lpc", title: oBundle.getText("lpcLabel") },
                { key: "profession", title: oBundle.getText("professionLabel") }
            ];
        },

        createStatusActions: function (oBundle) {
            return [
                { key: "DRAFT", title: oBundle.getText("statusDraft") },
                { key: "REGISTERED", title: oBundle.getText("statusRegistered") },
                { key: "CLOSED", title: oBundle.getText("statusClosed") }
            ];
        }
    };
});
