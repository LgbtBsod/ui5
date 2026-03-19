sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayBackendService",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/CurrentUserProfile"
], function (Result, GatewayBackendService, CurrentUserProfile) {
    "use strict";

    function buildSummary(oProfile) {
        return CurrentUserProfile.permissionKeys(oProfile);
    }

    function readCurrentUser(sLogin) {
        return GatewayBackendService.readEntity("CurrentUserSet", "'CURRENT'", {
            __ts: Date.now()
        }).then(function (oData) {
            return CurrentUserProfile.normalizeCurrentUser(oData, sLogin);
        });
    }

    function readExistingProfile(oStateModel) {
        if (!oStateModel || typeof oStateModel.getProperty !== "function") {
            return null;
        }
        return oStateModel.getProperty("/currentUser") || null;
    }

    return {
        execute: function (mInput, mDeps) {
            var sLogin = String(mInput && mInput.login || "").trim();
            var oStateModel = mDeps && mDeps.stateModel;
            return readCurrentUser(sLogin).then(function (oProfile) {
                oProfile = CurrentUserProfile.applyCurrentUserState(oStateModel, oProfile);
                oProfile.permissionKeys = buildSummary(oProfile);
                return Result.ok({
                    user: oProfile.fullName || "",
                    profile: oProfile
                });
            }).catch(function (oError) {
                var oExistingProfile = readExistingProfile(oStateModel);
                if (!oExistingProfile || !String(oExistingProfile.fullName || "").trim()) {
                    throw oError;
                }
                oExistingProfile = CurrentUserProfile.applyCurrentUserState(oStateModel, oExistingProfile);
                oExistingProfile.permissionKeys = buildSummary(oExistingProfile);
                return Result.ok({
                    user: oExistingProfile.fullName || "",
                    profile: oExistingProfile,
                    stale: true
                });
            });
        },
        refresh: function (mDeps) {
            var oStateModel = mDeps && mDeps.stateModel;
            return this.execute({ login: "" }, mDeps);
        }
    };
});
