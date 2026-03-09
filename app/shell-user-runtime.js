(function () {
    "use strict";

    var CurrentUserProfile = window.sap && sap.ui && sap.ui.requireSync
        ? sap.ui.requireSync("PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/CurrentUserProfile")
        : null;
    var PermissionPresentation = window.sap && sap.ui && sap.ui.requireSync
        ? sap.ui.requireSync("PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/PermissionPresentation")
        : null;
    var CODE_LABELS = {
        "01": "View",
        "02": "Edit/Create",
        "03": "Delete"
    };
    var REFRESH_MS = 15000;

    function normalizeRule(rule) {
        if (CurrentUserProfile && typeof CurrentUserProfile.normalizePermissionRule === "function") {
            return CurrentUserProfile.normalizePermissionRule(rule);
        }
        return {
            code: String(rule && rule.code || "").trim(),
            scopeKind: String(rule && (rule.scopeKind || rule.scope_kind) || "all").trim().toLowerCase() || "all",
            scopeValue: String(rule && (rule.scopeValue || rule.scope_value) || "ALL").trim() || "ALL"
        };
    }

    function normalizePermissionRules(value) {
        if (CurrentUserProfile && typeof CurrentUserProfile.normalizePermissionRules === "function") {
            return CurrentUserProfile.normalizePermissionRules(value);
        }
        var items = [];
        try {
            items = JSON.parse(String(value || "[]"));
        } catch (_error) {
            items = [];
        }
        if (!Array.isArray(items)) {
            return [];
        }
        return items.map(normalizeRule).filter(function (rule) {
            return !!rule.code;
        });
    }

    function normalizeCurrentUser(payload) {
        if (CurrentUserProfile && typeof CurrentUserProfile.normalizeCurrentUser === "function") {
            return CurrentUserProfile.withFetchedAt(CurrentUserProfile.normalizeCurrentUser(payload, ""));
        }
        var data = payload && payload.d ? payload.d : (payload || {});
        return {
            uname: String(data.Uname || data.uname || "").trim(),
            fullName: String(data.FullName || data.fullName || "").trim(),
            permissions: String(data.PermissionsCsv || "").split(",").map(function (code) {
                return String(code || "").trim();
            }).filter(Boolean),
            permissionRules: normalizePermissionRules(data.PermissionRulesJson || data.permissionRulesJson),
            canView: !!data.CanView,
            canEdit: !!data.CanEdit,
            canDelete: !!data.CanDelete,
            summaryText: String(data.SummaryText || data.summaryText || "").trim(),
            fetchedAt: new Date().toISOString()
        };
    }

    function scopeLabel(rule) {
        if (rule.scopeKind === "bukrs" && String(rule.scopeValue || "").toUpperCase() !== "ALL") {
            return "BE " + rule.scopeValue;
        }
        return "ALL";
    }

    function buildSheets(permissionRules) {
        if (PermissionPresentation && typeof PermissionPresentation.buildPermissionSheets === "function") {
            return PermissionPresentation.buildPermissionSheets(permissionRules, {
                codeOrder: ["01", "02", "03"],
                scopeLabel: scopeLabel,
                codeLabel: function (rule) {
                    return CODE_LABELS[rule.code] || rule.code;
                }
            });
        }
        return [];
    }

    function buildSummaryText(summaryText, sheets) {
        if (PermissionPresentation && typeof PermissionPresentation.buildSummaryText === "function") {
            return PermissionPresentation.buildSummaryText(summaryText, sheets, "");
        }
        return String(summaryText || "").trim();
    }

    function buildHeaderLabel(fullName, sheets) {
        if (PermissionPresentation && typeof PermissionPresentation.buildHeaderLabel === "function") {
            return PermissionPresentation.buildHeaderLabel(fullName, sheets, " | ");
        }
        return fullName;
    }

    function resolveModels() {
        var core = window.sap && sap.ui && sap.ui.getCore ? sap.ui.getCore() : null;
        var view = core && core.byId ? core.byId("checklist_app_comp---app") : null;
        if (!view || !view.getModel) {
            return null;
        }
        return {
            view: view,
            stateModel: view.getModel("state"),
            appViewModel: view.getModel("appView"),
            mainServiceModel: view.getModel("mainService") || view.getModel()
        };
    }

    function applyToModels(currentUser) {
        var models = resolveModels();
        var sheets;
        var fullName;
        var summaryText;
        if (!models || !models.stateModel || !models.appViewModel) {
            return false;
        }
        sheets = buildSheets(currentUser.permissionRules);
        fullName = String(currentUser.fullName || currentUser.uname || "Session profile unavailable").trim();
        summaryText = buildSummaryText(currentUser.summaryText, sheets);
        models.stateModel.setProperty("/currentUser", currentUser);
        models.appViewModel.setProperty("/shell/userLabel", buildHeaderLabel(fullName, sheets));
        models.appViewModel.setProperty("/shell/userLoginLabel", currentUser.uname || "");
        models.appViewModel.setProperty("/shell/userPermissions", sheets);
        models.appViewModel.setProperty("/shell/userSummaryText", summaryText);
        models.appViewModel.setProperty("/shell/userTooltip", summaryText || fullName);
        return true;
    }

    function fetchCurrentUser() {
        var models = resolveModels();
        var oMainServiceModel = models && models.mainServiceModel;

        if (!oMainServiceModel || typeof oMainServiceModel.read !== "function") {
            return Promise.reject(new Error("mainService ODataModel is not ready"));
        }

        return new Promise(function (resolve, reject) {
            oMainServiceModel.read("/CurrentUserSet('CURRENT')", {
                urlParameters: {
                    "__ts": Date.now()
                },
                success: function (oData) {
                    resolve(normalizeCurrentUser(oData || {}));
                },
                error: function (oError) {
                    reject(oError || new Error("CurrentUserSet read failed"));
                }
            });
        });
    }

    function syncFromBackend() {
        window.__shellUserRuntime = window.__shellUserRuntime || {};
        if (window.__shellUserRuntime.busy) {
            return Promise.resolve(false);
        }
        window.__shellUserRuntime.busy = true;
        return fetchCurrentUser().then(function (currentUser) {
            return applyToModels(currentUser);
        }).catch(function () {
            return false;
        }).finally(function () {
            window.__shellUserRuntime.busy = false;
        });
    }

    function boot() {
        syncFromBackend();
    }

    function startPolling() {
        window.__shellUserRuntime = window.__shellUserRuntime || {};
        if (window.__shellUserRuntime.timer) {
            return;
        }
        window.__shellUserRuntime.timer = window.setInterval(syncFromBackend, REFRESH_MS);
    }

    document.addEventListener("visibilitychange", function () {
        if (!document.hidden) {
            syncFromBackend();
        }
    }, { passive: true });

    if (window.sap && sap.ui && sap.ui.getCore) {
        sap.ui.getCore().attachInit(boot);
    }

    startPolling();
    (function bootstrapUntilModelsReady() {
        var attempts = 0;
        var timer = window.setInterval(function () {
            attempts += 1;
            if (resolveModels()) {
                window.clearInterval(timer);
                boot();
                return;
            }
            if (attempts > 120) {
                window.clearInterval(timer);
            }
        }, 1000);
    }());
}());
