(function () {
    "use strict";

    var CODE_LABELS = {
        "01": "View",
        "02": "Edit/Create",
        "03": "Delete"
    };
    var REFRESH_MS = 15000;

    function normalizeRule(rule) {
        return {
            code: String(rule && rule.code || "").trim(),
            scopeKind: String(rule && (rule.scopeKind || rule.scope_kind) || "all").trim().toLowerCase() || "all",
            scopeValue: String(rule && (rule.scopeValue || rule.scope_value) || "ALL").trim() || "ALL"
        };
    }

    function normalizePermissionRules(value) {
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
        var groups = {};
        var order = { "01": 1, "02": 2, "03": 3 };
        (Array.isArray(permissionRules) ? permissionRules : []).map(normalizeRule).forEach(function (rule) {
            var key = rule.scopeKind + ":" + String(rule.scopeValue || "ALL").toUpperCase();
            var group;
            if (!rule.code) {
                return;
            }
            group = groups[key];
            if (!group) {
                group = {
                    title: scopeLabel(rule),
                    description: "",
                    scope: "",
                    info: "",
                    codes: [],
                    labels: []
                };
                groups[key] = group;
            }
            if (group.codes.indexOf(rule.code) === -1) {
                group.codes.push(rule.code);
                group.labels.push(CODE_LABELS[rule.code] || rule.code);
            }
        });
        return Object.keys(groups).map(function (key) {
            var group = groups[key];
            group.codes.sort(function (a, b) {
                return (order[a] || 99) - (order[b] || 99);
            });
            group.description = group.codes.join(", ");
            group.scope = group.labels.join(" / ");
            group.info = group.scope;
            return group;
        }).sort(function (a, b) {
            if (a.title === "ALL") {
                return -1;
            }
            if (b.title === "ALL") {
                return 1;
            }
            return String(a.title).localeCompare(String(b.title));
        });
    }

    function buildSummaryText(summaryText, sheets) {
        if (String(summaryText || "").trim()) {
            return String(summaryText || "").trim();
        }
        return sheets.map(function (sheet) {
            return sheet.title + ": " + sheet.description;
        }).join("; ");
    }

    function buildHeaderLabel(fullName, sheets) {
        var suffix = sheets.map(function (sheet) {
            return sheet.title + ": " + sheet.description;
        }).join(" | ");
        if (!suffix) {
            return fullName;
        }
        return fullName + " | " + suffix;
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
            appViewModel: view.getModel("appView") || view.getModel("view"),
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
        models.view.__shellUserRuntimeBound = true;
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
        if (window.__shellUserRuntimeBusy) {
            return Promise.resolve(false);
        }
        window.__shellUserRuntimeBusy = true;
        return fetchCurrentUser().then(function (currentUser) {
            return applyToModels(currentUser);
        }).catch(function () {
            return false;
        }).finally(function () {
            window.__shellUserRuntimeBusy = false;
        });
    }

    function boot() {
        syncFromBackend();
    }

    function startPolling() {
        if (window.__shellUserRuntimeTimer) {
            return;
        }
        window.__shellUserRuntimeTimer = window.setInterval(syncFromBackend, REFRESH_MS);
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
