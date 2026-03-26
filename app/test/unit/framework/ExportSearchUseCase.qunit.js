sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ExportSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/SpreadsheetExport"
], function (ExportSearchUseCase, SpreadsheetExport) {
    "use strict";

    QUnit.module("ExportSearchUseCase");

    QUnit.test("export keeps explicit false values in normalized rows", function (assert) {
        var done = assert.async();
        var oUseCase = ExportSearchUseCase();
        var fnOriginalDownload = SpreadsheetExport.download;
        var oCaptured = null;

        SpreadsheetExport.download = function (_sFileName, aRows) {
            oCaptured = aRows;
            return Promise.resolve();
        };

        oUseCase.execute({
            entity: "screen",
            selectedRowIds: ["ROOT-1"]
        }, {
            repo: {
                exportSearchResults: function () {
                    return Promise.resolve([{
                        DB_KEY: "ROOT-1",
                        ItemType: "CHECK",
                        Result: false,
                        Text: "Failed item"
                    }]);
                }
            },
            stateModel: {
                getData: function () {
                    return {};
                }
            }
        }).then(function (oResult) {
            assert.strictEqual(oResult.ok, true, "use case succeeds");
            assert.ok(Array.isArray(oCaptured), "rows are passed to spreadsheet export");
            assert.strictEqual(oCaptured[0].Result, false, "explicit false is preserved");
            SpreadsheetExport.download = fnOriginalDownload;
            done();
        }).catch(function () {
            SpreadsheetExport.download = fnOriginalDownload;
            assert.ok(false, "Use case should not reject");
            done();
        });
    });

    QUnit.test("complex date range export falls back to currently bound rows", function (assert) {
        var done = assert.async();
        var oUseCase = ExportSearchUseCase();
        var fnOriginalDownload = SpreadsheetExport.download;
        var bRepoCalled = false;
        var bBoundRowsCalled = false;
        var oCaptured = null;

        SpreadsheetExport.download = function (_sFileName, aRows) {
            oCaptured = aRows;
            return Promise.resolve();
        };

        oUseCase.execute({
            entity: "screen",
            filterData: {
                DateCheck: {
                    ranges: [
                        { operation: "GE", value1: "2026-03-01" },
                        { operation: "LE", value1: "2026-03-31" }
                    ]
                }
            }
        }, {
            repo: {
                exportSearchResults: function () {
                    bRepoCalled = true;
                    return Promise.resolve([]);
                }
            },
            smartControls: {
                getBoundRows: function () {
                    bBoundRowsCalled = true;
                    return Promise.resolve([{
                        DB_KEY: "ROOT-77",
                        ItemType: "ROOT",
                        Result: false
                    }]);
                }
            },
            stateModel: {
                getData: function () {
                    return {};
                }
            }
        }).then(function (oResult) {
            assert.strictEqual(oResult.ok, true, "use case succeeds");
            assert.strictEqual(bBoundRowsCalled, true, "current bound rows are used");
            assert.strictEqual(bRepoCalled, false, "backend export is skipped for complex date range");
            assert.strictEqual(oCaptured[0].DB_KEY, "ROOT-77", "bound rows are exported with canonical root key");
            SpreadsheetExport.download = fnOriginalDownload;
            done();
        }).catch(function () {
            SpreadsheetExport.download = fnOriginalDownload;
            assert.ok(false, "Use case should not reject");
            done();
        });
    });
});
