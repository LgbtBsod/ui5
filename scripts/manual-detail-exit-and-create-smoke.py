#!/usr/bin/env python3
from __future__ import annotations

import json
import os
import sys
from pathlib import Path

try:
    from playwright.sync_api import sync_playwright
except ModuleNotFoundError:
    print("playwright unavailable")
    sys.exit(2)

ROOT = Path(__file__).resolve().parent.parent
ARTIFACT_DIR = ROOT / "docs" / "artifacts"
ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
BASE_URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html?sap-ui-xx-componentPreload=off&smoke=manual"
def wait_for_app(page):
    page.wait_for_load_state("domcontentloaded")
    page.wait_for_function(
        """
        () => {
          if (typeof sap === "undefined" || !sap.ui || !sap.ui.getCore) { return false; }
          const core = sap.ui.getCore();
          return !!core.byId("checklist_app_comp---app--mainFcl") && !!core.byId("checklist_app_comp---app--searchPaneHost");
        }
        """,
        timeout=90000,
    )
    page.wait_for_timeout(1200)


def wait_for_detail(page, root_id):
    page.wait_for_function(
        """
        (expectedRootId) => {
          const core = sap.ui.getCore();
          const app = core.byId("checklist_app_comp---app");
          const state = app && app.getModel && app.getModel("state");
          return !!state
            && state.getProperty("/currentRouteName") === "detail"
            && String(state.getProperty("/selectedId") || "") === expectedRootId;
        }
        """,
        arg=root_id,
        timeout=45000,
    )
    page.wait_for_timeout(1200)


def open_route(page, hash_fragment, route_name):
    page.goto(BASE_URL)
    wait_for_app(page)
    page.evaluate("(hash) => { window.location.hash = hash; }", hash_fragment)
    page.wait_for_function(
        """
        (expectedRoute) => {
          const state = sap.ui.getCore().byId('checklist_app_comp---app').getModel('state');
          return state.getProperty('/currentRouteName') === expectedRoute;
        }
        """,
        arg=route_name,
        timeout=30000,
    )
    page.wait_for_timeout(1200)


def state_dump(page):
    return page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId("checklist_app_comp---app");
          const state = app && app.getModel && app.getModel("state");
          const selected = core.byId("checklist_app_comp---app--detailPaneView")?.getModel?.("selected");
          return {
            hash: String(window.location.hash || ""),
            route: String(state?.getProperty("/currentRouteName") || ""),
            layout: String(state?.getProperty("/layout") || ""),
            editMode: String(state?.getProperty("/workflow/detail/editMode") || ""),
            lockState: String(state?.getProperty("/workflow/detail/lock/state") || ""),
            autosaveEnabled: !!state?.getProperty("/workflow/autosave/enabled"),
            dirty: !!state?.getProperty("/isDirty"),
            activeObjectId: String(state?.getProperty("/activeObjectId") || ""),
            selectedId: String(state?.getProperty("/selectedId") || ""),
            selectedRootId: String(selected?.getProperty?.("/root/id") || ""),
            bannerText: String(state?.getProperty("/ui/feedback/banner/global/text") || ""),
            bannerTextKey: String(state?.getProperty("/ui/feedback/banner/global/textKey") || "")
          };
        }
        """
    )


def click_edit_switch(page, desired_state: bool):
    page.evaluate(
        """
        (desiredState) => {
          const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneView');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller.onToggleEdit !== 'function') {
            throw new Error('detail controller toggle handler unavailable');
          }
          return Promise.resolve(controller.onToggleEdit({
            getParameter: function (name) {
              return name === 'state' ? desiredState : undefined;
            }
          }));
        }
        """,
        desired_state,
    )


def invoke_detail_handler(page, method_name):
    page.evaluate(
        """
        (methodName) => {
          const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneView');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error(methodName + ' unavailable');
          }
          return Promise.resolve(controller[methodName]());
        }
        """,
        method_name,
    )


def record_xhr(page):
    page.evaluate(
        """
        () => {
          if (window.__pcctTraceInstalled) { return; }
          window.__pcctTraceInstalled = true;
          window.__pcctBatchBodies = [];
          const origOpen = XMLHttpRequest.prototype.open;
          const origSend = XMLHttpRequest.prototype.send;
          XMLHttpRequest.prototype.open = function(method, url) {
            this.__pcctMethod = method;
            this.__pcctUrl = url;
            return origOpen.apply(this, arguments);
          };
          XMLHttpRequest.prototype.send = function(body) {
            try {
              window.__pcctBatchBodies.push({
                method: this.__pcctMethod || "",
                url: this.__pcctUrl || "",
                body: typeof body === "string" ? body : ""
              });
            } catch (e) {}
            return origSend.apply(this, arguments);
          };
        }
        """
    )


def main():
    report = {}
    with sync_playwright() as pw:
        browser = pw.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1440, "height": 960})

        # Scenario: create route should not autosave before first save, save should replace hash,
        # then turning edit off and leaving detail should not emit a false lock-release warning.
        open_route(page, "#/checklist/__CREATE", "detail")
        record_xhr(page)
        page.evaluate(
            """
            () => {
              const selected = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneView').getModel('selected');
              selected.setProperty('/root/equipment', 'EQ-CREATE-GUARD');
              sap.ui.getCore().byId('checklist_app_comp---app').getModel('state').setProperty('/isDirty', true);
            }
            """
        )
        page.wait_for_timeout(7000)
        pre_save_requests = page.evaluate("() => window.__pcctBatchBodies || []")
        pre_save_autosave = [r for r in pre_save_requests if "AutoSave" in (r.get("url") or "")]
        invoke_detail_handler(page, "onSaveDetail")
        page.wait_for_timeout(2500)
        page.wait_for_function(
            """
            () => {
              const state = sap.ui.getCore().byId('checklist_app_comp---app').getModel('state');
              const id = String(state.getProperty('/selectedId') || '');
              return !!id && id !== '__CREATE';
            }
            """,
            timeout=45000,
        )
        create_state = state_dump(page)
        click_edit_switch(page, False)
        page.wait_for_function(
            "() => sap.ui.getCore().byId('checklist_app_comp---app').getModel('state').getProperty('/workflow/detail/editMode') === 'READ'",
            timeout=30000,
        )
        page.wait_for_timeout(1200)
        invoke_detail_handler(page, "onCloseDetail")
        page.wait_for_function(
            "() => sap.ui.getCore().byId('checklist_app_comp---app').getModel('state').getProperty('/currentRouteName') === 'search'",
            timeout=30000,
        )
        exit_state = state_dump(page)
        exit_has_warning = "lockReleaseFailed" in (exit_state.get("bannerTextKey") or "") or "release lock" in (exit_state.get("bannerText") or "").lower()
        page.screenshot(path=str(ARTIFACT_DIR / "detail-create-after-first-save.png"))
        report["create_save_and_exit"] = {
            "preSaveAutoSaveRequests": len(pre_save_autosave),
            "postSaveState": create_state,
            "hashReplaced": "__CREATE" not in (create_state.get("hash") or ""),
            "postToggleCloseState": exit_state,
            "hasFalseReleaseWarning": exit_has_warning
        }

        browser.close()

    out_path = ARTIFACT_DIR / "manual-detail-exit-and-create-smoke.json"
    out_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
