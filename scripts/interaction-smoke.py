#!/usr/bin/env python3
"""Runtime interaction smoke checks for splitter, sticky layout, dialogs and phone fallback."""

from __future__ import annotations

import json
import sys
from typing import Any

try:
    from playwright.sync_api import sync_playwright
except ModuleNotFoundError:
    sync_playwright = None

URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
DETAIL_ROOT = "E49B679F518F4947BD7A0F2CC1C4AC46"


def ensure(condition: bool, message: str) -> None:
    if not condition:
        raise RuntimeError(message)


def wait_for_app_ready(page, delay: int = 1200) -> None:
    page.wait_for_load_state("domcontentloaded")
    page.wait_for_load_state("networkidle")
    page.wait_for_function(
        """
        () => {
          if (typeof sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
            return false;
          }
          const core = sap.ui.getCore();
          const search = core.byId('sap_ui5_comp---app--searchPaneHost');
          const detail = core.byId('sap_ui5_comp---app--detailPaneHost');
          const splitter = document.querySelector('#sap_ui5_comp---app--mainSplitter');
          const rootReady = document.documentElement.classList.contains('rnvAppRoot')
            && document.body.classList.contains('rnvAppRoot');
          return !!search && !!detail && !!splitter && rootReady;
        }
        """,
        timeout=90000,
    )
    page.wait_for_timeout(delay)


def wait_for_detail_ready(page, delay: int = 1400) -> None:
    wait_for_app_ready(page, delay)
    page.wait_for_function(
        """
        () => {
          const scroll = document.querySelector('.sapUxAPObjectPageScroll');
          const sticky = document.querySelector('.detailControlStickyBlock');
          return !!scroll && !!sticky;
        }
        """,
        timeout=15000,
    )
    page.wait_for_timeout(250)


def invoke_detail_action(page, action_name: str) -> None:
    page.evaluate(
        f"""
        () => {{
          const core = sap.ui.getCore();
          const view = core.byId('sap_ui5_comp---app--detailPaneHost');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller['{action_name}'] !== 'function') {{
            throw new Error('{action_name} is not available');
          }}
          controller['{action_name}']();
        }}
        """
    )


def wait_for_dialog(page, dialog_id_suffix: str) -> dict[str, float]:
    selector = f"[id$='{dialog_id_suffix}']"
    page.wait_for_selector(selector, timeout=15000)
    dialog_box = page.locator(selector).first.bounding_box()
    ensure(dialog_box is not None, f"{dialog_id_suffix} geometry is not available")
    return {
        "width": round(dialog_box["width"], 2),
        "height": round(dialog_box["height"], 2),
    }


def main() -> int:
    if sync_playwright is None:
        print("[error] playwright is not installed.")
        return 2

    report: dict[str, Any] = {}

    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": 1440, "height": 960})

        page.goto(URL, wait_until="networkidle", timeout=90000)
        wait_for_app_ready(page, 1800)
        page.get_by_text("Create", exact=True).click()
        page.wait_for_timeout(2200)

        splitter = page.locator(".sapUiLoSplitterBar").first
        before = page.locator("#sap_ui5_comp---app--mainSplitter-content-0").bounding_box()
        bar = splitter.bounding_box()
        ensure(before is not None and bar is not None, "splitter geometry is not available")
        page.mouse.move(bar["x"] + bar["width"] / 2, bar["y"] + 200)
        page.mouse.down()
        page.mouse.move(bar["x"] + bar["width"] / 2 + 150, bar["y"] + 200, steps=15)
        page.mouse.up()
        page.wait_for_timeout(500)
        after = page.locator("#sap_ui5_comp---app--mainSplitter-content-0").bounding_box()
        ensure(after is not None, "splitter left pane geometry is missing after drag")
        ensure(abs(after["width"] - before["width"]) > 20, "splitter drag did not change pane width")

        split_state = page.evaluate(
            """
            () => ({
              splitterClass: document.querySelector('#sap_ui5_comp---app--mainSplitter')?.className,
              barClass: document.querySelector('.sapUiLoSplitterBar')?.className,
              leftPaneY: document.querySelector('#sap_ui5_comp---app--mainSplitter-content-0')?.getBoundingClientRect()?.y,
              rightPaneY: document.querySelector('#sap_ui5_comp---app--mainSplitter-content-1')?.getBoundingClientRect()?.y
            })
            """
        )
        ensure("appSplitModeSplit" in str(split_state.get("splitterClass") or ""), "splitter is not in split visual mode")
        ensure(
            abs((split_state.get("leftPaneY") or 0) - (split_state.get("rightPaneY") or 0)) < 1,
            "detail pane is not aligned horizontally after split"
        )
        report["splitter"] = {
            "beforeWidth": round(before["width"], 2),
            "afterWidth": round(after["width"], 2),
            **split_state
        }

        page.goto(URL, wait_until="networkidle", timeout=90000)
        wait_for_app_ready(page, 1500)
        page.goto(f"{URL}#/checklist/{DETAIL_ROOT}", wait_until="networkidle", timeout=90000)
        wait_for_detail_ready(page, 1600)
        invoke_detail_action(page, "onOpenWorkflowAnalytics")
        analytics_box = wait_for_dialog(page, "workflowAnalyticsDialog")
        ensure(analytics_box["width"] > 400, "analytics dialog did not open correctly")
        page.evaluate(
            """
            () => {
              const core = sap.ui.getCore();
              const view = core.byId('sap_ui5_comp---app--searchPaneHost');
              const controller = view && view.getController && view.getController();
              if (!controller || typeof controller.onCloseWorkflowAnalytics !== 'function') {
                throw new Error('onCloseWorkflowAnalytics is not available');
              }
              controller.onCloseWorkflowAnalytics();
            }
            """
        )
        page.wait_for_timeout(400)
        report["analyticsDialog"] = analytics_box

        page.goto(f"{URL}#/checklist/{DETAIL_ROOT}", wait_until="networkidle", timeout=90000)
        wait_for_detail_ready(page, 1800)
        sticky_before = page.locator(".detailControlStickyBlock").first.bounding_box()
        ensure(sticky_before is not None, "sticky control rail is not rendered")
        scroll_state = page.evaluate(
            """
            () => {
              const pickScrollTarget = () => {
                const candidates = [
                  document.querySelector('.sapUxAPObjectPageWrapper'),
                  document.querySelector('.sapUxAPObjectPageScroll'),
                  document.querySelector('.sapUxAPObjectPageContainer'),
                  document.querySelector('.sapMPageEnableScrolling'),
                  document.scrollingElement
                ].filter(Boolean);
                return candidates.find((node) => (node.scrollHeight - node.clientHeight) > 120) || null;
              };
              const scroll = pickScrollTarget();
              if (!scroll) {
                return null;
              }
              const beforeTop = document.querySelector('.detailControlStickyBlock')?.getBoundingClientRect().top || 0;
              scroll.scrollTop = Math.max(320, Math.min(scroll.scrollHeight - scroll.clientHeight, 900));
              return {
                beforeTop,
                targetClass: scroll.className || scroll.tagName,
                scrollTop: scroll.scrollTop
              };
            }
            """
        )
        ensure(scroll_state is not None, "detail scroll container is not available")
        page.wait_for_timeout(450)
        sticky_after = page.locator(".detailControlStickyBlock").first.bounding_box()
        ensure(sticky_after is not None, "sticky control rail geometry is missing after scroll")
        pinned_state = page.evaluate(
            """
            () => {
              const candidates = [
                document.querySelector('.sapUxAPObjectPageWrapper'),
                document.querySelector('.sapUxAPObjectPageScroll'),
                document.querySelector('.sapUxAPObjectPageContainer'),
                document.querySelector('.sapMPageEnableScrolling'),
                document.scrollingElement
              ].filter(Boolean);
              const scroll = candidates.find((node) => (node.scrollHeight - node.clientHeight) > 120) || null;
              const sticky = document.querySelector('.detailControlStickyBlock');
              const shellHeader = document.querySelector('.appShellHeader');
              return {
                scrollTop: scroll ? scroll.scrollTop : 0,
                stickyTop: sticky ? sticky.getBoundingClientRect().top : null,
                shellHeaderBottom: shellHeader ? shellHeader.getBoundingClientRect().bottom : 0
              };
            }
            """
        )
        ensure(pinned_state["scrollTop"] > 200, "detail page did not scroll far enough")
        ensure(
            pinned_state["stickyTop"] is not None and
            max(0, pinned_state["shellHeaderBottom"] - 4) <= pinned_state["stickyTop"] <= (pinned_state["shellHeaderBottom"] + 160),
            "sticky checklist control did not pin near the top of the detail viewport"
        )
        report["detailScroll"] = {
            "beforeTop": round(scroll_state["beforeTop"], 2),
            "pinnedTop": round(pinned_state["stickyTop"], 2),
            "scrollTop": round(pinned_state["scrollTop"], 2),
            "shellHeaderBottom": round(pinned_state["shellHeaderBottom"], 2),
            "targetClass": scroll_state["targetClass"],
            "stickyHeight": round(sticky_after["height"], 2),
        }

        invoke_detail_action(page, "onExpandChecks")
        checks_dialog = wait_for_dialog(page, "checksExpandedDialog")
        checks_contract = page.evaluate(
            """
            () => {
              const dialog = document.querySelector("[id$='checksExpandedDialog']");
              return {
                hasGridTable: !!dialog?.querySelector('.sapUiTable'),
                hasPhoneTable: !!dialog?.querySelector('.sapMListTbl')
              };
            }
            """
        )
        ensure(checks_contract["hasGridTable"], "checks expanded dialog is missing desktop grid table")
        invoke_detail_action(page, "onCloseChecksExpanded")
        page.wait_for_timeout(350)
        report["checksExpandedDialog"] = {**checks_dialog, **checks_contract}

        invoke_detail_action(page, "onExpandBarriers")
        barriers_dialog = wait_for_dialog(page, "barriersExpandedDialog")
        barriers_contract = page.evaluate(
            """
            () => {
              const dialog = document.querySelector("[id$='barriersExpandedDialog']");
              return {
                hasGridTable: !!dialog?.querySelector('.sapUiTable'),
                hasPhoneTable: !!dialog?.querySelector('.sapMListTbl')
              };
            }
            """
        )
        ensure(barriers_contract["hasGridTable"], "barriers expanded dialog is missing desktop grid table")
        invoke_detail_action(page, "onCloseBarriersExpanded")
        page.wait_for_timeout(350)
        report["barriersExpandedDialog"] = {**barriers_dialog, **barriers_contract}

        invoke_detail_action(page, "onOpenLocationValueHelp")
        value_help_box = wait_for_dialog(page, "locationValueHelpDialog")
        ensure(value_help_box["height"] > 300, "location value help did not open correctly")
        invoke_detail_action(page, "onCloseLocationValueHelp")
        page.wait_for_timeout(350)
        report["locationDialog"] = value_help_box

        phone_page = browser.new_page(viewport={"width": 390, "height": 844})
        phone_page.goto(f"{URL}#/checklist/{DETAIL_ROOT}", wait_until="networkidle", timeout=90000)
        wait_for_detail_ready(phone_page, 1800)
        invoke_detail_action(phone_page, "onExpandChecks")
        phone_checks_dialog = wait_for_dialog(phone_page, "checksExpandedDialog")
        phone_contract = phone_page.evaluate(
            """
            () => {
              const core = typeof sap !== 'undefined' && sap.ui && sap.ui.getCore && sap.ui.getCore();
              const dialog = document.querySelector("[id$='checksExpandedDialog']");
              const grid = dialog?.querySelector('.sapUiTable');
              const list = dialog?.querySelector('.sapMListTbl');
              return {
                devicePhone: !!(core && core.getModel && core.getModel('device') && core.getModel('device').getProperty('/system/phone')),
                gridVisible: !!(grid && grid.getBoundingClientRect().height > 0),
                phoneTableVisible: !!(list && list.getBoundingClientRect().height > 0)
              };
            }
            """
        )
        if phone_contract["devicePhone"]:
            ensure(phone_contract["phoneTableVisible"], "phone fallback table is not visible in checks expanded dialog")
            ensure(not phone_contract["gridVisible"], "desktop grid table remained visible on phone viewport")
        report["phoneChecksDialog"] = {**phone_checks_dialog, **phone_contract}
        phone_page.close()

        browser.close()

    print(json.dumps({"ok": True, "report": report}, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
