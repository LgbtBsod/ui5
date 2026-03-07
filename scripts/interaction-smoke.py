#!/usr/bin/env python3
"""Runtime interaction smoke checks for FCL routes, resize freeze, dialogs and phone fallback."""

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
          const fcl = core.byId('sap_ui5_comp---app--mainFcl');
          const search = core.byId('sap_ui5_comp---app--searchPaneHost');
          const smartFilterBar = core.byId('sap_ui5_comp---app--searchPaneHost--searchSmartFilterBar');
          const smartTable = core.byId('sap_ui5_comp---app--searchPaneHost--searchSmartTable');
          const appReady = document.documentElement.getAttribute('data-ui5-app-ready') === 'true'
            && document.body.getAttribute('data-ui5-app-ready') === 'true';
          return !!fcl && !!search && !!smartFilterBar && !!smartTable && appReady;
        }
        """,
        timeout=90000,
    )
    page.wait_for_timeout(delay)


def wait_for_detail_ready(page, root_id: str = DETAIL_ROOT, delay: int = 1400) -> None:
    wait_for_app_ready(page, 900)
    page.wait_for_function(
        """
        (expectedRootId) => {
          const core = sap.ui.getCore();
          const view = core.byId('sap_ui5_comp---app--detailPaneHost');
          const objectPage = core.byId('sap_ui5_comp---app--detailPaneHost--detailObjectPage');
          const selected = view && view.getModel && view.getModel('selected');
          const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
          return !!view && !!objectPage && (!expectedRootId || rootId === expectedRootId);
        }
        """,
        arg=root_id,
        timeout=30000,
    )
    page.wait_for_selector(".detailControlStickyBlock", timeout=15000)
    page.wait_for_timeout(delay)


def wait_for_analytics_ready(page, delay: int = 1200) -> None:
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('sap_ui5_comp---app');
          const analyticsView = core.byId('sap_ui5_comp---app--analyticsPaneHost');
          const state = app && app.getModel && app.getModel('state');
          const viewModel = analyticsView && analyticsView.getModel && analyticsView.getModel('view');
          return !!analyticsView
            && !!analyticsView.getDomRef()
            && state
            && state.getProperty('/currentRouteName') === 'analytics'
            && viewModel
            && viewModel.getProperty('/busy') === false;
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(delay)


def wait_for_search_route(page, delay: int = 900) -> None:
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('sap_ui5_comp---app');
          const state = app && app.getModel && app.getModel('state');
          return !!state
            && state.getProperty('/currentRouteName') === 'search'
            && (state.getProperty('/layout') || 'OneColumn') === 'OneColumn';
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(delay)


def route_state(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('sap_ui5_comp---app');
          const fcl = core.byId('sap_ui5_comp---app--mainFcl');
          const state = app && app.getModel && app.getModel('state');
          const currentMid = fcl && fcl.getCurrentMidColumnPage && fcl.getCurrentMidColumnPage();
          return {
            hash: String(window.location.hash || ''),
            routeName: state && state.getProperty ? String(state.getProperty('/currentRouteName') || '') : '',
            layout: state && state.getProperty ? String(state.getProperty('/layout') || '') : '',
            selectedId: state && state.getProperty ? String(state.getProperty('/selectedId') || '') : '',
            midPageId: currentMid && currentMid.getId ? currentMid.getId() : ''
          };
        }
        """
    )


def invoke_controller_method(page, view_id: str, method_name: str, *args: Any) -> Any:
    return page.evaluate(
        """
        ({ viewId, methodName, args }) => {
          const view = sap.ui.getCore().byId(viewId);
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error(methodName + ' is not available on ' + viewId);
          }
          return Promise.resolve(controller[methodName].apply(controller, args || []));
        }
        """,
        {"viewId": view_id, "methodName": method_name, "args": list(args)},
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


def set_theme_mode(page, mode: str) -> dict[str, Any]:
    result = page.evaluate(
        """
        (targetMode) => {
          const view = sap.ui.getCore().byId('sap_ui5_comp---app');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller.onSelectThemeMode !== 'function') {
            throw new Error('onSelectThemeMode is not available');
          }
          return controller.onSelectThemeMode({
            getParameter: function (name) {
              return name === 'key' ? targetMode : undefined;
            }
          });
        }
        """,
        mode,
    )
    expected_theme = "dark" if mode == "night" else "light"
    expected_class = "appDark" if mode == "night" else "appLight"
    page.wait_for_function(
        """
        ({ expectedTheme, expectedClass, expectedMode }) => {
          const bgState = window.Ui5Bg && typeof window.Ui5Bg.getState === 'function' ? window.Ui5Bg.getState() : null;
          const app = sap.ui.getCore().byId('sap_ui5_comp---app');
          const appView = app && app.getModel && app.getModel('appView');
          return !!bgState
            && bgState.theme === expectedTheme
            && document.body.classList.contains(expectedClass)
            && (!appView || appView.getProperty('/themeMode') === expectedMode);
        }
        """,
        arg={"expectedTheme": expected_theme, "expectedClass": expected_class, "expectedMode": mode},
        timeout=15000,
    )
    page.wait_for_timeout(250)
    return result


def capture_resize_runtime(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const bg = document.getElementById('ui5-bg');
          const container = document.getElementById('ui5_container');
          const bgStyle = bg ? getComputedStyle(bg) : null;
          const containerStyle = container ? getComputedStyle(container) : null;
          const bgState = window.Ui5Bg && typeof window.Ui5Bg.getState === 'function' ? window.Ui5Bg.getState() : null;
          return {
            appReady: document.documentElement.getAttribute('data-ui5-app-ready') === 'true',
            resizing: document.documentElement.classList.contains('rnvResizing'),
            bgOpacity: bgStyle ? Number(bgStyle.opacity || 0) : -1,
            bgVisibility: bgStyle ? String(bgStyle.visibility || '') : '',
            containerVisibility: containerStyle ? String(containerStyle.visibility || '') : '',
            currentTheme: bgState ? String(bgState.theme || '') : '',
            bgState: bgState
          };
        }
        """
    )


def run_resize_trace(page, theme_mode: str) -> dict[str, Any]:
    set_theme_mode(page, theme_mode)
    page.set_viewport_size({"width": 1440, "height": 960})
    page.wait_for_timeout(180)

    samples = []
    widths = [1400, 1340, 1280, 1220, 1160, 1100, 1040, 1120, 1240, 1360, 1440]
    heights = [960, 948, 936, 924, 912, 900, 888, 900, 924, 948, 960]
    for width, height in zip(widths, heights):
        page.set_viewport_size({"width": width, "height": height})
        page.wait_for_timeout(85)
        samples.append({"width": width, "height": height, **capture_resize_runtime(page)})

    page.wait_for_function(
        """
        () => {
          const bgState = window.Ui5Bg && typeof window.Ui5Bg.getState === 'function' ? window.Ui5Bg.getState() : null;
          return !document.documentElement.classList.contains('rnvResizing')
            && !!bgState
            && bgState.resizing === false;
        }
        """,
        timeout=10000,
    )
    page.wait_for_timeout(160)
    final_state = capture_resize_runtime(page)
    ensure(any(sample["resizing"] for sample in samples), f"resize runtime did not enter resizing state for {theme_mode}")
    ensure(all(sample["appReady"] for sample in samples), f"app-ready marker dropped during {theme_mode} resize")
    ensure(all(sample["bgOpacity"] >= 0.99 for sample in samples), f"background opacity dropped during {theme_mode} resize")
    ensure(all(sample["containerVisibility"] != "hidden" for sample in samples), f"ui5 container became hidden during {theme_mode} resize")
    ensure(final_state["resizing"] is False, f"resize runtime did not settle after {theme_mode} resize")

    return {
        "themeMode": theme_mode,
        "samples": samples,
        "final": final_state,
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
        report["resizeMorning"] = run_resize_trace(page, "morning")
        report["resizeNight"] = run_resize_trace(page, "night")
        set_theme_mode(page, "morning")

        invoke_controller_method(page, "sap_ui5_comp---app", "onOpenShellAnalytics")
        wait_for_analytics_ready(page)
        shell_analytics_state = route_state(page)
        ensure(shell_analytics_state["routeName"] == "analytics", "shell analytics did not navigate to analytics route")
        ensure(shell_analytics_state["midPageId"].endswith("analyticsPaneHost"), "shell analytics did not activate analytics mid page")
        invoke_controller_method(page, "sap_ui5_comp---app--analyticsPaneHost", "onCloseAnalytics")
        wait_for_search_route(page)
        report["shellAnalytics"] = shell_analytics_state

        page.goto(f"{URL}#/checklist/{DETAIL_ROOT}", wait_until="networkidle", timeout=90000)
        wait_for_detail_ready(page, DETAIL_ROOT, 1600)
        invoke_controller_method(page, "sap_ui5_comp---app--detailPaneHost", "onOpenWorkflowAnalytics")
        wait_for_analytics_ready(page)
        detail_analytics_state = route_state(page)
        ensure(detail_analytics_state["routeName"] == "analytics", "detail analytics did not navigate to analytics route")
        invoke_controller_method(page, "sap_ui5_comp---app--analyticsPaneHost", "onCloseAnalytics")
        wait_for_detail_ready(page, DETAIL_ROOT, 1200)
        detail_return_state = route_state(page)
        ensure(detail_return_state["routeName"] in ("detail", "detailLayout"), "analytics close did not return to detail route")
        ensure(detail_return_state["selectedId"] == DETAIL_ROOT, "detail return lost selected root after analytics")
        report["detailAnalytics"] = {
            "opened": detail_analytics_state,
            "returned": detail_return_state,
        }

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
            pinned_state["stickyTop"] is not None
            and max(0, pinned_state["shellHeaderBottom"] - 4) <= pinned_state["stickyTop"] <= (pinned_state["shellHeaderBottom"] + 160),
            "sticky checklist control did not pin near the top of the detail viewport",
        )
        report["detailScroll"] = {
            "beforeTop": round(scroll_state["beforeTop"], 2),
            "pinnedTop": round(pinned_state["stickyTop"], 2),
            "scrollTop": round(pinned_state["scrollTop"], 2),
            "shellHeaderBottom": round(pinned_state["shellHeaderBottom"], 2),
            "targetClass": scroll_state["targetClass"],
            "stickyHeight": round(sticky_after["height"], 2),
        }

        invoke_controller_method(page, "sap_ui5_comp---app--detailPaneHost", "onExpandChecks")
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
        invoke_controller_method(page, "sap_ui5_comp---app--detailPaneHost", "onCloseChecksExpanded")
        page.wait_for_timeout(350)
        report["checksExpandedDialog"] = {**checks_dialog, **checks_contract}

        invoke_controller_method(page, "sap_ui5_comp---app--detailPaneHost", "onExpandBarriers")
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
        invoke_controller_method(page, "sap_ui5_comp---app--detailPaneHost", "onCloseBarriersExpanded")
        page.wait_for_timeout(350)
        report["barriersExpandedDialog"] = {**barriers_dialog, **barriers_contract}

        invoke_controller_method(page, "sap_ui5_comp---app--detailPaneHost", "onOpenLocationValueHelp")
        value_help_box = wait_for_dialog(page, "locationValueHelpDialog")
        ensure(value_help_box["height"] > 300, "location value help did not open correctly")
        invoke_controller_method(page, "sap_ui5_comp---app--detailPaneHost", "onCloseLocationValueHelp")
        page.wait_for_timeout(350)
        report["locationDialog"] = value_help_box

        phone_page = browser.new_page(viewport={"width": 390, "height": 844})
        phone_page.goto(f"{URL}#/checklist/{DETAIL_ROOT}", wait_until="networkidle", timeout=90000)
        wait_for_detail_ready(phone_page, DETAIL_ROOT, 1800)
        invoke_controller_method(phone_page, "sap_ui5_comp---app--detailPaneHost", "onExpandChecks")
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
