#!/usr/bin/env python3
"""Shared UI5 browser bootstrap and routing helpers for headless smoke scripts."""

from __future__ import annotations

from typing import Any


def is_navigation_race(exc: Exception) -> bool:
    message = str(exc or "")
    return "Execution context was destroyed" in message or "Cannot find context with specified id" in message


def safe_evaluate(page, script: str, arg: Any = None, retries: int = 3):
    last_error = None
    for attempt in range(max(1, int(retries))):
        try:
            if arg is None:
                return page.evaluate(script)
            return page.evaluate(script, arg)
        except Exception as exc:  # noqa: BLE001
            last_error = exc
            if not is_navigation_race(exc) or attempt >= retries - 1:
                raise
            page.wait_for_timeout(750)
    raise last_error


def wait_for_ui5_core(page, timeout: int = 90000) -> None:
    page.wait_for_function(
        """
        () => {
          if (typeof window === 'undefined' || typeof window.sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
            return false;
          }
          return true;
        }
        """,
        timeout=timeout,
    )


def ensure_component_mounted(page) -> None:
    safe_evaluate(
        page,
        """
        () => new Promise((resolve, reject) => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          if (app) {
            resolve('already-mounted');
            return;
          }
          if (window.__codexMountInFlight) {
            resolve('mount-pending');
            return;
          }
          window.__codexMountInFlight = true;
          sap.ui.require([
            'sap/ui/core/ComponentContainer',
            'PRODUCTION_CONTROL_CHECKLIST/infra/adapters/Ui5StyleAdapter'
          ], function (ComponentContainer, Ui5StyleAdapter) {
            try {
              if (Ui5StyleAdapter && typeof Ui5StyleAdapter.enableOn !== 'function') {
                Ui5StyleAdapter.enableOn = function (oTarget, sClassNames) {
                  String(sClassNames || '').split(/\\s+/).map(function (sName) { return sName.trim(); }).filter(Boolean).forEach(function (sClassName) {
                    if (oTarget && typeof oTarget.addStyleClass === 'function') {
                      oTarget.addStyleClass(sClassName);
                    }
                  });
                  return oTarget;
                };
              }
              if (Ui5StyleAdapter && typeof Ui5StyleAdapter.disableOn !== 'function') {
                Ui5StyleAdapter.disableOn = function (oTarget, sClassNames) {
                  String(sClassNames || '').split(/\\s+/).map(function (sName) { return sName.trim(); }).filter(Boolean).forEach(function (sClassName) {
                    if (oTarget && typeof oTarget.removeStyleClass === 'function') {
                      oTarget.removeStyleClass(sClassName);
                    }
                  });
                  return oTarget;
                };
              }
              if (core.byId('checklist_app_comp---app')) {
                resolve('already-mounted');
                return;
              }
              sap.ui.component({
                name: 'PRODUCTION_CONTROL_CHECKLIST',
                manifest: true,
                async: true
              }).then(function (oComponent) {
                let oContainer = core.byId('checklist_app_comp');
                if (!oContainer) {
                  oContainer = new ComponentContainer({
                    id: 'checklist_app_comp',
                    component: oComponent,
                    height: '100%'
                  });
                  oContainer.placeAt('ui5_container');
                } else if (typeof oContainer.setComponent === 'function') {
                  oContainer.setComponent(oComponent);
                }
                if (core.applyChanges) {
                  core.applyChanges();
                }
                window.__codexMountInFlight = false;
                resolve('mounted');
              }).catch(function (err) {
                window.__codexMountInFlight = false;
                reject(err);
              });
            } catch (err) {
              window.__codexMountInFlight = false;
              reject(err);
            }
          }, function (err) {
            window.__codexMountInFlight = false;
            reject(err);
          });
        })
        """,
    )


def wait_for_app_ready(page, timeout: int = 90000) -> None:
    wait_for_ui5_core(page, timeout=timeout)
    ensure_component_mounted(page)
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const component = app && sap.ui.core.Component.getOwnerComponentFor(app);
          const router = component && component.getRouter && component.getRouter();
          const fcl = core.byId('checklist_app_comp---app--mainFcl');
          return !!app && !!component && !!router && !!fcl;
        }
        """,
        timeout=timeout,
    )


def collect_bootstrap_diagnostics(page) -> dict[str, Any]:
    return safe_evaluate(
        page,
        """
        () => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          function findView(controllerName) {
            return all.find((item) => item
              && item.isA
              && item.isA('sap.ui.core.mvc.View')
              && item.getController
              && item.getController()
              && item.getController().getMetadata
              && item.getController().getMetadata().getName() === controllerName) || null;
          }
          function findBySuffix(suffix) {
            return all.find((item) => item && item.getId && String(item.getId()).endsWith(suffix)) || null;
          }
          const app = core.byId('checklist_app_comp---app');
          const component = app && sap.ui.core.Component.getOwnerComponentFor(app);
          const router = component && component.getRouter && component.getRouter();
          const state = app && app.getModel && app.getModel('state');
          const fcl = core.byId('checklist_app_comp---app--mainFcl');
          const searchView = findView('PRODUCTION_CONTROL_CHECKLIST.controller.Search');
          const detailView = findView('PRODUCTION_CONTROL_CHECKLIST.controller.Detail');
          const analyticsView = findView('PRODUCTION_CONTROL_CHECKLIST.controller.Analytics');
          const searchDock = findBySuffix('searchWorkbenchDock');
          const smartTable = findBySuffix('searchSmartTable');
          const smartFilterBar = findBySuffix('searchSmartFilterBar');
          const detailObjectPage = findBySuffix('detailObjectPage');
          const selected = detailView && detailView.getModel && detailView.getModel('selected');
          const detailState = detailView && detailView.getModel && detailView.getModel('state');
          return {
            hash: String(window.location.hash || ''),
            appId: app && app.getId ? String(app.getId()) : '',
            componentId: component && component.getId ? String(component.getId()) : '',
            hasRouter: !!router,
            routeName: state && state.getProperty ? String(state.getProperty('/currentRouteName') || '') : '',
            layout: state && state.getProperty ? String(state.getProperty('/layout') || '') : '',
            selectedId: state && state.getProperty ? String(state.getProperty('/selectedId') || '') : '',
            activeObjectId: state && state.getProperty ? String(state.getProperty('/activeObjectId') || '') : '',
            fclId: fcl && fcl.getId ? String(fcl.getId()) : '',
            fclBusy: !!(fcl && fcl.getBusy && fcl.getBusy()),
            searchViewId: searchView && searchView.getId ? String(searchView.getId()) : '',
            detailViewId: detailView && detailView.getId ? String(detailView.getId()) : '',
            analyticsViewId: analyticsView && analyticsView.getId ? String(analyticsView.getId()) : '',
            searchDockId: searchDock && searchDock.getId ? String(searchDock.getId()) : '',
            smartTableId: smartTable && smartTable.getId ? String(smartTable.getId()) : '',
            smartFilterBarId: smartFilterBar && smartFilterBar.getId ? String(smartFilterBar.getId()) : '',
            detailObjectPageId: detailObjectPage && detailObjectPage.getId ? String(detailObjectPage.getId()) : '',
            detailRootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            detailMode: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/editMode') || '') : '',
            detailLockState: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/lock/state') || '') : ''
          };
        }
        """,
    )


def resolve_view_id(page, controller_name: str) -> str:
    result = safe_evaluate(
        page,
        """
        (controllerName) => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const view = all.find((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === controllerName) || null;
          return String((view && view.getId && view.getId()) || '');
        }
        """,
        controller_name,
    )
    if not result:
        raise RuntimeError(f"view not resolved: {controller_name}")
    return str(result)


def navigate_to_search(page) -> dict[str, Any]:
    wait_for_app_ready(page)
    safe_evaluate(
        page,
        """
        () => new Promise((resolve, reject) => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const searchView = all.find((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Search');
          const controller = searchView && searchView.getController ? searchView.getController() : null;
          const component = app && sap.ui.core.Component.getOwnerComponentFor(app);
          const router = component && component.getRouter && component.getRouter();
          sap.ui.require(['PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService'], function (NavigationIntentService) {
            try {
              if (controller && NavigationIntentService && typeof NavigationIntentService.navigateToSearch === 'function') {
                Promise.resolve(NavigationIntentService.navigateToSearch(controller)).then(resolve).catch(reject);
                return;
              }
              if (router && typeof router.navTo === 'function') {
                router.navTo('search', {}, false);
                resolve(true);
                return;
              }
              reject(new Error('search router unavailable'));
            } catch (err) {
              reject(err);
            }
          }, reject);
        })
        """,
    )
    return wait_for_search_ready(page)


def navigate_to_detail(page, root_id: str, layout: str | None = None) -> dict[str, Any]:
    wait_for_app_ready(page)
    payload = {"rootId": root_id, "layout": layout or ""}
    safe_evaluate(
        page,
        """
        ({ rootId, layout }) => new Promise((resolve, reject) => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const detailView = all.find((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Detail');
          const searchView = all.find((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Search');
          const controller = (detailView && detailView.getController && detailView.getController())
            || (searchView && searchView.getController && searchView.getController())
            || (app && app.getController && app.getController())
            || null;
          const component = app && sap.ui.core.Component.getOwnerComponentFor(app);
          const router = component && component.getRouter && component.getRouter();
          sap.ui.require(['PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService'], function (NavigationIntentService) {
            try {
              if (controller && NavigationIntentService && typeof NavigationIntentService.navigateToDetail === 'function') {
                Promise.resolve(NavigationIntentService.navigateToDetail(controller, String(rootId || ''), layout || undefined)).then(resolve).catch(reject);
                return;
              }
              if (router && typeof router.navTo === 'function') {
                if (layout) {
                  router.navTo('detailLayout', { id: String(rootId || ''), layout: String(layout) }, false);
                } else {
                  router.navTo('detail', { id: String(rootId || '') }, false);
                }
                resolve(true);
                return;
              }
              reject(new Error('detail router unavailable'));
            } catch (err) {
              reject(err);
            }
          }, reject);
        })
        """,
        payload,
    )
    return wait_for_detail_ready(page, root_id, layout or "")


def wait_for_search_ready(page, timeout: int = 45000) -> dict[str, Any]:
    wait_for_app_ready(page, timeout=max(timeout, 60000))
    page.wait_for_timeout(min(timeout, 15000))
    diagnostics = collect_bootstrap_diagnostics(page)
    body_text = safe_evaluate(page, "() => document.body && document.body.innerText ? document.body.innerText : ''")
    if diagnostics.get("routeName") != "search":
        raise RuntimeError(f"search route not ready: {diagnostics}")
    if "Create" not in body_text and "Создать" not in body_text:
        raise RuntimeError("search create action not rendered")
    page.wait_for_timeout(1200)
    return diagnostics


def wait_for_detail_ready(page, root_id: str, layout: str = "", timeout: int = 45000) -> dict[str, Any]:
    wait_for_app_ready(page, timeout=max(timeout, 60000))
    payload = {"rootId": root_id, "layout": layout}
    page.wait_for_function(
        """
        ({ rootId, layout }) => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const app = core.byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          const detailView = all.find((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Detail') || null;
          const detailObjectPage = all.find((item) => item && item.getId && String(item.getId()).endsWith('detailObjectPage')) || null;
          const selected = detailView && detailView.getModel && detailView.getModel('selected');
          const selectedRoot = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
          const routeName = state && state.getProperty ? String(state.getProperty('/currentRouteName') || '') : '';
          const currentLayout = state && state.getProperty ? String(state.getProperty('/layout') || '') : '';
          const domReady = !!(detailView && detailView.getDomRef && detailView.getDomRef());
          const layoutOk = !layout || currentLayout === layout || currentLayout === 'TwoColumnsMidExpanded';
          return !!state
            && !!detailView
            && !!detailObjectPage
            && domReady
            && routeName === 'detail'
            && layoutOk
            && selectedRoot === String(rootId || '');
        }
        """,
        arg=payload,
        timeout=timeout,
    )
    page.wait_for_timeout(1500)
    return collect_bootstrap_diagnostics(page)


def invoke_controller_method(page, controller_name: str, method_name: str, *args: Any) -> Any:
    return safe_evaluate(
        page,
        """
        ({ controllerName, methodName, args }) => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const view = all.find((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === controllerName) || null;
          const controller = view && view.getController ? view.getController() : null;
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error('Controller method not found: ' + controllerName + ':' + methodName);
          }
          return Promise.resolve(controller[methodName].apply(controller, args || []));
        }
        """,
        {"controllerName": controller_name, "methodName": method_name, "args": list(args)},
    )


def get_tail_search_row(page) -> dict[str, Any]:
    payload = safe_evaluate(
        page,
        """
        () => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const smartTable = all.find((item) => item && item.isA && item.isA('sap.ui.comp.smarttable.SmartTable')) || null;
          const table = (smartTable && smartTable.getTable && smartTable.getTable())
            || all.find((item) => item && item.isA && item.isA('sap.m.Table') && item.getItems) || null;
          const rows = table && table.getItems ? table.getItems().filter((item) => !!(item && item.getVisible && item.getVisible() && item.getBindingContext && item.getBindingContext())) : [];
          const last = rows.length ? rows[rows.length - 1] : null;
          const ctx = last && last.getBindingContext ? last.getBindingContext() : null;
          const data = ctx && ctx.getObject ? ctx.getObject() : {};
          const dom = last && last.getDomRef ? last.getDomRef() : null;
          const tableDom = table && table.getDomRef ? table.getDomRef() : null;
          const scrollHost = tableDom && tableDom.querySelector ? tableDom.querySelector('.sapMListTblCnt') : null;
          if (scrollHost) {
            scrollHost.scrollLeft = scrollHost.scrollWidth;
            scrollHost.scrollTop = scrollHost.scrollHeight;
          }
          if (dom && dom.scrollIntoView) {
            dom.scrollIntoView({ block: 'end', inline: 'nearest' });
          }
          return {
            domId: dom && dom.id ? String(dom.id) : '',
            rootKey: String(data.Key || data.RootKey || '').trim(),
            checklistId: String(data.Id || data.ChecklistId || '').trim(),
            tableId: table && table.getId ? String(table.getId()) : ''
          };
        }
        """,
    )
    if not payload.get("domId"):
        raise RuntimeError("search tail row not resolved")
    return payload
