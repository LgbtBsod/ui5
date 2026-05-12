#!/usr/bin/env node
const { spawn } = require('child_process');
const http = require('http');

async function waitForServer(url, timeoutMs = 30000) {
  const startedAt = Date.now();
  return new Promise((resolve, reject) => {
    const ping = () => {
      const req = http.get(url, (res) => {
        res.resume();
        if ((res.statusCode || 500) < 500) return resolve(true);
        if (Date.now() - startedAt > timeoutMs) return reject(new Error('server timeout'));
        setTimeout(ping, 500);
      });
      req.on('error', () => {
        if (Date.now() - startedAt > timeoutMs) return reject(new Error('server timeout'));
        setTimeout(ping, 500);
      });
    };
    ping();
  });
}

async function clickIfVisible(page, selector) {
  const locator = page.locator(selector).first();
  if (await locator.count()) {
    try {
      await locator.click({ timeout: 2000 });
      await page.waitForTimeout(500);
      return true;
    } catch (_) {
      return false;
    }
  }
  return false;
}

(async () => {
  let chromium;
  try {
    chromium = require('playwright').chromium;
  } catch {
    console.log('SKIP browser-ui-glitch-smoke: playwright not installed');
    process.exit(0);
  }

  const uiUrl = process.env.UI_SMOKE_URL || 'http://127.0.0.1:8080/index.html';
  const server = spawn('npx', ['ui5', 'serve', '--config', 'ui5.yaml'], { stdio: 'ignore' });

  try {
    await waitForServer('http://127.0.0.1:8080', 45000);
    const browser = await chromium.launch({ headless: true });
    const page = await browser.newPage({ viewport: { width: 1512, height: 982 } });
    const consoleErrors = [];
    const pageErrors = [];
    page.on('console', (msg) => { if (msg.type() === 'error') consoleErrors.push(msg.text()); });
    page.on('pageerror', (err) => pageErrors.push(String(err && err.message ? err.message : err)));

    await page.goto(uiUrl, { waitUntil: 'domcontentloaded' });
    await page.waitForTimeout(1500);

    await clickIfVisible(page, '.sapMSLITitleOnly');
    await clickIfVisible(page, '.sapMListTblRow');
    await clickIfVisible(page, 'button:has-text("Edit")');
    await clickIfVisible(page, 'button:has-text("Изменить")');
    await clickIfVisible(page, 'button:has-text("Save")');
    await clickIfVisible(page, 'button:has-text("Сохранить")');
    await page.waitForTimeout(1000);

    const busyCount = await page.locator('.sapUiLocalBusyIndicator').count();
    if (busyCount > 0) throw new Error('busy indicator stuck after scenario walk');
    if (pageErrors.length) throw new Error(`page errors detected: ${pageErrors.slice(0, 3).join(' | ')}`);
    if (consoleErrors.length) throw new Error(`console errors detected: ${consoleErrors.slice(0, 3).join(' | ')}`);

    await browser.close();
    console.log('PASS browser-ui-glitch-smoke: scenarios executed without UI/runtime glitches');
  } catch (error) {
    console.error('FAIL browser-ui-glitch-smoke:', error.message);
    process.exitCode = 1;
  } finally {
    server.kill('SIGTERM');
  }
})();
