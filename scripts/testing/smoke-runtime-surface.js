const { createFileExistenceSmoke } = require('./smoke-file-check');

async function runLockSmoke() {
  return createFileExistenceSmoke(
    'lock',
    ['app/service/ports/LockPort.js', 'app/service/runtime/LockStatusMonitor.js'],
    'lock lifecycle contract file exists'
  );
}

async function runNavigationSmoke() {
  return createFileExistenceSmoke(
    'nav',
    ['app/infra/navigation/RouteSync.js', 'app/controller/base/RouterMixin.js'],
    'navigation coordination layer exists'
  );
}

async function runNetworkSmoke() {
  return createFileExistenceSmoke(
    'network',
    ['app/service/backend/GatewayClient.js', 'app/service/backend/GatewayBackendService.js'],
    'backend network contract surface exists'
  );
}

module.exports = {
  runLockSmoke,
  runNavigationSmoke,
  runNetworkSmoke
};
