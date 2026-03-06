const { createFileExistenceSmoke } = require('./smoke-file-check');

async function runLockSmoke() {
  return createFileExistenceSmoke(
    'lock',
    ['ports/LockPort.js', 'manager/LockStatusMonitor.js'],
    'lock lifecycle contract file exists'
  );
}

async function runNavigationSmoke() {
  return createFileExistenceSmoke(
    'nav',
    ['infra/navigation/RouteSync.js', 'controller/base/RouterMixin.js'],
    'navigation coordination layer exists'
  );
}

async function runNetworkSmoke() {
  return createFileExistenceSmoke(
    'network',
    ['service/backend/GatewayClient.js', 'service/backend/GatewayBackendService.js'],
    'backend network contract surface exists'
  );
}

module.exports = {
  runLockSmoke,
  runNavigationSmoke,
  runNetworkSmoke
};
