const { createFileExistenceSmoke } = require('./smoke-file-check');

async function runLockSmoke() {
  return createFileExistenceSmoke(
    'lock',
    ['app/infra/adapters/LockAdapter.js', 'app/service/runtime/component/ComponentLockEventsRuntime.js'],
    'canonical lock adapter and runtime owners exist'
  );
}

async function runNavigationSmoke() {
  return createFileExistenceSmoke(
    'nav',
    ['app/infra/navigation/WorkspaceRouteNavigation.js', 'app/service/framework/ActionDispatcher.js'],
    'navigation and action dispatch owners exist'
  );
}

async function runNetworkSmoke() {
  return createFileExistenceSmoke(
    'network',
    ['app/service/backend/GatewayClient.js', 'app/service/framework/ComponentBootstrap.js', 'app/service/features/detail/runtime/AttachmentGatewayRuntime.js'],
    'gateway transport, component bootstrap, and attachment upload owners exist'
  );
}

module.exports = {
  runLockSmoke,
  runNavigationSmoke,
  runNetworkSmoke
};
