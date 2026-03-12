sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeOptionBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeHandlerBootstrap"
], function (ComponentRuntimeOptionBuilder, ComponentRuntimeHandlerBootstrap) {
    "use strict";

    return {
        buildListenerRuntimeOptions: ComponentRuntimeOptionBuilder.buildListenerRuntimeOptions,
        buildLockRuntimeOptions: ComponentRuntimeOptionBuilder.buildLockRuntimeOptions,
        buildManagerRuntimeOptions: ComponentRuntimeOptionBuilder.buildManagerRuntimeOptions,
        createForceReadOnlyHandler: ComponentRuntimeHandlerBootstrap.createForceReadOnlyHandler,
        createGuardedSave: ComponentRuntimeHandlerBootstrap.createGuardedSave,
        registerCrossTabRuntime: ComponentRuntimeHandlerBootstrap.registerCrossTabRuntime,
        registerDefaultHandlers: ComponentRuntimeHandlerBootstrap.registerDefaultHandlers
    };
});
