const fs = require("fs");
const path = require("path");

function read(relPath) {
  return fs.readFileSync(path.join(__dirname, "..", relPath), "utf8");
}

const issues = [];
const metadata = read("app/localService/metadata.xml");
const lockAdapter = read("app/infra/adapters/LockAdapter.js");
const dpc = read("backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap");
const shellState = read("app/service/features/shell/runtime/ShellStateRuntime.js");
const modelConstants = read("app/constants/ModelConstants.js");
const modelFactory = read("app/model/ModelFactory.js");

if (!/FunctionImport Name="LockAcquire"[\s\S]*?<Parameter Name="DB_KEY" Type="Edm.Binary"/.test(metadata)) {
  issues.push("app/localService/metadata.xml: LockAcquire must expose DB_KEY as canonical Edm.Binary lock key");
}

if (!/FunctionImport Name="LockHeartbeat"[\s\S]*?<Parameter Name="DB_KEY" Type="Edm.Binary"/.test(metadata)) {
  issues.push("app/localService/metadata.xml: LockHeartbeat must expose DB_KEY as canonical Edm.Binary lock key");
}

if (!/FunctionImport Name="LockRelease"[\s\S]*?<Parameter Name="DB_KEY" Type="Edm.Binary"/.test(metadata)) {
  issues.push("app/localService/metadata.xml: LockRelease must expose DB_KEY as canonical Edm.Binary lock key");
}

if (!/FunctionImport Name="CopyChecklist"[\s\S]*?<Parameter Name="DB_KEY" Type="Edm.Binary"/.test(metadata)) {
  issues.push("app/localService/metadata.xml: CopyChecklist must expose DB_KEY as canonical Edm.Binary root key");
}

if (/ObjectUuid/.test(lockAdapter)) {
  issues.push("app/infra/adapters/LockAdapter.js: frontend canonical lock surface must not use ObjectUuid");
}
const componentLockRuntime = read("app/service/runtime/component/ComponentLockEventsRuntime.js");
if (/\bObjectUuid\b|\bobjectUuid\b/.test(componentLockRuntime)) {
  issues.push("app/service/runtime/component/ComponentLockEventsRuntime.js: lock runtime must not consume ObjectUuid aliases");
}
if (/rootId/.test(lockAdapter) && !/dbKey \|\| mArgs\.DB_KEY \|\| mArgs\.rootId/.test(lockAdapter)) {
  issues.push("app/infra/adapters/LockAdapter.js: lock adapter must expose dbKey as canonical frontend key and keep rootId only as compatibility fallback");
}
if (/currentRootKey/.test(shellState) || /currentRootKey/.test(modelFactory) || /SHELL_CURRENT_ROOT_KEY/.test(modelConstants)) {
  issues.push("shell lock state still uses currentRootKey naming instead of canonical currentChecklistDbKey");
}

const mutationRuntime = read("app/infra/adapters/shared/ODataChecklistMutationRuntime.js");
if (/SourceUuid/.test(mutationRuntime)) {
  issues.push("app/infra/adapters/shared/ODataChecklistMutationRuntime.js: copy contract must not use SourceUuid on the frontend canonical surface");
}

["lockacquire_create_entity", "lockheartbeat_create_entity", "lockrelease_create_entity"].forEach((methodName) => {
  const methodMatch = dpc.match(new RegExp(`METHOD ${methodName}\\.[\\s\\S]*?ENDMETHOD\\.`, "i"));
  const methodBody = methodMatch ? methodMatch[0] : "";
  if (!methodBody) {
    issues.push(`backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap: method ${methodName} not found`);
    return;
  }
  if (!/ls_req-db_key/.test(methodBody)) {
    issues.push(`backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap: ${methodName} must read canonical DB_KEY`);
  }
});

const mpc = read("backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap");
if (!/CopyChecklist[\s\S]*?iv_name = 'DB_KEY'\s+iv_edm_type = 'Binary'/.test(mpc)) {
  issues.push("backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap: CopyChecklist must define DB_KEY as Binary");
}

["LockAcquire", "LockHeartbeat", "LockRelease"].forEach((name) => {
  if (!new RegExp(`${name}[\\s\\S]*?iv_name = 'DB_KEY'\\s+iv_edm_type = 'Binary'`).test(mpc)) {
    issues.push(`backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap: ${name} must define DB_KEY as Binary`);
  }
  if (!new RegExp(`${name}[\\s\\S]*?iv_name = 'SessionGuid'\\s+iv_edm_type = 'String'`).test(mpc)) {
    issues.push(`backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap: ${name} must define SessionGuid as String`);
  }
});

if (!/CopyChecklist[\s\S]*?iv_name = 'SessionGuid'\s+iv_edm_type = 'String'/.test(mpc)) {
  issues.push("backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap: CopyChecklist must define SessionGuid as String");
}

const fallbackCount = (dpc.match(/ObjectUuid/g) || []).length;
if (fallbackCount > 0) {
  issues.push("backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap: ObjectUuid compatibility alias must stay out of active DPC runtime and remain boundary-only");
}

if (issues.length) {
  console.error("Lock contract naming gate failed:");
  issues.forEach((issue) => console.error(" - " + issue));
  process.exit(1);
}

console.log("Lock contract naming gate: OK");
