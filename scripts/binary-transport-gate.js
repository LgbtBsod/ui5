#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const issues = [];

function read(relPath) {
  return fs.readFileSync(path.join(ROOT, relPath), "utf8");
}

function add(issue) {
  issues.push(issue);
}

const keyNormalizer = read("app/service/shared/ODataKeyNormalizer.js");
const odataUtils = read("app/infra/adapters/shared/ODataAdapterUtils.js");
const detailRead = read("app/infra/adapters/shared/ODataChecklistReadRuntime.js");
const permissionRuntime = read("app/infra/adapters/shared/ODataChecklistPermissionRuntime.js");
const lastChangeAdapter = read("app/infra/adapters/LastChangeSetAdapter.js");
const metadata = read("app/localService/metadata.xml");

if (!/normalizeBinaryKey/.test(keyNormalizer)) {
  add("app/service/shared/ODataKeyNormalizer.js must keep canonical binary-key to hex normalization");
}

if (!/ODataUtils\.formatValue/.test(odataUtils)) {
  add("app/infra/adapters/shared/ODataAdapterUtils.js must use standard ODataUtils.formatValue for typed literal formatting");
}

if (/formatBinaryLiteral|formatKeyPredicate|formatEqFilter/.test(keyNormalizer)) {
  add("app/service/shared/ODataKeyNormalizer.js must not reimplement standard OData literal or predicate formatting");
}

if (!/name: "DB_KEY"/.test(detailRead)) {
  add("app/infra/adapters/shared/ODataChecklistReadRuntime.js must address ChecklistRootSet via named DB_KEY binary predicate");
}

if (!/normalizeBinaryKey/.test(detailRead)) {
  add("app/infra/adapters/shared/ODataChecklistReadRuntime.js must normalize binary root ids before canonical reads");
}

if (!/buildEqFilter\(oFilterContract\.property,\s*ODataKeyNormalizer\.normalizeBinaryKey\(sDbKey\),\s*oFilterContract\.type\)/.test(detailRead.replace(/\r?\n/g, " "))) {
  add("app/infra/adapters/shared/ODataChecklistReadRuntime.js must pass normalized binary keys into standard typed filter generation");
}

if (fs.existsSync(path.join(ROOT, "app/infra/adapters/shared/AttachmentRepoRuntime.js"))) {
  add("app/infra/adapters/shared/AttachmentRepoRuntime.js must stay deleted after media-upload boundary cleanup");
}

if (!/name: "DB_KEY"/.test(permissionRuntime) || !/ODataKeyContracts\.TYPES\.DB_KEY/.test(permissionRuntime)) {
  add("app/infra/adapters/shared/ODataChecklistPermissionRuntime.js must read canonical permission entity via DB_KEY binary key predicate");
}

if (!/name: "DB_KEY"/.test(lastChangeAdapter) || !/ODataKeyContracts\.TYPES\.DB_KEY/.test(lastChangeAdapter)) {
  add("app/infra/adapters/LastChangeSetAdapter.js must read last-change entity via DB_KEY binary key predicate");
}

["ChecklistCheck", "ChecklistBarrier", "Attachment"].forEach((entityName) => {
  const block = (metadata.match(new RegExp(`<EntityType Name="${entityName}"[\\s\\S]*?<\\/EntityType>`)) || [""])[0];
  if (!/Name="DB_KEY" Type="Edm.String"/.test(block)) {
    add(`app/localService/metadata.xml: ${entityName}.DB_KEY must be Edm.String (mock gateway serializes it as bin-to-hex string32)`);
  }
  if (!/Name="PARENT_KEY" Type="Edm.String"/.test(block)) {
    add(`app/localService/metadata.xml: ${entityName}.PARENT_KEY must be Edm.String (mock gateway serializes it as bin-to-hex string32)`);
  }
});

if (issues.length) {
  console.error(["Binary transport gate failed:", ...issues.map((issue) => " - " + issue)].join("\n"));
  process.exit(1);
}

console.log("Binary transport gate: OK");
