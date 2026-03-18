#!/usr/bin/env node

const { canonicalizeUrl, signature } = require('./network-signature');

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

(function main() {
const a = canonicalizeUrl('https://host/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistSearchSet?$skip=0&$top=10&sap-language=EN&$filter=HasFailedChecks%20eq%20true');
const b = canonicalizeUrl('/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistSearchSet?sap-ui-debug=true&$top=10&$filter=HasFailedChecks%20%20eq%20%20true&$skip=0');
  assert(a === b, 'canonicalizeUrl should normalize noisy params and preserve semantic equality');

  const s1 = signature({ method: 'get', url: '/x?$top=10&$skip=0', body: '' });
  const s2 = signature({ method: 'GET', url: '/x?$skip=0&$top=10' });
  assert(s1 === s2, 'signature should be stable for query-order permutations');

  const s3 = signature({ method: 'POST', url: '/x', body: '  GET ChecklistSearchSet?$filter=HasFailedChecks   eq true  ' });
  const s4 = signature({ method: 'POST', url: '/x', body: 'GET ChecklistSearchSet?$filter=HasFailedChecks eq true' });
  assert(s3 === s4, 'signature should normalize whitespace in body');

  console.log('PASS network-signature-fixture');
  process.exit(0);
})();
