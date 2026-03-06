function printViolations(title, violations) {
  if (!violations.length) {
    console.log(`PASS ${title}`);
    return;
  }
  console.error(`FAIL ${title}`);
  violations.forEach((v) => {
    const line = v.line ? `:${v.line}` : '';
    console.error(`- ${v.file}${line} ${v.message || v.rule || ''}`.trim());
  });
}

function emitJson(payload) {
  if (process.env.REPORT_JSON === '1') {
    console.log(JSON.stringify(payload, null, 2));
  }
}

module.exports = { printViolations, emitJson };
