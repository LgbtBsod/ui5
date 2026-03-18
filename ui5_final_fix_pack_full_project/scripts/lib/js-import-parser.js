function parseImports(text) {
  const deps = [];
  const regexes = [
    /sap\.ui\.define\s*\(\s*\[([\s\S]*?)\]/g,
    /require\(\s*['"]([^'"]+)['"]\s*\)/g,
    /import\s+[^'";]*['"]([^'"]+)['"]/g
  ];
  const def = /['"]([^'"]+)['"]/g;

  let arr;
  while ((arr = regexes[0].exec(text)) !== null) {
    let m;
    while ((m = def.exec(arr[1])) !== null) deps.push(m[1]);
  }
  for (const rx of regexes.slice(1)) {
    let m;
    while ((m = rx.exec(text)) !== null) deps.push(m[1]);
  }
  return deps;
}

module.exports = { parseImports };
