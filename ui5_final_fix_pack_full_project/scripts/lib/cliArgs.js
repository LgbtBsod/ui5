function argValue(argv, flag, fallback) {
  const args = Array.isArray(argv) ? argv : [];
  const index = args.indexOf(flag);
  if (index < 0) return fallback;
  return args[index + 1];
}

function hasArg(argv, flag) {
  const args = Array.isArray(argv) ? argv : [];
  return args.includes(flag);
}

module.exports = {
  argValue,
  hasArg
};
