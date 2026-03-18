const FEATURE_REGISTRY = {
  search: 'service/domain/search/',
  detail: 'service/domain/detail/',
  lock: 'service/domain/lock/',
  autosave: 'service/domain/autosave/',
  cache: 'service/domain/cache/',
  dictionary: 'service/domain/dictionary/',
  personSuggest: 'service/domain/person/',
  shared: 'service/domain/shared/'
};

function featureOfFile(file) {
  const f = String(file || '').toLowerCase();
  if (f.includes('/search/')) return 'search';
  if (f.includes('/detail/')) return 'detail';
  if (f.includes('lock')) return 'lock';
  if (f.includes('autosave')) return 'autosave';
  if (f.includes('/cache/')) return 'cache';
  if (f.includes('dict') || f.includes('valuehelp')) return 'dictionary';
  if (f.includes('person') || f.includes('suggest')) return 'personSuggest';
  return 'shared';
}

module.exports = {
  FEATURE_REGISTRY,
  featureOfFile
};
