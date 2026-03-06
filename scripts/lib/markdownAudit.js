function renderAuditMarkdown(config) {
  const title = config && config.title ? String(config.title) : 'Audit Report';
  const summary = Array.isArray(config && config.summary) ? config.summary : [];
  const sections = Array.isArray(config && config.sections) ? config.sections : [];
  const lines = [`# ${title}`, ''];

  summary.forEach((line) => lines.push(`- ${line}`));
  lines.push('');

  sections.forEach((section) => {
    lines.push(`## ${section.title}`);
    (Array.isArray(section.lines) ? section.lines : ['- none']).forEach((line) => lines.push(line));
    lines.push('');
  });

  return lines.join('\n') + '\n';
}

function renderSummaryEntries(entries) {
  return (Array.isArray(entries) ? entries : []).map((entry) => {
    const item = entry || {};
    return `${item.label || '-'}: ${item.value}`;
  });
}

function renderListEntries(items, formatter, fallbackLine) {
  const list = Array.isArray(items) ? items : [];
  if (!list.length) {
    return [String(fallbackLine || '- none')];
  }
  return list.map((item, index) => {
    if (typeof formatter === 'function') {
      return formatter(item, index);
    }
    return `- ${String(item)}`;
  });
}

module.exports = {
  renderAuditMarkdown,
  renderListEntries,
  renderSummaryEntries
};
