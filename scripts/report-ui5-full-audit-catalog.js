#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const catalog = {
  generatedAt: new Date().toISOString(),
  ui5Baseline: '1.71.28',
  scope: 'All APIs and implicit runtime surfaces for libraries used by this application shell (sap.m, sap.f, sap.ui.table, sap.ui.comp, sap.ui.core).',
  libraries: {
    'sap.m': {
      controls: [
        'Button','Input','SearchField','Select','ComboBox','MultiComboBox','DatePicker','TimePicker','Switch','CheckBox','RadioButton',
        'SegmentedButton','MenuButton','OverflowToolbar','Toolbar','Table','List','ColumnListItem','ObjectStatus','ObjectIdentifier','MessageStrip',
        'Dialog','Popover','ResponsivePopover','ActionSheet','BusyDialog','SelectDialog','ViewSettingsDialog','Token','Tokenizer','UploadCollection',
        'Wizard','Carousel','Panel','HBox','VBox','Page','NavContainer'
      ],
      implicitRuntimeSurfaces: ['MessageBox','ViewSettingsDialog','SelectDialog','ResponsivePopover','ActionSheet','BusyDialog']
    },
    'sap.f': {
      controls: ['FlexibleColumnLayout','DynamicPage','GridList','Card','ShellBar']
    },
    'sap.ui.table': {
      controls: ['Table','TreeTable','AnalyticalTable','Column']
    },
    'sap.ui.comp': {
      controls: ['SmartTable','SmartFilterBar','FilterBar','ValueHelpDialog','P13nDialog']
    },
    'sap.ui.core': {
      controls: ['Fragment','Item','CustomData','InvisibleText','Icon']
    }
  },
  polishPolicy: {
    resetLayer: 'Wave 61 + Wave 62 CSS reset/skin overlays',
    styleTokens: ['radius','glass-bg','glass-border','focus-ring','motion','opacity'],
    asyncSafety: ['pending guard','busy state','model-first reconciliation'],
    accessibility: ['focus-visible','reduced-motion','contrast-safe states']
  }
};

const outJson = path.join(process.cwd(), 'docs', 'ux', 'ui5-full-audit-catalog.json');
fs.writeFileSync(outJson, JSON.stringify(catalog, null, 2));

const md = [];
md.push('# UI5 1.71.28 Full audit catalog (libraries + implicit surfaces)');
md.push('');
md.push(`Generated: ${catalog.generatedAt}`);
md.push('');
md.push(`Scope: ${catalog.scope}`);
md.push('');
Object.entries(catalog.libraries).forEach(([lib, data]) => {
  md.push(`## ${lib}`);
  md.push('');
  md.push('### Controls/APIs to skin');
  data.controls.forEach((c) => md.push(`- ${lib}.${c}`));
  if (data.implicitRuntimeSurfaces) {
    md.push('');
    md.push('### Implicit runtime surfaces');
    data.implicitRuntimeSurfaces.forEach((c) => md.push(`- ${lib}.${c}`));
  }
  md.push('');
});
md.push('## Polish policy');
md.push(`- Reset layer: ${catalog.polishPolicy.resetLayer}`);
md.push(`- Tokens: ${catalog.polishPolicy.styleTokens.join(', ')}`);
md.push(`- Async safety: ${catalog.polishPolicy.asyncSafety.join(', ')}`);
md.push(`- Accessibility: ${catalog.polishPolicy.accessibility.join(', ')}`);

const outMd = path.join(process.cwd(), 'docs', 'ux', 'ui5-full-audit-catalog.md');
fs.writeFileSync(outMd, md.join('\n'));
console.log('Generated docs/ux/ui5-full-audit-catalog.json and .md');
