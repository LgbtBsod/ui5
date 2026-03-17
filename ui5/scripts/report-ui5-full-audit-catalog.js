#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { writeJsonAndMarkdown } = require('./lib/reportWriteRuntime');

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

let mdText = '# UI5 1.71.28 Full audit catalog (libraries + implicit surfaces)\n\n';
mdText += `Generated: ${catalog.generatedAt}\n\n`;
mdText += `Scope: ${catalog.scope}\n\n`;
Object.entries(catalog.libraries).forEach(([lib, data]) => {
  mdText += `## ${lib}\n\n`;
  mdText += '### Controls/APIs to skin\n';
  data.controls.forEach((c) => { mdText += `- ${lib}.${c}\n`; });
  if (data.implicitRuntimeSurfaces) {
    mdText += '\n### Implicit runtime surfaces\n';
    data.implicitRuntimeSurfaces.forEach((c) => { mdText += `- ${lib}.${c}\n`; });
  }
  mdText += '\n';
});
mdText += '## Polish policy\n';
mdText += `- Reset layer: ${catalog.polishPolicy.resetLayer}\n`;
mdText += `- Tokens: ${catalog.polishPolicy.styleTokens.join(', ')}\n`;
mdText += `- Async safety: ${catalog.polishPolicy.asyncSafety.join(', ')}\n`;
mdText += `- Accessibility: ${catalog.polishPolicy.accessibility.join(', ')}\n`;

const outMd = path.join(process.cwd(), 'docs', 'ux', 'ui5-full-audit-catalog.md');
writeJsonAndMarkdown(outJson, catalog, outMd, mdText.split('\n'));
console.log('Generated docs/ux/ui5-full-audit-catalog.json and .md');
