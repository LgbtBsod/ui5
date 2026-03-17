#!/usr/bin/env node

const { runGate } = require('./lib/gateRunner');
const { scanPatterns } = require('./lib/patternScan');

const FEEDBACK_UI_ALLOW = new Set([
  'service/framework/EffectApplier.js',
  'app/service/framework/EffectApplier.js',
  'service/framework/EffectDialogFeedbackRuntime.js',
  'app/service/framework/EffectDialogFeedbackRuntime.js',
  'service/framework/EffectToastRuntime.js',
  'app/service/framework/EffectToastRuntime.js'
]);
const POLICY_ALLOW = new Set([
  'service/framework/FeedbackPolicy.js',
  'app/service/framework/FeedbackPolicy.js',
  'service/backend/GatewayErrorNormalizer.js',
  'app/service/backend/GatewayErrorNormalizer.js',
  'service/backend/GatewayClient.js',
  'app/service/backend/GatewayClient.js'
]);

runGate({
  name: 'feedback-unification-gate',
  include: ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'util/**/*.js', 'model/**/*.js', 'ports/**/*.js'],
  check: ({ file, text }) => {
    const violations = [];
    if (!FEEDBACK_UI_ALLOW.has(file)) {
      violations.push(...scanPatterns(file, text, [
        { id: 'feedback', regex: /MessageBox|MessageToast/, message: 'UI feedback allowed only in sanctioned effect feedback boundary' }
      ]));
    }
    if (!POLICY_ALLOW.has(file)) {
      violations.push(...scanPatterns(file, text, [
        { id: 'odata-error', regex: /responseText\s*[:=]|oError\.response|error\.response/i, message: 'Manual OData error parsing outside FeedbackPolicy' }
      ]));
    }
    return violations;
  }
});
