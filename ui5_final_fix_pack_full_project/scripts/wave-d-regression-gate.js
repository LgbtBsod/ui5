#!/usr/bin/env node
const { runWaveRegressionGate, WAVE_REGRESSION_PROFILES } = require('./wave-regression-shared');
runWaveRegressionGate(Object.assign({ reportPath: process.argv[2] }, WAVE_REGRESSION_PROFILES.waveD));
