#!/usr/bin/env node
const path = require('path');

const ROOT = path.resolve(__dirname, '..', '..');

module.exports = {
  ROOT,
  stateFile: path.join(ROOT, 'udos', 'history', 'udos-state.json'),
  eventsFile: path.join(ROOT, 'udos', 'history', 'events.log'),
  missionsFile: path.join(ROOT, 'udos', 'history', 'missions.json'),
  defaults: {
    scores: {
      ArchitectureScore: 94,
      AIL: 90,
      PMI: 90,
      ADT: 91,
      DomainCompleteness: 90
    },
    budget: {
      violated: false,
      reasons: []
    },
    queue: [],
    missions: [],
    decisions: [],
    simulations: []
  }
};
