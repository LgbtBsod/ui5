# Architecture Air Traffic Rules

## Rule 1
Только один HIGH-risk flight одновременно (exclusive window).

## Rule 2
PR без flight plan (`flightPlanId`) — reject.

## Rule 3
PR без preflight simulation evidence (`hasPreflight=true`) — reject.
