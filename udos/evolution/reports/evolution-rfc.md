# RFC-20260303-ARCH-02

Title: Introduce LockResultHandlers as canonical lock transition handler

Reason:
architectural drift detected (4 signals), top anti-pattern: controller calling domain helper directly (2).

Proposal:
- extract repeated lock transition handling into domain/platform/LockResultHandlers
- run mission MISSION-CONTROLLER-THINNING to reduce controller LOC
- propose policy update for controller complexity trend alerting

Expected impact:
- duplication -8
- architectureScore +3
- controller complexity trend normalization

Governance note:
- Evolution Engine does not modify runtime code or constitution automatically.
- Output is advisory: RFC + mission + policy-change proposals only.
