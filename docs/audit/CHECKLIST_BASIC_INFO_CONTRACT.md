# Checklist Basic Info Contract

`ChecklistBasicInfoSet` is a valid standalone service entity.

It must remain separate from `ChecklistRootSet` because:

- basic info is backed by its own table/CDS source
- the service contract already treats it as an independent read model
- frontend detail loading expects a dedicated basic-info payload beside root, checks, barriers, and attachments

## Current Local Sample State

`ChecklistBasicInfoSet` must remain a CDS reference entity and should be served by generated `SEGW/SADL` code.

That means:

- do not implement manual read logic for it in `DPC_EXT`
- do not add a custom DAO/service just to fetch basic info
- do not route it through `ChecklistRootSet` handlers either

The local repo should treat `ChecklistBasicInfoSet` as part of the generated Gateway contract, not as a hand-written read model.

Metadata expectations for the local sample:

- `ChecklistBasicInfoSet` stays addressable
- `sap:requires-filter="true"` stays enabled
- `sap:updatable="false"` because writes do not go through direct entity updates
- frontend reads it through its own entity set, not through `ChecklistRootSet`

## Correct Next Step

Keep the entity contract aligned with the real CDS-backed productive source and let `SADL` resolve the read path from the CDS reference.

If you need custom behavior around it, do it only in places that do not replace generated read access, for example:

- authorization checks that wrap the generated flow
- mapping/contract documentation
- frontend adapter cleanup

Do not:

- remove `ChecklistBasicInfoSet`
- fold basic-info fields into `ChecklistRootSet`
- hand-code data retrieval that productive SAP already gets from CDS/SADL
