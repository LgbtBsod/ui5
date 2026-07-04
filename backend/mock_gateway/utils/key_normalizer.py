from __future__ import annotations

import uuid


def hex_to_storage_key(value: str) -> str:
    """Normalize a RAW16/Edm.Binary key to its canonical storage form.

    SAP/CDS convention (see README_ODATA.md): DB_KEY and other checklist keys are
    RAW16 on the SAP side, exposed to the frontend as a 32-char uppercase hex string
    via BINTOHEX - the frontend never parses or reformats the key, it just echoes
    back whatever hex32 string it was given. This mock stores entity primary keys as
    standard dashed UUID strings, so any hex32 (or already-dashed) key crossing the
    wire boundary must be normalized to that one canonical form - here, the single
    place this conversion happens, mirroring what SADL/CDS do standardly in real SAP.

    Returns the input unchanged if it isn't a recognizable 32-hex-char key (e.g. a
    business identifier like checklist_id, or the "__CREATE" sentinel).
    """
    raw = str(value or "").strip()
    if not raw:
        return raw
    hex_candidate = raw.replace("-", "")
    if len(hex_candidate) == 32 and all(c in "0123456789abcdefABCDEF" for c in hex_candidate):
        try:
            return str(uuid.UUID(hex=hex_candidate))
        except ValueError:
            pass
    return raw
