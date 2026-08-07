"""
Service for handling Report Export operations.
Extracted from gateway_save_api to adhere to Single Responsibility Principle.
"""

import logging
from typing import Any, Dict, List, Optional

from sqlalchemy.orm import Session

from models import ChecklistRoot, DictionaryItem

logger = logging.getLogger(__name__)


class ReportExportService:
    """
    Service responsible for generating report exports.

    Responsibilities:
    - Filter and select checklist roots based on criteria
    - Transform root data into export format
    - Enrich export rows with dictionary texts
    - Apply export limits and validation
    """

    def __init__(self, db: Session):
        """
        Initialize ReportExportService.

        Args:
            db: Database session
        """
        self.db = db

    def get_roots_for_export(
        self,
        root_keys: Optional[List[str]] = None,
        selection_mode: str = "",
        search_contract: Optional[Dict[str, Any]] = None,
        export_limit: int = 1000,
    ) -> List[ChecklistRoot]:
        """
        Get checklist roots for export based on selection criteria.

        Args:
            root_keys: Specific root keys to export
            selection_mode: Selection mode ('all' or specific)
            search_contract: Search contract for filtering
            export_limit: Maximum number of roots to return

        Returns:
            List of ChecklistRoot entities

        Raises:
            ValueError: If export limit exceeded
        """
        if not root_keys and selection_mode != "all":
            return []

        roots: List[ChecklistRoot] = []

        if root_keys:
            roots = self._get_roots_by_keys(root_keys)
            if len(root_keys) > export_limit:
                raise ValueError("Selected export exceeds configured limit")
        elif selection_mode == "all":
            roots = self._get_roots_by_search(search_contract, export_limit)

        return roots

    def _get_roots_by_keys(self, root_keys: List[str]) -> List[ChecklistRoot]:
        """
        Retrieve roots by their keys.

        Args:
            root_keys: List of root keys

        Returns:
            List of ChecklistRoot entities
        """
        from api.gateway_core import _entity_key
        from api.gateway_operations import _normalize_hex_key

        roots_by_key: Dict[str, ChecklistRoot] = {}

        for key in root_keys:
            normalized_key = _normalize_hex_key(key)
            if not normalized_key:
                continue

            root = (
                self.db.query(ChecklistRoot)
                .filter(ChecklistRoot.id == _entity_key(str(normalized_key)), ChecklistRoot.is_deleted.isnot(True))
                .first()
            )
            if root:
                roots_by_key[str(key)] = root

        return [roots_by_key[key] for key in root_keys if key in roots_by_key]

    def _get_roots_by_search(self, search_contract: Optional[Dict[str, Any]], limit: int) -> List[ChecklistRoot]:
        """
        Retrieve roots by search contract.

        Args:
            search_contract: Search criteria
            limit: Maximum number of roots

        Returns:
            List of ChecklistRoot entities
        """
        from api.gateway_serializers import _search_contract_matches

        all_roots = self.db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)).all()

        if not search_contract:
            return all_roots[:limit]

        return [root for root in all_roots if _search_contract_matches(root, search_contract, self.db)][:limit]

    def load_dictionary_texts(self) -> Dict[str, Dict[str, str]]:
        """
        Load dictionary texts for checks and barriers.

        This preloads all dictionary texts in a single query to avoid
        N+1 query problem during export generation.

        Returns:
            Dictionary with 'check' and 'barrier' keys containing text mappings
        """
        check_texts = {
            item.key: item.text for item in self.db.query(DictionaryItem).filter(DictionaryItem.domain == "CHECK").all()
        }

        barrier_texts = {
            item.key: item.text
            for item in self.db.query(DictionaryItem).filter(DictionaryItem.domain == "BARRIER").all()
        }

        return {"check": check_texts, "barrier": barrier_texts}

    def build_export_rows(
        self,
        roots: List[ChecklistRoot],
        entity_type: str = "screen",
        dictionary_texts: Optional[Dict[str, Dict[str, str]]] = None,
    ) -> List[Dict[str, Any]]:
        """
        Build export rows from checklist roots.

        Args:
            roots: List of ChecklistRoot entities
            entity_type: Type of entity to export ('screen', 'check', 'barrier', 'all')
            dictionary_texts: Preloaded dictionary texts

        Returns:
            List of export row dictionaries
        """
        from api.gateway_serializers import _to_check, _to_barrier, _to_search

        if dictionary_texts is None:
            dictionary_texts = self.load_dictionary_texts()

        check_dict_texts = dictionary_texts.get("check", {})
        barrier_dict_texts = dictionary_texts.get("barrier", {})

        rows: List[Dict[str, Any]] = []

        for root in roots:
            base = _to_search(root, db=self.db)
            checks = [_to_check(c) for c in root.checks]
            barriers = [_to_barrier(b) for b in root.barriers]

            base_row = self._build_base_row(base)

            if entity_type == "screen":
                rows.append({**base_row, "ItemType": "ROOT", "Num": 0, "Text": "", "Comment": "", "Result": None})
                continue

            if entity_type == "check":
                rows.extend(self._build_check_rows(base_row, checks, check_dict_texts))
                continue

            if entity_type == "barrier":
                rows.extend(self._build_barrier_rows(base_row, barriers, barrier_dict_texts))
                continue

            # Default: export both checks and barriers
            rows.extend(self._build_check_rows(base_row, checks, check_dict_texts))
            rows.extend(self._build_barrier_rows(base_row, barriers, barrier_dict_texts))

        return rows

    def _build_base_row(self, base: Dict[str, Any]) -> Dict[str, Any]:
        """Build base row from search data."""
        return {
            "DB_KEY": base["DB_KEY"],
            "Id": base["Id"],
            "Lpc": base.get("Lpc", ""),
            "LpcText": base.get("LpcText", ""),
            "Profession": base.get("Profession", ""),
            "ProfessionText": base.get("ProfessionText", ""),
            "LocationKey": base.get("LocationKey", ""),
            "Status": base.get("Status", ""),
            "EquipName": base.get("EquipName", ""),
            "ChangedOn": base.get("ChangedOn", ""),
            "DateCheck": base["DateCheck"],
        }

    def _build_check_rows(
        self, base_row: Dict[str, Any], checks: List[Dict[str, Any]], dict_texts: Dict[str, str]
    ) -> List[Dict[str, Any]]:
        """Build rows for check items."""
        rows = []
        for c in checks:
            key = str(c["ChecksNum"])
            rows.append(
                {
                    **base_row,
                    "ItemType": "CHECK",
                    "Num": c["ChecksNum"],
                    "Text": dict_texts.get(key, c.get("Text", "")),
                    "Comment": c["Comment"],
                    "Result": c["Result"],
                }
            )
        return rows

    def _build_barrier_rows(
        self, base_row: Dict[str, Any], barriers: List[Dict[str, Any]], dict_texts: Dict[str, str]
    ) -> List[Dict[str, Any]]:
        """Build rows for barrier items."""
        rows = []
        for b in barriers:
            key = str(b["BarriersNum"])
            rows.append(
                {
                    "DB_KEY": base_row["DB_KEY"],
                    "Id": base_row["Id"],
                    "Lpc": base_row.get("Lpc", ""),
                    "LpcText": base_row.get("LpcText", ""),
                    "Profession": base_row.get("Profession", ""),
                    "ProfessionText": base_row.get("ProfessionText", ""),
                    "DateCheck": base_row["DateCheck"],
                    "ItemType": "BARRIER",
                    "Num": b["BarriersNum"],
                    "Text": dict_texts.get(key, b.get("Text", "")),
                    "Comment": b["Comment"],
                    "Result": b["Result"],
                }
            )
        return rows

    def validate_export_limit(self, rows: List[Dict[str, Any]], limit: int) -> bool:
        """
        Validate that export does not exceed limit.

        Args:
            rows: List of export rows
            limit: Maximum allowed rows

        Returns:
            True if valid, False otherwise
        """
        return len(rows) <= limit
