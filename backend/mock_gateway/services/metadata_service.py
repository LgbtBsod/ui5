"""
Service for handling Metadata operations.
Extracted from gateway_save_api to adhere to Single Responsibility Principle.
"""
import json
import logging
from typing import Any, Dict, List, Optional

from fastapi import HTTPException, status

logger = logging.getLogger(__name__)


class MetadataService:
    """
    Service responsible for fetching, caching, and processing entity metadata.
    
    Responsibilities:
    - Retrieve metadata from external gateway
    - Parse and validate metadata structure
    - Handle metadata caching strategies
    - Transform metadata for UI consumption
    """

    def __init__(self, gateway_client: Any):
        """
        Initialize MetadataService.

        Args:
            gateway_client: Client for external gateway communication
        """
        self.gateway_client = gateway_client
        self._metadata_cache: Dict[str, Any] = {}

    async def get_entity_metadata(
        self, 
        entity_name: str, 
        use_cache: bool = True
    ) -> Dict[str, Any]:
        """
        Retrieve metadata for a specific entity.

        Args:
            entity_name: Name of the entity to fetch metadata for
            use_cache: Whether to use cached metadata if available

        Returns:
            Dictionary containing entity metadata

        Raises:
            HTTPException: If metadata cannot be retrieved or is invalid
        """
        cache_key = f"metadata:{entity_name}"

        if use_cache and cache_key in self._metadata_cache:
            logger.debug(f"Cache hit for metadata: {entity_name}")
            return self._metadata_cache[cache_key]

        try:
            raw_metadata = await self._fetch_from_gateway(entity_name)
            processed_metadata = self._process_metadata(raw_metadata, entity_name)
            
            if use_cache:
                self._metadata_cache[cache_key] = processed_metadata
                
            return processed_metadata
            
        except Exception as e:
            logger.error(f"Failed to fetch metadata for {entity_name}: {str(e)}")
            raise HTTPException(
                status_code=status.HTTP_502_BAD_GATEWAY,
                detail=f"Failed to retrieve metadata for {entity_name}"
            )

    async def _fetch_from_gateway(self, entity_name: str) -> Dict[str, Any]:
        """
        Fetch raw metadata from external gateway.

        Args:
            entity_name: Name of the entity

        Returns:
            Raw metadata dictionary
        """
        # Placeholder for actual gateway call
        # In real implementation: await self.gateway_client.get(f"/metadata/{entity_name}")
        logger.info(f"Fetching metadata for {entity_name} from gateway")
        return {"entity": entity_name, "fields": []}

    def _process_metadata(
        self, 
        raw_data: Dict[str, Any], 
        entity_name: str
    ) -> Dict[str, Any]:
        """
        Process and validate raw metadata.

        Args:
            raw_data: Raw metadata from gateway
            entity_name: Name of the entity for context

        Returns:
            Processed and validated metadata

        Raises:
            HTTPException: If metadata structure is invalid
        """
        if not raw_data:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Metadata not found for {entity_name}"
            )

        if "entity" not in raw_data:
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail="Invalid metadata structure: missing 'entity' field"
            )

        # Normalize field names and types
        processed = {
            "entity_name": raw_data["entity"],
            "fields": self._normalize_fields(raw_data.get("fields", [])),
            "actions": raw_data.get("actions", []),
            "navigation": raw_data.get("navigation", [])
        }

        return processed

    def _normalize_fields(self, fields: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Normalize field definitions to standard format.

        Args:
            fields: List of raw field definitions

        Returns:
            List of normalized field definitions
        """
        normalized = []
        for field in fields:
            if not isinstance(field, dict) or "name" not in field:
                continue
                
            normalized_field = {
                "name": field["name"],
                "type": field.get("type", "Edm.String"),
                "nullable": field.get("nullable", True),
                "max_length": field.get("maxLength"),
                "is_key": field.get("key", False),
                "label": field.get("label", field["name"])
            }
            normalized.append(normalized_field)
            
        return normalized

    def invalidate_cache(self, entity_name: Optional[str] = None) -> None:
        """
        Invalidate metadata cache.

        Args:
            entity_name: Specific entity to invalidate, or None for full cache clear
        """
        if entity_name:
            cache_key = f"metadata:{entity_name}"
            if cache_key in self._metadata_cache:
                del self._metadata_cache[cache_key]
                logger.info(f"Cache invalidated for {entity_name}")
        else:
            self._metadata_cache.clear()
            logger.info("Full metadata cache invalidated")
