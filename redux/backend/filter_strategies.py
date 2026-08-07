"""Filter evaluation strategies for OData queries.

This module implements the Strategy Pattern to decompose the monolithic
eval_one_filter() function into focused, testable strategy classes.
Each strategy handles a specific type of OData filter expression following
the Single Responsibility Principle.

Refactored from resolvers.py eval_one_filter() which had cyclomatic
complexity of 12. Now each strategy has max complexity of 2-3.
"""
from __future__ import annotations

import re
from abc import ABC, abstractmethod
from typing import Any, Callable, Dict, Optional, Pattern, Tuple, TYPE_CHECKING

from .patterns import Patterns

if TYPE_CHECKING:
    from .resolvers import EntityRow


class FilterStrategy(ABC):
    """Abstract base class for filter strategies."""
    
    @abstractmethod
    def matches(self, row: EntityRow) -> bool:
        """Check if the row matches this filter strategy.
        
        Args:
            row: Entity row to evaluate
            
        Returns:
            True if the row matches the filter, False otherwise
        """
        pass
    
    @abstractmethod
    def can_handle(self, token: str) -> bool:
        """Check if this strategy can handle the given filter token.
        
        Args:
            token: Filter expression token
            
        Returns:
            True if this strategy can handle the token, False otherwise
        """
        pass


class SublocationFilterStrategy(FilterStrategy):
    """Handles location_name_insub('value') filters."""
    
    PATTERN = Patterns.LOCATION_INSUB_RE
    
    def __init__(self, sublocation_resolver: Callable[[str], list[str]]):
        """Initialize with sublocation resolver function.
        
        Args:
            sublocation_resolver: Function to resolve sublocation names
        """
        self.sublocation_resolver = sublocation_resolver
        self._match: Optional[re.Match] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.PATTERN.match(token)
        return self._match is not None
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match:
            return False
        search_term = self._match.group(1)
        location_name = str(row.get("LocationName") or "")
        valid_names = self.sublocation_resolver(search_term)
        return location_name in valid_names


class SubstringFilterStrategy(FilterStrategy):
    """Handles substringof('value', field) filters."""
    
    PATTERN = Patterns.SUBSTRING_OF_RE
    
    def __init__(self):
        self._match: Optional[re.Match] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.PATTERN.match(token)
        return self._match is not None
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match:
            return False
        needle = self._match.group(1).lower()
        field_name = self._match.group(2)
        haystack = str(row.get(field_name) or "").lower()
        return needle in haystack


class ToLowerEqualsFilterStrategy(FilterStrategy):
    """Handles tolower(field) eq 'value' filters."""
    
    PATTERN = Patterns.TOLOWER_EQ_RE
    
    def __init__(self):
        self._match: Optional[re.Match] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.PATTERN.match(token)
        return self._match is not None
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match:
            return False
        field_name = self._match.group(1)
        expected_value = self._match.group(2).lower()
        actual_value = str(row.get(field_name) or "").lower()
        return actual_value == expected_value


class DateCompareFilterStrategy(FilterStrategy):
    """Handles date comparison filters (le, ge)."""
    
    LE_PATTERN = Patterns.FIELD_LE_RE
    GE_PATTERN = Patterns.FIELD_GE_RE
    
    def __init__(self, date_comparator: Callable[[Any, str, str], bool]):
        """Initialize with date comparator function.
        
        Args:
            date_comparator: Function to compare dates
        """
        self.date_comparator = date_comparator
        self._match: Optional[re.Match] = None
        self._operation: Optional[str] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.LE_PATTERN.match(token)
        if self._match:
            self._operation = "le"
            return True
        self._match = self.GE_PATTERN.match(token)
        if self._match:
            self._operation = "ge"
            return True
        return False
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match or not self._operation:
            return False
        field_name = self._match.group(1)
        filter_value = self._match.group(2)
        row_value = row.get(field_name)
        return self.date_comparator(row_value, filter_value, self._operation)


class PkLevelsFilterStrategy(FilterStrategy):
    """Handles PkLevels eq 'value' filters (CSV containment)."""
    
    PATTERN: Pattern[str] = Patterns.PK_LEVELS_EQ_RE
    
    def __init__(self):
        self._match: Optional[re.Match] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.PATTERN.match(token)
        return self._match is not None
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match:
            return False
        csv_value = str(row.get("PkLevels") or "")
        csv_list = [s.strip() for s in csv_value.split(",")]
        target_value = self._match.group(2)
        return target_value in csv_list


class BooleanEqualsFilterStrategy(FilterStrategy):
    """Handles field eq true/false filters."""
    
    PATTERN: Pattern[str] = Patterns.FIELD_BOOL_EQ_RE
    
    def __init__(self):
        self._match: Optional[re.Match] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.PATTERN.match(token)
        return self._match is not None
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match:
            return False
        field_name = self._match.group(1)
        expected_bool = self._match.group(2).lower() == "true"
        actual_value = bool(row.get(field_name))
        return actual_value == expected_bool


class NotEqualsFilterStrategy(FilterStrategy):
    """Handles field ne 'value' filters."""
    
    PATTERN: Pattern[str] = Patterns.FIELD_NE_RE
    
    def __init__(self):
        self._match: Optional[re.Match] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.PATTERN.match(token)
        return self._match is not None
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match:
            return False
        field_name = self._match.group(1)
        expected_value = self._match.group(2)
        actual_value = str(row.get(field_name) or "")
        return actual_value != expected_value


class EqualsFilterStrategy(FilterStrategy):
    """Handles field eq 'value' filters."""
    
    PATTERN: Pattern[str] = Patterns.FIELD_STR_EQ_RE
    
    def __init__(self):
        self._match: Optional[re.Match] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.PATTERN.match(token)
        return self._match is not None
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match:
            return False
        field_name = self._match.group(1)
        expected_value = self._match.group(2)
        actual_value = str(row.get(field_name) or "")
        return actual_value == expected_value


class NullEqualsFilterStrategy(FilterStrategy):
    """Handles field eq null filters."""
    
    PATTERN: Pattern[str] = Patterns.FIELD_NULL_EQ_RE
    
    def __init__(self):
        self._match: Optional[re.Match] = None
    
    def can_handle(self, token: str) -> bool:
        self._match = self.PATTERN.match(token)
        return self._match is not None
    
    def matches(self, row: EntityRow) -> bool:
        if not self._match:
            return False
        field_name = self._match.group(1)
        return row.get(field_name) is None


class FilterStrategyFactory:
    """Factory for creating filter strategies."""
    
    def __init__(
        self, 
        sublocation_resolver: Callable[[str], list[str]], 
        date_comparator: Callable[[Any, str, str], bool]
    ):
        """Initialize factory with resolver functions.
        
        Args:
            sublocation_resolver: Function to resolve sublocation names
            date_comparator: Function to compare dates
        """
        self.strategies: list[FilterStrategy] = [
            SublocationFilterStrategy(sublocation_resolver),
            SubstringFilterStrategy(),
            ToLowerEqualsFilterStrategy(),
            DateCompareFilterStrategy(date_comparator),
            PkLevelsFilterStrategy(),
            BooleanEqualsFilterStrategy(),
            NotEqualsFilterStrategy(),
            EqualsFilterStrategy(),
            NullEqualsFilterStrategy(),
        ]
    
    def get_strategy(self, token: str) -> Optional[FilterStrategy]:
        """Get the appropriate strategy for a filter token.
        
        Args:
            token: Filter expression token
            
        Returns:
            FilterStrategy instance or None if no strategy matches
        """
        for strategy in self.strategies:
            if strategy.can_handle(token):
                return strategy
        return None
    
    def evaluate(self, token: str, row: EntityRow) -> bool:
        """Evaluate a filter token against a row.
        
        Args:
            token: Filter expression token
            row: Entity row to evaluate
            
        Returns:
            True if the row matches the filter, False otherwise
        """
        strategy = self.get_strategy(token)
        if strategy:
            return strategy.matches(row)
        return False
