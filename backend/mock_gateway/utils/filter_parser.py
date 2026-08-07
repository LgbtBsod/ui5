from collections.abc import Mapping

from utils.filter_engine import parse_filter_to_predicate
from utils.odata_filter_sql import ODataFilterParser


class FilterParser:
    @staticmethod
    def parse(model, expression: str, field_map: Mapping[str, str] | None = None):
        if not expression:
            return None
        normalized = expression.strip()
        if not normalized:
            return None
        return ODataFilterParser.parse(model, normalized, field_map=field_map)

    @staticmethod
    def parse_to_predicate(expression: str | None, field_map: Mapping[str, str] | None = None):
        return parse_filter_to_predicate(expression, dict(field_map or {}))
