from collections.abc import Mapping

from util.odata_filter import ODataFilterParser
from utils.filter_engine import parse_filter_to_predicate


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
