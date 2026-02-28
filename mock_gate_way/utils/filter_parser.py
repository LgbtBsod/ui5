from collections.abc import Mapping

from util.odata_filter import ODataFilterParser


class FilterParser:
    @staticmethod
    def parse(model, expression: str, field_map: Mapping[str, str] | None = None):
        if not expression:
            return None
        normalized = expression.strip()
        if not normalized:
            return None
        return ODataFilterParser.parse(model, normalized, field_map=field_map)
