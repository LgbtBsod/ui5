from util.odata_filter import ODataFilterParser


class FilterParser:
    @staticmethod
    def parse(model, expression: str):
        if not expression:
            return None
        normalized = expression.strip()
        if not normalized:
            return None
        return ODataFilterParser.parse(model, normalized)
