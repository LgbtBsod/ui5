from util.odata_filter import ODataFilterParser


class FilterParser:
    @staticmethod
    def parse(model, expression: str):
        if not expression:
            return None
        return ODataFilterParser.parse(model, expression)
