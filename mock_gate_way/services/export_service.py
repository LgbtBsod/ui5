class ExportService:
    @staticmethod
    def preserve_root_order(keys: list[str], rows_by_key: dict[str, dict]) -> list[dict]:
        return [rows_by_key[k] for k in keys if k in rows_by_key]
