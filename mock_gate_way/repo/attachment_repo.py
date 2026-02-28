class AttachmentRepo:
    def __init__(self, storage: dict[str, dict]):
        self.storage = storage

    def create_meta(self, key: str, payload: dict):
        self.storage[key] = payload
        return self.storage[key]

    def put_value(self, key: str, content: bytes):
        self.storage[key]["content"] = content
        return self.storage[key]

    def delete(self, key: str):
        return self.storage.pop(key, None)

    def list_by_root(self, root_uuid: str):
        return [x for x in self.storage.values() if x.get("root_uuid") == root_uuid]
