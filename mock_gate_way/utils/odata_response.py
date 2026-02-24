def odata_list(data):
    return {"d": {"results": data}}


def odata_single(data):
    return {"d": data}
