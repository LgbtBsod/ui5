class FilterSyntaxError(ValueError):
    """Raised when an OData $filter expression cannot be parsed or references an
    unknown field/operator. Callers must map this to HTTP 400 - silently falling
    back to "match everything" would leak the full unfiltered dataset to the client.
    """
