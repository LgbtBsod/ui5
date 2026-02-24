# odata/types.py

from datetime import datetime

def python_to_edm(py_type):
    mapping = {
        str: "Edm.String",
        int: "Edm.Int32",
        bool: "Edm.Boolean",
        float: "Edm.Double",
    }
    return mapping.get(py_type, "Edm.String")