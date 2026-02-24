# odata/generator.py

from typing import get_type_hints, List
from odata.types import python_to_edm

def generate_entity_type(name, model):
    hints = get_type_hints(model)
    properties_xml = ""
    navigation_xml = ""

    for field, field_type in hints.items():

        if getattr(field_type, "__origin__", None) == list:
            # navigation collection
            target = field_type.__args__[0].__name__
            navigation_xml += f"""
            <NavigationProperty Name="{field}"
              Relationship="Mock.{name}_{target}"
              FromRole="{name}"
              ToRole="{target}" />
            """
            continue

        if hasattr(field_type, "__fields__"):
            # single navigation
            target = field_type.__name__
            navigation_xml += f"""
            <NavigationProperty Name="{field}"
              Relationship="Mock.{name}_{target}"
              FromRole="{name}"
              ToRole="{target}" />
            """
            continue

        edm_type = python_to_edm(field_type)

        if field == "id":
            key_block = f"""
            <Key>
                <PropertyRef Name="id"/>
            </Key>
            """
        else:
            key_block = ""

        properties_xml += f'<Property Name="{field}" Type="{edm_type}" Nullable="true"/>'

    return f"""
    <EntityType Name="{name}">
        {key_block}
        {properties_xml}
        {navigation_xml}
    </EntityType>
    """