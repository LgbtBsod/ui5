from typing import get_type_hints
from odata.types import python_to_edm
from odata.relations import RELATIONS


def generate_navigation(entity_name):
    nav_xml = ""
    for rel in RELATIONS:
        if rel["principal"] == entity_name:
            nav_xml += f"""
            <NavigationProperty Name="{rel['nav_principal']}"
              Relationship="Mock.{rel['principal']}_{rel['dependent']}"
              FromRole="{rel['principal']}"
              ToRole="{rel['dependent']}" />
            """
    return nav_xml


def generate_entity_type(name, model):
    hints = get_type_hints(model)

    props = ""
    key_block = ""

    for field, field_type in hints.items():
        edm_type = python_to_edm(field_type)

        if field == "id":
            key_block = """
            <Key>
                <PropertyRef Name="id"/>
            </Key>
            """

        props += f'<Property Name="{field}" Type="{edm_type}" Nullable="false" />'

    nav = generate_navigation(name)

    return f"""
    <EntityType Name="{name}">
        {key_block}
        {props}
        {nav}
    </EntityType>
    """