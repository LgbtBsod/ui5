from __future__ import annotations

from sqlalchemy.orm.interfaces import MANYTOONE, ONETOMANY
from sqlalchemy.sql.sqltypes import Boolean, Date, DateTime, Integer, String, Text

from database import Base
from services.action_registry import ACTIONS

NAMESPACE = "MockService"


def map_type(column_type):
    if isinstance(column_type, (String, Text)):
        return "Edm.String"
    if isinstance(column_type, Integer):
        return "Edm.Int32"
    if isinstance(column_type, DateTime):
        return "Edm.DateTime"
    if isinstance(column_type, Date):
        return "Edm.DateTime"
    if isinstance(column_type, Boolean):
        return "Edm.Boolean"
    return "Edm.String"


def _assoc_name(source: str, rel_name: str, target: str) -> str:
    return f"{source}_{rel_name}_{target}"


def build_metadata() -> str:
    entity_types: list[str] = []
    entity_sets: list[str] = []
    associations: list[str] = []
    function_imports: list[str] = []

    association_names: set[str] = set()

    for mapper in sorted(Base.registry.mappers, key=lambda m: m.class_.__name__):
        cls = mapper.class_
        table = cls.__table__
        name = cls.__name__

        props: list[str] = []
        keys: list[str] = []
        nav_props: list[str] = []

        for column in table.columns:
            edm_type = map_type(column.type)
            nullable = "false" if column.primary_key else str(column.nullable).lower()
            props.append(f'<Property Name="{column.name}" Type="{edm_type}" Nullable="{nullable}" />')
            if column.primary_key:
                keys.append(f'<PropertyRef Name="{column.name}" />')

        for rel in mapper.relationships:
            target = rel.mapper.class_.__name__
            assoc_name = _assoc_name(name, rel.key, target)

            if rel.direction == ONETOMANY:
                from_multiplicity, to_multiplicity = "1", "*"
            elif rel.direction == MANYTOONE:
                from_multiplicity, to_multiplicity = "*", "1"
            else:
                from_multiplicity, to_multiplicity = "*", "*"

            if assoc_name not in association_names:
                association_names.add(assoc_name)

                principal_refs: list[str] = []
                dependent_refs: list[str] = []
                for local_col, remote_col in rel.local_remote_pairs:
                    if local_col.foreign_keys:
                        principal_refs.append(remote_col.name)
                        dependent_refs.append(local_col.name)

                referential_constraint = ""
                if principal_refs and dependent_refs:
                    principal_xml = "".join([f'<PropertyRef Name="{c}" />' for c in principal_refs])
                    dependent_xml = "".join([f'<PropertyRef Name="{c}" />' for c in dependent_refs])
                    referential_constraint = (
                        "<ReferentialConstraint>"
                        f'<Principal Role="{target}">{principal_xml}</Principal>'
                        f'<Dependent Role="{name}">{dependent_xml}</Dependent>'
                        "</ReferentialConstraint>"
                    )

                associations.append(
                    f'<Association Name="{assoc_name}">'
                    f'<End Type="{NAMESPACE}.{name}" Role="{name}" Multiplicity="{from_multiplicity}"/>'
                    f'<End Type="{NAMESPACE}.{target}" Role="{target}" Multiplicity="{to_multiplicity}"/>'
                    f"{referential_constraint}"
                    "</Association>"
                )

            nav_props.append(
                f'<NavigationProperty Name="{rel.key}" Relationship="{NAMESPACE}.{assoc_name}" FromRole="{name}" ToRole="{target}"/>'
            )

        entity_types.append(
            f'<EntityType Name="{name}"><Key>{"".join(keys)}</Key>{"".join(props)}{"".join(nav_props)}</EntityType>'
        )
        entity_sets.append(f'<EntitySet Name="{name}s" EntityType="{NAMESPACE}.{name}" />')

    for action in ACTIONS:
        function_imports.append(
            f'<FunctionImport Name="{action["name"]}" ReturnType="{action["return"]}" m:HttpMethod="{action["method"]}"/>'
        )

    metadata = f'''<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="1.0" xmlns:edmx="http://schemas.microsoft.com/ado/2007/06/edmx">
 <edmx:DataServices>
  <Schema Namespace="{NAMESPACE}" xmlns="http://schemas.microsoft.com/ado/2008/09/edm" xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">
   {''.join(entity_types)}
   {''.join(associations)}
   <EntityContainer Name="Container" m:IsDefaultEntityContainer="true">
    {''.join(entity_sets)}
    {''.join(function_imports)}
   </EntityContainer>
  </Schema>
 </edmx:DataServices>
</edmx:Edmx>
'''

    return metadata
