from models import Checklist, BasicInfo, CheckItem, BarrierItem
from odata.entity_generator import generate_entity_type
from odata.relations import RELATIONS
from odata.association_generator import generate_association


def generate_metadata():

    entities = [
        generate_entity_type("Checklist", Checklist),
        generate_entity_type("BasicInfo", BasicInfo),
        generate_entity_type("CheckItem", CheckItem),
        generate_entity_type("BarrierItem", BarrierItem),
    ]

    associations = [generate_association(r) for r in RELATIONS]

    entity_sets = """
    <EntityContainer Name="MockContainer"
        m:IsDefaultEntityContainer="true"
        xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">

        <EntitySet Name="ChecklistSet" EntityType="Mock.Checklist"/>
        <EntitySet Name="BasicInfoSet" EntityType="Mock.BasicInfo"/>
        <EntitySet Name="CheckItemSet" EntityType="Mock.CheckItem"/>
        <EntitySet Name="BarrierItemSet" EntityType="Mock.BarrierItem"/>

    </EntityContainer>
    """

    return f"""<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="1.0"
 xmlns:edmx="http://schemas.microsoft.com/ado/2007/06/edmx">
 <edmx:DataServices>
  <Schema Namespace="Mock"
   xmlns="http://schemas.microsoft.com/ado/2008/09/edm">

    {''.join(entities)}
    {''.join(associations)}
    {entity_sets}

  </Schema>
 </edmx:DataServices>
</edmx:Edmx>
"""