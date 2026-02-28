from utils.odata import ODATA_NS


def build_metadata() -> str:
    return f'''<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="1.0" xmlns:edmx="http://schemas.microsoft.com/ado/2007/06/edmx">
 <edmx:DataServices m:DataServiceVersion="2.0" xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">
  <Schema Namespace="{ODATA_NS}" xmlns="http://schemas.microsoft.com/ado/2008/09/edm" xmlns:sap="http://www.sap.com/Protocols/SAPData">
   <EntityType Name="Checklist"><Key><PropertyRef Name="Uuid"/></Key>
    <Property Name="Uuid" Type="Edm.String" Nullable="false" sap:label="UUID" sap:creatable="false" sap:updatable="false"/>
    <Property Name="ChecklistId" Type="Edm.String" sap:filterable="true" sap:sortable="true"/>
    <Property Name="Lpc" Type="Edm.String" sap:filterable="true" sap:sortable="true"/>
    <Property Name="Status" Type="Edm.String" sap:filterable="true" sap:sortable="true"/>
    <Property Name="Date" Type="Edm.String" sap:filterable="true" sap:sortable="true"/>
    <Property Name="Equipment" Type="Edm.String" sap:filterable="true" sap:sortable="true"/>
    <Property Name="ChangedOn" Type="Edm.DateTime" sap:filterable="true" sap:sortable="true"/>
    <Property Name="ChangedBy" Type="Edm.String" sap:filterable="true" sap:sortable="true"/>
    <Property Name="VersionNumber" Type="Edm.Int32" sap:filterable="true" sap:sortable="true"/>
   </EntityType>
   <EntityType Name="ChecklistFlat"><Key><PropertyRef Name="Uuid"/></Key>
    <Property Name="Uuid" Type="Edm.String" Nullable="false"/><Property Name="ChecklistId" Type="Edm.String"/><Property Name="Lpc" Type="Edm.String"/>
    <Property Name="Status" Type="Edm.String"/><Property Name="Date" Type="Edm.String"/><Property Name="Equipment" Type="Edm.String"/>
    <Property Name="ChangedOn" Type="Edm.DateTime"/><Property Name="ChangedBy" Type="Edm.String"/><Property Name="VersionNumber" Type="Edm.Int32"/>
   </EntityType>
   <EntityType Name="Check"><Key><PropertyRef Name="Uuid"/></Key><Property Name="Uuid" Type="Edm.String" Nullable="false"/><Property Name="ParentUuid" Type="Edm.String" sap:filterable="true"/><Property Name="Text" Type="Edm.String"/><Property Name="Status" Type="Edm.String"/><Property Name="Position" Type="Edm.Int32"/><Property Name="ChangedOn" Type="Edm.DateTime"/><Property Name="EditMode" Type="Edm.String"/></EntityType>
   <EntityType Name="Barrier"><Key><PropertyRef Name="Uuid"/></Key><Property Name="Uuid" Type="Edm.String" Nullable="false"/><Property Name="ParentUuid" Type="Edm.String" sap:filterable="true"/><Property Name="Description" Type="Edm.String"/><Property Name="IsActive" Type="Edm.Boolean"/><Property Name="Position" Type="Edm.Int32"/><Property Name="ChangedOn" Type="Edm.DateTime"/><Property Name="EditMode" Type="Edm.String"/></EntityType>
   <EntityType Name="LastChange"><Key><PropertyRef Name="Uuid"/></Key><Property Name="Uuid" Type="Edm.String" Nullable="false"/><Property Name="EntityName" Type="Edm.String"/><Property Name="EntityId" Type="Edm.String"/><Property Name="ServerChangedOn" Type="Edm.DateTime"/></EntityType>
   <EntityType Name="PersonVH"><Key><PropertyRef Name="Pernr"/></Key><Property Name="Pernr" Type="Edm.String" Nullable="false"/><Property Name="FirstName" Type="Edm.String"/><Property Name="LastName" Type="Edm.String"/><Property Name="MiddleName" Type="Edm.String"/><Property Name="Position" Type="Edm.String"/><Property Name="OrgUnit" Type="Edm.String"/><Property Name="IntegrationName" Type="Edm.String"/><Property Name="Begda" Type="Edm.String"/><Property Name="Endda" Type="Edm.String"/><Property Name="ChangedOn" Type="Edm.DateTime"/></EntityType>
   <EntityType Name="Attachment"><Key><PropertyRef Name="Uuid"/></Key><Property Name="Uuid" Type="Edm.String" Nullable="false"/><Property Name="ParentUuid" Type="Edm.String"/><Property Name="FileName" Type="Edm.String"/></EntityType>
   <EntityType Name="MplTree"><Key><PropertyRef Name="NodeId"/></Key><Property Name="NodeId" Type="Edm.String" Nullable="false"/><Property Name="ParentId" Type="Edm.String"/><Property Name="Level" Type="Edm.Int32"/><Property Name="Text" Type="Edm.String"/><Property Name="ValidFrom" Type="Edm.String"/><Property Name="ValidTo" Type="Edm.String"/></EntityType>

   <EntityContainer Name="Z_UI5_SRV_Entities" m:IsDefaultEntityContainer="true" sap:supported-formats="json">
    <EntitySet Name="ChecklistSet" EntityType="{ODATA_NS}.Checklist" sap:creatable="true" sap:updatable="true" sap:deletable="true"/>
    <EntitySet Name="ChecklistFlatSet" EntityType="{ODATA_NS}.ChecklistFlat" sap:creatable="false" sap:updatable="false" sap:deletable="false"/>
    <EntitySet Name="CheckSet" EntityType="{ODATA_NS}.Check" sap:creatable="true" sap:updatable="true" sap:deletable="true"/>
    <EntitySet Name="BarrierSet" EntityType="{ODATA_NS}.Barrier" sap:creatable="true" sap:updatable="true" sap:deletable="true"/>
    <EntitySet Name="LastChangeSet" EntityType="{ODATA_NS}.LastChange" sap:creatable="false" sap:updatable="false" sap:deletable="false"/>
    <EntitySet Name="PersonVHSet" EntityType="{ODATA_NS}.PersonVH" sap:creatable="false" sap:updatable="false" sap:deletable="false"/>
    <EntitySet Name="AttachmentSet" EntityType="{ODATA_NS}.Attachment" sap:creatable="false" sap:updatable="false" sap:deletable="false"/>
    <EntitySet Name="MplTreeSet" EntityType="{ODATA_NS}.MplTree" sap:creatable="false" sap:updatable="false" sap:deletable="false"/>
    <FunctionImport Name="LockAcquire" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="LockHeartbeat" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="LockRelease" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="SaveDraft" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="SaveChanges" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="AutoSave" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="CopyChecklist" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="MplTree" ReturnType="Edm.String" m:HttpMethod="GET"/>
   </EntityContainer>
  </Schema>
 </edmx:DataServices>
</edmx:Edmx>'''
