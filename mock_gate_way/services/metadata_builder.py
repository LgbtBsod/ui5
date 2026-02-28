from utils.odata import ODATA_NS


def build_metadata() -> str:
    return f'''<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="1.0" xmlns:edmx="http://schemas.microsoft.com/ado/2007/06/edmx">
 <edmx:DataServices m:DataServiceVersion="2.0" xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">
  <Schema Namespace="{ODATA_NS}" xmlns="http://schemas.microsoft.com/ado/2008/09/edm" xmlns:sap="http://www.sap.com/Protocols/SAPData">
   <EntityType Name="ChecklistSearch"><Key><PropertyRef Name="Key"/></Key><Property Name="Key" Type="Edm.String" Nullable="false"/><Property Name="Id" Type="Edm.String"/><Property Name="DateCheck" Type="Edm.DateTime"/><Property Name="TimeCheck" Type="Edm.String"/><Property Name="TimeZone" Type="Edm.String"/><Property Name="LocationKey" Type="Edm.String"/><Property Name="Lpc" Type="Edm.String"/><Property Name="LpcText" Type="Edm.String"/><Property Name="Profession" Type="Edm.String"/><Property Name="ProfessionText" Type="Edm.String"/><Property Name="EquipName" Type="Edm.String"/><Property Name="Status" Type="Edm.String"/><Property Name="CreatedOn" Type="Edm.DateTime"/><Property Name="ChangedOn" Type="Edm.DateTime"/><Property Name="HasFailedChecks" Type="Edm.Boolean"/><Property Name="HasFailedBarriers" Type="Edm.Boolean"/><Property Name="SuccessChecksRate" Type="Edm.Double"/><Property Name="SuccessBarriersRate" Type="Edm.Double"/></EntityType>
   <EntityType Name="ChecklistRoot"><Key><PropertyRef Name="Key"/></Key><Property Name="Key" Type="Edm.String" Nullable="false"/><Property Name="RequestId" Type="Edm.String"/><Property Name="Id" Type="Edm.String"/><Property Name="ChangedOn" Type="Edm.DateTime"/><Property Name="CreatedOn" Type="Edm.DateTime"/><Property Name="Status" Type="Edm.String"/><Property Name="HasFailedChecks" Type="Edm.Boolean"/><Property Name="HasFailedBarriers" Type="Edm.Boolean"/><Property Name="SuccessChecksRate" Type="Edm.Double"/><Property Name="SuccessBarriersRate" Type="Edm.Double"/></EntityType>
   <EntityType Name="ChecklistBasicInfo"><Key><PropertyRef Name="RootKey"/></Key><Property Name="RootKey" Type="Edm.String" Nullable="false"/><Property Name="LocationKey" Type="Edm.String"/><Property Name="LocationName" Type="Edm.String"/><Property Name="ObserverPernr" Type="Edm.String"/><Property Name="ObserverFullname" Type="Edm.String"/><Property Name="ObserverPosition" Type="Edm.String"/><Property Name="ObserverOrgUnit" Type="Edm.String"/><Property Name="ObservedPernr" Type="Edm.String"/><Property Name="ObservedFullname" Type="Edm.String"/><Property Name="ObservedPosition" Type="Edm.String"/><Property Name="ObservedOrgUnit" Type="Edm.String"/><Property Name="Lpc" Type="Edm.String"/><Property Name="Profession" Type="Edm.String"/><Property Name="DateCheck" Type="Edm.DateTime"/><Property Name="TimeCheck" Type="Edm.String"/><Property Name="TimeZone" Type="Edm.String"/><Property Name="EquipName" Type="Edm.String"/></EntityType>
   <EntityType Name="ChecklistCheck"><Key><PropertyRef Name="Key"/></Key><Property Name="Key" Type="Edm.String" Nullable="false"/><Property Name="RootKey" Type="Edm.String"/><Property Name="ChecksNum" Type="Edm.Int32"/><Property Name="Comment" Type="Edm.String"/><Property Name="Result" Type="Edm.Boolean"/><Property Name="ChangedOn" Type="Edm.DateTime"/></EntityType>
   <EntityType Name="ChecklistBarrier"><Key><PropertyRef Name="Key"/></Key><Property Name="Key" Type="Edm.String" Nullable="false"/><Property Name="RootKey" Type="Edm.String"/><Property Name="BarriersNum" Type="Edm.Int32"/><Property Name="Comment" Type="Edm.String"/><Property Name="Result" Type="Edm.Boolean"/><Property Name="ChangedOn" Type="Edm.DateTime"/></EntityType>
   <EntityType Name="DictionaryItem"><Key><PropertyRef Name="Domain"/><PropertyRef Name="Key"/></Key><Property Name="Domain" Type="Edm.String" Nullable="false"/><Property Name="Key" Type="Edm.String" Nullable="false"/><Property Name="Text" Type="Edm.String"/></EntityType>
   <EntityType Name="LastChange"><Key><PropertyRef Name="RootKey"/></Key><Property Name="RootKey" Type="Edm.String" Nullable="false"/><Property Name="AggChangedOn" Type="Edm.DateTime"/></EntityType>
   <EntityType Name="LockStatus"><Key><PropertyRef Name="RootKey"/></Key><Property Name="RootKey" Type="Edm.String" Nullable="false"/><Property Name="Ok" Type="Edm.Boolean"/><Property Name="ReasonCode" Type="Edm.String"/><Property Name="Owner" Type="Edm.String"/><Property Name="ExpiresOn" Type="Edm.DateTime"/></EntityType>
   <EntityType Name="RuntimeSettings"><Key><PropertyRef Name="Key"/></Key><Property Name="Key" Type="Edm.String" Nullable="false"/><Property Name="CacheToleranceMs" Type="Edm.Int32"/><Property Name="HeartbeatIntervalSec" Type="Edm.Int32"/><Property Name="StatusPollIntervalSec" Type="Edm.Int32"/><Property Name="LockTtlSec" Type="Edm.Int32"/><Property Name="IdleTimeoutSec" Type="Edm.Int32"/><Property Name="AutoSaveDebounceMs" Type="Edm.Int32"/><Property Name="RequiredFieldsJson" Type="Edm.String"/><Property Name="UploadPolicyJson" Type="Edm.String"/></EntityType>
   <EntityType Name="AttachmentFolder"><Key><PropertyRef Name="FolderKey"/></Key><Property Name="FolderKey" Type="Edm.String" Nullable="false"/><Property Name="RootKey" Type="Edm.String"/><Property Name="Title" Type="Edm.String"/><Property Name="CreatedOn" Type="Edm.DateTime"/><Property Name="ChangedOn" Type="Edm.DateTime"/></EntityType>
   <EntityType Name="Attachment" m:HasStream="true"><Key><PropertyRef Name="AttachmentKey"/></Key><Property Name="AttachmentKey" Type="Edm.String" Nullable="false"/><Property Name="RootKey" Type="Edm.String"/><Property Name="FolderKey" Type="Edm.String"/><Property Name="CategoryKey" Type="Edm.String"/><Property Name="FileName" Type="Edm.String"/><Property Name="MimeType" Type="Edm.String"/><Property Name="FileSize" Type="Edm.Int32"/><Property Name="ScanStatus" Type="Edm.String"/><Property Name="ScannedOn" Type="Edm.DateTime"/><Property Name="CreatedOn" Type="Edm.DateTime"/><Property Name="ChangedOn" Type="Edm.DateTime"/></EntityType>
   <EntityContainer Name="Z_UI5_SRV_Entities" m:IsDefaultEntityContainer="true" sap:supported-formats="json">
    <EntitySet Name="ChecklistSearchSet" EntityType="{ODATA_NS}.ChecklistSearch"/>
    <EntitySet Name="ChecklistRootSet" EntityType="{ODATA_NS}.ChecklistRoot"/>
    <EntitySet Name="ChecklistBasicInfoSet" EntityType="{ODATA_NS}.ChecklistBasicInfo"/>
    <EntitySet Name="ChecklistCheckSet" EntityType="{ODATA_NS}.ChecklistCheck"/>
    <EntitySet Name="ChecklistBarrierSet" EntityType="{ODATA_NS}.ChecklistBarrier"/>
    <EntitySet Name="DictionaryItemSet" EntityType="{ODATA_NS}.DictionaryItem"/>
    <EntitySet Name="LastChangeSet" EntityType="{ODATA_NS}.LastChange"/>
    <EntitySet Name="LockStatusSet" EntityType="{ODATA_NS}.LockStatus"/>
    <EntitySet Name="RuntimeSettingsSet" EntityType="{ODATA_NS}.RuntimeSettings"/>
    <EntitySet Name="AttachmentFolderSet" EntityType="{ODATA_NS}.AttachmentFolder"/>
    <EntitySet Name="AttachmentSet" EntityType="{ODATA_NS}.Attachment"/>
    <FunctionImport Name="LockControl" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="AutoSave" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="SaveChanges" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="SetChecklistStatus" ReturnType="Edm.String" m:HttpMethod="POST"/>
    <FunctionImport Name="GetHierarchy" ReturnType="Edm.String" m:HttpMethod="GET"/>
    <FunctionImport Name="ReportExport" ReturnType="Edm.String" m:HttpMethod="POST"/>
   </EntityContainer>
  </Schema>
 </edmx:DataServices>
</edmx:Edmx>'''
