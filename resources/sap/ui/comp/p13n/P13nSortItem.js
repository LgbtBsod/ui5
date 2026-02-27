/*!
 * SAPUI5
 * (c) Copyright 2025 SAP SE. All rights reserved.
 */
sap.ui.define(["sap/m/library","sap/ui/core/Item"],function(t,e){"use strict";var r=e.extend("sap.ui.comp.p13n.P13nSortItem",{metadata:{library:"sap.ui.comp",properties:{operation:{type:"string",group:"Misc",defaultValue:null},columnKey:{type:"string",group:"Misc",defaultValue:null}}}});r.prototype.setColumnKey=function(t){return this.setProperty("columnKey",t,true)};r.prototype.setOperation=function(t){return this.setProperty("operation",t,true)};return r});
//# sourceMappingURL=P13nSortItem.js.map