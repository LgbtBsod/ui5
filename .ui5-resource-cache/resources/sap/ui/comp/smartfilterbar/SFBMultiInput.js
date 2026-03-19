/*!
 * SAPUI5

		(c) Copyright 2009-2019 SAP SE. All rights reserved
	
 */
sap.ui.define(["sap/m/MultiInput","sap/m/MultiInputRenderer"],function(M,a){"use strict";var S=M.extend("sap.ui.comp.smartfilterbar.SFBMultiInput",{metadata:{library:"sap.ui.comp"},renderer:a});S.prototype.setTokens=function(t){M.prototype.setTokens.apply(this,arguments);this._pendingAutoTokenGeneration=true;this._getFilterProvider()._tokenUpdate({control:this,fieldViewMetadata:this._getFieldViewMetadata()});this._pendingAutoTokenGeneration=false;};S.prototype._setFilterProvider=function(f){this.oFilterProvider=f;};S.prototype._getFilterProvider=function(){return this.oFilterProvider;};S.prototype._setFieldViewMetadata=function(f){this.oFieldViewMetadata=f;};S.prototype._getFieldViewMetadata=function(){return this.oFieldViewMetadata;};S.prototype.onBeforeRendering=function(){M.prototype.onBeforeRendering.apply(this,arguments);if(this.getValue()){this._pendingAutoTokenGeneration=true;this._validateCurrentText(true);this._pendingAutoTokenGeneration=false;}};return S;});
