/*!
 * OpenUI5
 * (c) Copyright 2009-2019 SAP SE or an SAP affiliate company.
 * Licensed under the Apache License, Version 2.0 - see LICENSE.txt.
 */
sap.ui.define(['sap/m/Button','sap/m/ButtonRenderer'],function(B,a){"use strict";var O=B.extend("sap.m.OverflowToolbarButton",{metadata:{interfaces:["sap.f.IShellBar","sap.m.IToolbarInteractiveControl"]},renderer:a.render});O.prototype._getText=function(){if(this._bInOverflow){return B.prototype._getText.call(this);}return"";};O.prototype._getToolbarInteractive=function(){return true;};return O;});
