/*!
 * OpenUI5
 * (c) Copyright 2009-2019 SAP SE or an SAP affiliate company.
 * Licensed under the Apache License, Version 2.0 - see LICENSE.txt.
 */
sap.ui.define(["sap/ui/core/date/CalendarWeekNumbering","sap/ui/core/LocaleData"],function(C,L){"use strict";var w={ISO_8601:{firstDayOfWeek:1,minimalDaysInFirstWeek:4},MiddleEastern:{firstDayOfWeek:6,minimalDaysInFirstWeek:1},WesternTraditional:{firstDayOfWeek:0,minimalDaysInFirstWeek:1}};var a={getWeekConfigurationValues:function(c,l){var o;if(w.hasOwnProperty(c)){return w[c];}c=c||C.Default;if(c===C.Default){l=l||sap.ui.getCore().getConfiguration().getFormatSettings().getFormatLocale();o=L.getInstance(l);return{firstDayOfWeek:o.getFirstDayOfWeek(),minimalDaysInFirstWeek:o.getMinimalDaysInFirstWeek()};}return undefined;}};return a;});
