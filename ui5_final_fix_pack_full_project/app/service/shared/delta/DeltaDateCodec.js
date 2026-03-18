sap.ui.define([], function () {
  "use strict";

  function parseODataDateMillis(vDate) {
    if (!vDate) { return null; }
    if (typeof vDate === "string") {
      var m = vDate.match(/^\/Date\((-?\d+)(?:[+-]\d+)?\)\/$/);
      if (m) { return Number(m[1]); }
    }
    var n = new Date(vDate).getTime();
    return Number.isNaN(n) ? null : n;
  }

  function formatODataDate(vDate) {
    var nMillis = parseODataDateMillis(vDate);
    if (nMillis === null) { return null; }
    return "/Date(" + nMillis + ")/";
  }

  return { parseODataDateMillis: parseODataDateMillis, formatODataDate: formatODataDate };
});
