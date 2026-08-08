sap.ui.define(["./OnceRegistry"], function (OnceRegistry) {
	"use strict";

	class ButtonWatcher {
		constructor() {
			this._oSeen = new OnceRegistry();
		}

		watchOnce(oButton, fnOnPress) {
			if (!oButton) {
				return;
			}
			if (this._oSeen.isMarked(oButton)) {
				return;
			}
			this._oSeen.mark(oButton);
			oButton.attachPress(fnOnPress);
		}
	}

	return ButtonWatcher;
});
