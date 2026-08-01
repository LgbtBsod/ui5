sap.ui.define([], function () {
	"use strict";

	// Идемпотентность "контрол уже обработан" через WeakSet (object-ключи) плюс
	// обычный Set для строковых ключей — для случаев, когда "once"-идентичность
	// определяется путём/строкой, а не ссылкой на объект. Не путать с
	// CachedElementLookup — та кэширует результат поиска, а не факт обработки.
	class OnceRegistry {
		constructor() {
			this._oSeen = new WeakSet();
			this._oSeenKeys = new Set();
		}

		isMarked(oElement) {
			return this._oSeen.has(oElement);
		}

		mark(oElement) {
			this._oSeen.add(oElement);
		}

		unmark(oElement) {
			this._oSeen.delete(oElement);
		}

		// Возвращает true, если fnAction был вызван (т.е. ключ был новым).
		onceByKey(sKey, fnAction) {
			if (this._oSeenKeys.has(sKey)) {
				return false;
			}
			this._oSeenKeys.add(sKey);
			fnAction();
			return true;
		}
	}

	return OnceRegistry;
});
