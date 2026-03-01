sap.ui.define([], function () {
    "use strict";

    var STORAGE_KEY = "ui5.splitter.sizes";
    var SNAP_POINTS = [25, 33, 50, 67, 75];

    function debugLog(sEvent, oPayload) {
        if (window.__DEBUG_UI5__ === true && window.console && window.console.info) {
            window.console.info("[SplitterSizeService] " + sEvent, oPayload || {});
        }
    }

    function clampPct(nPct) {
        var n = Number(nPct);
        if (!Number.isFinite(n)) {
            return 50;
        }
        return Math.max(0, Math.min(100, n));
    }

    function roundPct(nPct) {
        return Math.round(clampPct(nPct));
    }

    function snapPct(nPct) {
        var n = clampPct(nPct);
        if (n <= 8) {
            return 0;
        }
        if (n >= 92) {
            return 100;
        }

        var nClosest = SNAP_POINTS[0];
        var nDistance = Math.abs(n - nClosest);
        SNAP_POINTS.forEach(function (nPoint) {
            var nNextDistance = Math.abs(n - nPoint);
            if (nNextDistance < nDistance) {
                nClosest = nPoint;
                nDistance = nNextDistance;
            }
        });
        return nDistance <= 3 ? nClosest : roundPct(n);
    }

    function safeParseStorage() {
        try {
            var sRaw = window.localStorage.getItem(STORAGE_KEY);
            return sRaw ? JSON.parse(sRaw) : null;
        } catch (e) {
            return null;
        }
    }

    function safeStore(mState) {
        try {
            window.localStorage.setItem(STORAGE_KEY, JSON.stringify(mState));
        } catch (e) {
            // no-op: storage failures must not break app
        }
    }

    function normalizeSizes(mInput) {
        var nLeft = snapPct(mInput && mInput.leftPct);
        var nRight = roundPct(100 - nLeft);
        var nLastLeft = snapPct((mInput && mInput.lastLeftPct) || nLeft || 40);
        var nLastRight = roundPct(100 - nLastLeft);
        var sCollapsedSide = (mInput && mInput.collapsedSide) || null;

        return {
            leftPct: nLeft,
            rightPct: nRight,
            lastLeftPct: nLastLeft,
            lastRightPct: nLastRight,
            collapsedSide: sCollapsedSide === "left" || sCollapsedSide === "right" ? sCollapsedSide : null
        };
    }

    function toPctText(nPct) {
        return String(roundPct(nPct)) + "%";
    }

    function syncStateModel(oStateModel, mSizes) {
        var mNormalized = normalizeSizes(mSizes);
        oStateModel.setProperty("/splitSizes", mNormalized);
        oStateModel.setProperty("/splitterLeftSize", toPctText(mNormalized.leftPct));
        oStateModel.setProperty("/splitterRightSize", toPctText(mNormalized.rightPct));
        return mNormalized;
    }

    return {
        restoreOnBoot: function (oStateModel) {
            var mStored = safeParseStorage();
            var mSizes = normalizeSizes(mStored || {
                leftPct: 40,
                rightPct: 60,
                lastLeftPct: 40,
                lastRightPct: 60,
                collapsedSide: null
            });
            syncStateModel(oStateModel, mSizes);
            debugLog("restore", mSizes);
            return mSizes;
        },

        applyMode: function (oStateModel, sMode) {
            var mSizes = normalizeSizes(oStateModel.getProperty("/splitSizes") || {});
            if (sMode === "single") {
                mSizes.leftPct = 100;
                mSizes.rightPct = 0;
                mSizes.collapsedSide = "right";
            } else if (sMode === "detailOnly") {
                mSizes.leftPct = 0;
                mSizes.rightPct = 100;
                mSizes.collapsedSide = "left";
            } else {
                mSizes.leftPct = mSizes.lastLeftPct || 40;
                mSizes.rightPct = mSizes.lastRightPct || 60;
                mSizes.collapsedSide = null;
            }
            syncStateModel(oStateModel, mSizes);
            oStateModel.setProperty("/splitLayoutMode", sMode);
            debugLog("applyMode", { mode: sMode, sizes: mSizes });
        },

        syncFromSplitter: function (oStateModel, nLeftPct) {
            var mSizes = normalizeSizes(oStateModel.getProperty("/splitSizes") || {});
            var nSnappedLeft = snapPct(nLeftPct);
            var nSnappedRight = roundPct(100 - nSnappedLeft);

            mSizes.leftPct = nSnappedLeft;
            mSizes.rightPct = nSnappedRight;

            if (nSnappedLeft === 0) {
                mSizes.collapsedSide = "left";
                oStateModel.setProperty("/splitLayoutMode", "detailOnly");
            } else if (nSnappedRight === 0) {
                mSizes.collapsedSide = "right";
                oStateModel.setProperty("/splitLayoutMode", "single");
            } else {
                mSizes.lastLeftPct = nSnappedLeft;
                mSizes.lastRightPct = nSnappedRight;
                mSizes.collapsedSide = null;
                oStateModel.setProperty("/splitLayoutMode", "split");
            }

            var mSynced = syncStateModel(oStateModel, mSizes);
            debugLog("syncFromSplitter", { inputLeftPct: nLeftPct, sizes: mSynced });
            return mSynced;
        },

        persistIfSplit: function (oStateModel) {
            if (oStateModel.getProperty("/splitLayoutMode") !== "split") {
                return;
            }
            var mStored = normalizeSizes(oStateModel.getProperty("/splitSizes") || {});
            safeStore(mStored);
            debugLog("persist", mStored);
        }
    };
});
