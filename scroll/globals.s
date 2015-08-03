; Here in globals.s we specify common blocks of memory that are re-used in
; multiple effects.

; NOTE: If you change these, then also update the load addresses in !BOOT

; TODO: This might be a suitable place if we are always in a heavier mode, but
; it is not suitable if we are in MODE 7 and want to use a large area before
; screenmem for data.
; It would be preferable to put the lookups either before or after the program
; in memory.

;.alias sinLookup $2B00
.alias sinLookup $2600

.alias lookupLog $2C00
.alias lookupUnlog2 lookupLog + 256
.alias lookupLogOfSin lookupUnlog2 + 256
;; sinCharge is deprecated leaving a spare page - it_s the top bit and faster to test that way!
; .alias sinCharge lookupLogOfSin + 256
.alias lookupSquare lookupLogOfSin + 256

