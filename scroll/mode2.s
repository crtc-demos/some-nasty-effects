; With thorough use of these properties, we should be able to recompile effects
; to work in different modes!

;.include "mode_clear.spp"

.alias MODE_MODE 2
.alias MODE_COLUMNS 20
.alias MODE_BITS_PER_PIXEL 4
.alias MODE_COLORS 16
.alias MODE_SCRTOP 0x3000
; MODE_BYTES_PER_ROW really means PER_CHAR_ROW, not per pixel row
.alias MODE_BYTES_PER_ROW 640
.alias MODE_PIXELS_PER_ROW 160
.alias MODE_PIXELS_PER_BYTE 2
; #define MODE_LOOKUP_COLOR_BITS .byte 1, 4, 16, 64
; #define MODE_LOOKUP_COLOR_BITS .byte 1,4,5,16,17,20,21,25,64,65,brainsplode...
; #define MODE_PALETTE .byte 0, 1, 4, 0b00000101, 0b00010000, 0b00010001, 0b00010100, 0b00010101, 0b01000000, 0b01000001, 0b01000100, 0b01000101, 0b01010000, 0b01010001, 0b01010100, 0b01010101
; rol this 1 bit left for the left-hand pixel of 2-pixel byte
; #define MODE_PIXELS_PER_ROW (MODE_COLUMNS*8)
; #define MODE_PIXELS_PER_BYTE (8/MODE_BITS_PER_PIXEL)

.include "mode_graphics.s"

; ... blah
; TODO: Since many of these are derived, we might want to #inc a set of calculations

