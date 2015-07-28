; Scroller

; TODO CONSIDER Optimisation: If we synchronise phase of y-wobble with screen
; width, this might reduce/simplify the clearing of areas above and below the
; text.  However it doesn_t address the slowdown due to really tall text.  :f

.include "macros-etc.s"

.org $e02

.temps $70..$8f

@startScreenHack setupScroller, shiftScroller

; Needs to come before screenhack.spp
.include "mode2.s"

.alias screenTop $3000
.alias screenEnd $8000

.macro sec_implied
.mend

.ifndef _LOADED_DEMO2LIB

.include "globals.s"

; .alias seed $70

.alias KEEP_INTERRUPTS -1
.alias LOOP_IN_FOREGROUND -1
.alias SHOW_FPS -1

; Do not add any includes before screenhack.spp so it declares entry = org.

.endif

.alias WAVY_TEXT -1
.alias DOUBLE_SPEED -1
; #define WIDER_TEXT
.alias VARY_TEXT_WIDTH -1
; Unfortunately VARY_TEXT_WIDTH decreases the feeling of speed, because it slows down when displaying small text.
; No that is not true!  We still scroll at DOUBLE_SPEED, VARY_TEXT_WIDTH only affects readingBit progression speed.
.alias WOBBLE_HEIGHT -1
.alias WOBBLE_TOP_WITH_HEIGHT -1
.alias SUPER_SQUIDGY -1

; DOUBLE_SPEED writes two vertical lines each frame instead of one, managing 41fps instead of 46fps.
; Choose either WIDER_TEXT or VARY_TEXT_WIDTH, or neither.  They only work with DOUBLE_SPEED enabled.
; I believe the problem is with heavy vertical blocks, which are a minority, but essential!  T I D E

; TODO: Good opportunities for optimisation before gotBlockColor...

; DONE: The solution to rough scaling is to not use the dodgy sine method and
; instead sensibly use a fractional number for the height of the current block.
; When the lower part (remainder) carries, that means this block gets +1
; height.

; DONE:
; Ahhh duh the real problem was that we were only changing the sizeWobble every
; two columns (it was outside the DOUBLE_SPEED loop).  Now it_s inside the loop
; but the problem is it moves too fast!
; So the real problem is the low X-resolution of our sin table.
; Interpolate?
; No ... more likely the problem is that SUPER_SQUIDGY goes up in jumps of K
; every 2 frames.  Better do K/2 every 1 frame!

; TODO: SUPER_SQUIDGY is nice now it_s half on, half off, but the rest size is
; always the same.  We should use a low-byte remainder for sizeWobble, so after
; 256 frames it will flatten out on a different size.

.alias startReading screenTop + MODE_BYTES_PER_ROW
.alias stopReading startReading + MODE_BYTES_PER_ROW*3

; -8 to write onto the RHS of screen
; +1 line ofc, and maybe + some more lines for fun
.alias offsetForBlit [2*MODE_BYTES_PER_ROW - 8]

; #include "../fonts/font12x18.spp"
; .alias fontStart font12x18
; .alias firstBitToRead 8

; #include "../fonts/font16x18.spp"
; #include "../fonts/font16x18_weighted_tooheavy.spp"
; #include "../fonts/font16x18_weighted.spp"
; .alias fontStart font16x18
; #include "../fonts/font16x14_strikethrough.spp"
; #include "../fonts/font16x14_weighted.spp"
; .alias fontStart font16x14
; .alias firstBitToRead 128

; #include "../fonts/font15x14_weighted_outline_filled.spp"
; #include "../fonts/font15x14_weighted_outline_strikethrough.spp"
.include "font15x14_weighted_outline.s"
.alias fontStart font15x14
.alias firstBitToRead 64
; If we DON_T want the chars to touch, we can turn it back to 16x14:
; #undef fontWidth
; #define fontWidth 16
; .alias firstBitToRead 128

; #include "../fonts/font4x6_funked.spp"
; .alias fontStart joeys4x6font
; .alias firstBitToRead 8

.alias fontWidthBytes [[fontWidth-1]/8+1]
.alias fontCharSize [ fontWidthBytes * fontHeight ]
; In general: .alias firstBitToRead 2^((fontWidth MOD 8)-1)

; Variables we don`t need in zeropage
; Declared here so that entry == .org
; To save space, these could go in re-usable bits of memory, e.g. before screenBuf

scrollerText:
	.asc " !!! A BBC MICRO SQUIDGY SCROLLER FOR SUNDOWN 2012"
	; .asc " !!! PROBABLY ONLY SCENERS CAN READ THIS !!! PLEASE ENTER CAPTCHA CODE !!! VERIFICATION FAILED ! HUMAN PRIVILEGES REVOKED"
	; .asc " !!! NOW THE EDGES ARE SMOOTH BUT 50 FPS IS NOT SO EASY"
	.asc " !!! WHICH 6502 ASSEMBLY INSTRUCTION IS THE MOST DEPRESSED? !!! !!! !!! EOR !!! !!!"
	.asc " !!! WHAT DOES A 6502 PROCESSOR DO WHEN IT GETS TIRED? !!! !!! !!! IT TAKES A NOP !!! !!! !!!"
	.byte 0

; Pointer into scrollerText
positionInText: .word scrollerText

; The current top-left address
currentScrStart: .byte 0,0

; What we send to R12,13 for hardware scrolling
scrollRegisters: .byte 0, 0
; related to currentScrStart, but updates in parallel, rather than derived each time

; Where we are reading font data from, relative to readingPos
readingBit:  .byte 0
readingChar: .byte 0

; Things which change over time (mostly indices into sin table)
textWobble:   .byte 0
colorWobble:  .byte 256-32-0   ; Slightly later phase, to sync with colors already on the screen, not those entering the screen now!
sizeWobbleLO: .byte 0
sizeWobble:   .byte 0

resetScrollRegisters:
	lda #<screenTop : sta currentScrStart
	lda #>screenTop : sta currentScrStart+1
	lda #[screenTop >> 3] & $1f : sta scrollRegisters
	lda #[screenTop] >> 11 : sta scrollRegisters+1
	rts
.notemps resetScrollRegisters

.context setupScroller
setupScroller:
.(

	@mode 2
	@hideCursor

	; No we must set screenTop/8 aka screenTop>>3
	; Is that mask useful?  scrollRegisters+0 is usually 0 anyway :P
	jsr resetScrollRegisters

	jsr resetTextLooper
	jsr moveToNextCharacter

	;; Old fudge to get same-size text when wobble is first applied.
	; lda #64+24 : sta sizeWobble
	;; Start at full-height:
	; lda #64 : sta sizeWobble
	;; Start quite medium large:
	; lda #128 : sta sizeWobble
	;; Start medium small:
	lda #128+32 : sta sizeWobble
	;; Start small:
	; lda #128+64 : sta sizeWobble

	rts

.)
.ctxend

resetTextLooper:
	lda #<scrollerText : sta positionInText
	lda #>scrollerText : sta positionInText+1
rts
.notemps resetTextLooper

.context moveToNextCharacter
.var2 positionInTextZP
moveToNextCharacter:
.(
	lda positionInText : sta %positionInTextZP
	lda positionInText+1 : sta %positionInTextZP+1
	ldy #0 : lda (%positionInTextZP),y
	; case statement to get index from charcode
	; we could just subtract 32, and define 63 codes, but that costs 2k of data!
	cmp #65 : bcc notCapitalLetter
		sec : sbc #65 : bra gotIndex
	notCapitalLetter:
	cmp #48 : bcc notNumber
		sec : sbc #48-26 : bra gotIndex
	notNumber:
	cmp #0 : bne notEnd
		; End of string, go back to start and retry
		jsr resetTextLooper
		jmp moveToNextCharacter
	notEnd:
	; cmp #32 : bne notSpace
		; lda #26+10 : jmp gotIndex
	; notSpace:
		; A space or a . or something
		clc : adc #26+10-32
	gotIndex:
	; fontCharInMem=fontStart+fontCharSize*fontByteCols*index
	;                     ceil pixel width/8
	tax
	lda #<fontStart : sta %shiftScroller.readingPos
	lda #>fontStart : sta %shiftScroller.readingPos+1
	loop:
		cpx #0 : beq doneLoop
		lda #<fontCharSize : clc : adc %shiftScroller.readingPos : sta %shiftScroller.readingPos
		; most likely 0 :)
		lda #>fontCharSize : adc %shiftScroller.readingPos+1 : sta %shiftScroller.readingPos+1
		dex
	jmp loop
	doneLoop:
	; OK readingPos is correct
	; Except we need to start on last byterow of font, not first!
	; lda #[fontWidthBytes-1]*fontHeight : @adcWord %shiftScroller.readingPos
	; @incword %shiftScroller.readingPos
	lda #[fontWidthBytes-1] : @adcWord %shiftScroller.readingPos
	lda #fontWidthBytes : sta readingChar
	lda #firstBitToRead : sta readingBit
	; Advance char
	@incword positionInText
.)
.ctxend

; CONSIDER: Could be a routine
.macro checkWrapAround plotPos
	lda %plotPos+1 : cmp #>screenEnd : bcc noLoopBack
		lda %plotPos+1 : sec : sbc #>[screenEnd-screenTop] : sta %plotPos+1
	noLoopBack:
.mend

; CONSIDER: Could be a routine
.macro progressLine charTop rowsFromTop
	.if MODE_BYTES_PER_ROW >= 512
		@addWordToWord %charTop, MODE_BYTES_PER_ROW
	.else
		; lda %charTop : clc : adc #<MODE_BYTES_PER_ROW : sta %charTop
		; bcc noCarry
			; inc %charTop+1
		; noCarry:
		lda #<MODE_BYTES_PER_ROW : @adcWord %charTop
		.if MODE_BYTES_PER_ROW >= 256
			inc %charTop+1
		.endif
		;; This looked from FPS like it might be a bit slower
		; #if MODE_BYTES_PER_ROW >= 512
			; inc %charTop+1
		; #endif
	.endif
	inc %rowsFromTop
	@checkWrapAround %charTop
.mend

; no offset, plot black (clear one whole char)
; CONSIDER: Should this be a routine instead of a macro?  Used in two/three places.
.macro empty8Rows plotPos
	lda #0 : @splat8Rows %plotPos
.mend

; Plots whatever is in A
.macro splat8Rows plotPos
	ldy #7

	; clearLoopInner:
		; sta (%plotPos),y
	; dey : bpl clearLoopInner

	; Faster:
	sta (%plotPos),y : dey
	sta (%plotPos),y : dey
	sta (%plotPos),y : dey
	sta (%plotPos),y : dey
	sta (%plotPos),y : dey
	sta (%plotPos),y : dey
	sta (%plotPos),y : dey
	sta (%plotPos),y

	@progressLine %plotPos, %shiftScroller.rowsFromTop
.mend

; charTop (aka plotPos) and offset will be maintained, i.e. updated on exit
.macro fillXRowsFrom charTop offset ; count
	; A = color/pixelfill
	; X = count
	sta %shiftScroller.colorPixel
	ldy %offset
	; ldx %count
	; CONSIDER: Avoid doubling up of dex,cpx and iny/cpy? It might be more efficient to compare 8-offset against count at the start, to see whether we will or will not need to progress to next charline.  Then we can branch to one of two simpler loops and do only what is needed.
	loopOuter:
		lda %shiftScroller.colorPixel
		loop:
			cpx #0 : beq done
			sta (%charTop),y
			iny : dex    ; WTF can I not do  beq done  here?!  Ohhh if Y was 8.  :F
			cpy #8 : bne loop
		@progressLine %charTop, %shiftScroller.rowsFromTop
		; This made no difference to FPS
		.if 0
		blatABlockIfPossible:
			cpx #8 : bcc cont
				lda %shiftScroller.colorPixel
				@splat8Rows %charTop
				txa : sec : sbc #8 : tax
				bra blatABlockIfPossible
		cont:
		.endif
		ldy #0
		jmp loopOuter
	done:
	sty %offset
.mend

.if 0
; Smarter version that works out whether it can blit a whole block.
; Turned out to be slightly slower than the original implementation!
; It might be more efficient to calculate the length of all three stages (activity in current block, number of full blocks, activity in final block) before starting.
; DONE: We should maintain numTillNextCharSwitch in a var/register, rather than recalculating each time.
.macro fillXRowsFrom charTop offset ; count
	tay
	stx %shiftScroller.numToDo ; really just a temp, not needed outside this macro
	lda #8 : sec : sbc %offset : sta %shiftScroller.numTillNextCharSwitch
	mainLoop:
	lda %shiftScroller.numTillNextCharSwitch
	; cpx #0 : beq noProbs
	cmp %shiftScroller.numToDo : bcs onlyDoAFew
	doToNextChar:
		; numToDo > 8-offset
		;; Alternative to the dex inside the loop:
		; sta %shiftScroller.numWillDo
		; lda %shiftScroller.numToDo : sec : sbc %shiftScroller.numWillDo : sta %shiftScroller.numToDo
		;; Backwards subtraction
		clc_implied : sbc %shiftScroller.numToDo : eor #255 : sta %shiftScroller.numToDo
		;; Note: if you restore dex, the stx above needs to come back inside mainLoop.
		tya : ldy #7
		loop1:
			sta (%charTop),y
			; dex   ; must be maintained, before we drop out of loop
			cpy %offset : beq outLoop1
			dey
			bra loop1
			; jmp loop1
			; cpy %offset : bcs loop1
		outLoop1:
		tay
		@progressLine %charTop, %shiftScroller.rowsFromTop
		lda #0 : sta %offset
		lda #8 : sta %shiftScroller.numTillNextCharSwitch
		bra mainLoop
	onlyDoAFew:
		; numToDo <= 8-offset
		tya : ldy %offset
		ldx %shiftScroller.numToDo
		loop2:
			cpx #0 : beq outLoop2
			sta (%charTop),y
			iny
			dex : bne loop2
		outLoop2:
		cpy #8 : bne noProbs
			@progressLine %charTop, %shiftScroller.rowsFromTop
			ldy #0
		noProbs:
		sty %offset
.mend
.endif

.if 0
; GAH! Stolen from plasma.spp
.context interlaceNum
.var out,tmp
interlaceNum:
.(
	ldy #0 : sty %out
	; We are only interested in the bottom 4 bits
	rol a : rol a : rol a : rol a
;	_loopY(loop4bits, #0, #4,
;		: rol a : pha
;		: bcs bitSet
;		: 	lda #0 : bcc bitNotSet
;		: bitSet:
;		: 	lda #1
;		: bitNotSet:
;		: sta %tmp
;		: lda %out : asl a : ora %tmp : asl a : ora %tmp : sta %out
;		: pla
;	)
	lda %out
	rts
.)
.ctxend
.endif

; New hopefully faster implementation
; TODO: Move this back into plasma!  Or better, push it to a library.
.context interlaceNum
.var out,tmp
interlaceNum:
.(
	sta %tmp
	lda #$ff   ; we will shift all these, but as we do C will always be set :)
	@doubleROR %tmp
	@doubleROR %tmp
	@doubleROR %tmp
	@doubleROR %tmp
	rts
.)
.ctxend

.macro doubleROR tmp
	ror %tmp : bcc addZero
	addOne:
		; sec_implied because of lda #$ff above :)
		@sec_implied : ror a : @sec_implied : ror a : @sec_implied : bcs doneAdd
	addZero:
		lsr a : lsr a
	doneAdd:
.mend

.context spankThePalette
.var logical, val
spankThePalette:
.(
	; We will use colors 1-8 for the text, and wobble them up and down.
	lda #8 : sta %logical
	loop:
		ldx colorWobble : lda sinLookup,x
		lsr a : lsr a : lsr a : lsr a ; : lsr a
		@invert_if_negative colorWobble
		clc : adc #16
		adc %logical
		; @slowMod 8
		and #7
		lsr a   ; 2-banded 0-3 (the only combination I can get to work!)
		clc : adc #1
		sta %val
		; NO fails! @setPalette %logical, %val
		@vdu 19
		lda %logical : jsr oswrch
		lda %val : jsr oswrch
		@vdu 0
		jsr oswrch
		jsr oswrch
		jsr oswrch
	dec %logical : bne loop
	rts
.)
.ctxend

.context shiftScroller
; vars which must be preserved during the main loop
.var2 plotPos  ; points to start of char
.var offset    ; y within char
.var block
.var offsetTop
.var numDone           ; Main loop for DOUBLE_SPEED - need not be ZP
.var rowsFromTop       ; Unit: char-rows not pixel-rows
.var sizeLO,sizeHI     ; The plotted height of each font pixel, fractional part in LO
.var currentRemainder  ; Frequently incremented by sizeLO
.var numToDo           ; TODO: May not be needed in final build
.var numWillDo         ;       Only used by the second @fillXRowsFrom implementation
.var numTillNextCharSwitch ;   Only used by the second @fillXRowsFrom implementation
.var colorPixel        ;       Used only by the first @fillXRowsFrom implementation
.var2 readingPos       ; Where we are currently reading from in the fontchar
shiftScroller:
.(

	;; Slow it down for debugging
	; lda frameCounter : and #7
	; cmp #0 : bne noSpecial

	; TODO IMPORTANT: Is this the best moment to change screen start?
	; I see some flickering in BeebEm but really need to test this on a Beeb!
	; If this is the beginning of the sweep perhaps we would do better waiting
	; for the end, so the hardware can flicker all it wants before the next
	; sweep starts.

	jsr spankThePalette

	.ifdef DOUBLE_SPEED
	lda #0 : sta %numDone
	loopDo:
	.endif

	clc : lda currentScrStart : adc #<offsetForBlit : sta %plotPos
	lda currentScrStart+1 : adc #>offsetForBlit : sta %plotPos+1
	; DONE: This will need to wrap back to top if it goes off bottom
	; but not only here - it may need to do that during loopBlocks!

	@checkWrapAround %plotPos

	lda #0 : sta %offset
	sta %rowsFromTop

	; Acquire offsetTop

	; If wobbling, first calculate vertical size of this column
	.ifdef WOBBLE_HEIGHT
	lda #0 : sta %sizeLO
	ldx sizeWobble : lda sinLookup,x ; [0,255]
	lsr a : @invert_if_x_negative ; [-128,+127]
	clc : adc #128 ; [0,255]
	clc
	ror a : ror %sizeLO ; [0,127]
	.if fontHeight <= 8
	clc : adc #120
	.else
	.if fontHeight <= 14
	clc : adc #40       ; for smaller fonts (can go to 64 but then text does not get narrow)
	.else
	clc : adc #32       ; [32,159]
	.endif
	.endif
	ror a : ror %sizeLO ; [16,79]
	ror a : ror %sizeLO ; [8,39]
	ror a : ror %sizeLO ; [4,19]
	ror a : ror %sizeLO ; [2,9]
	sta %sizeHI
	; _cursorTo(6,6)
	; lda %sizeHI
	; jsr print_hex
	; lda %sizeLO
	; jsr print_hex
	.endif
	; TODO CONSIDER: Why if we change adc #32 to adc #0 is there little change in the resulting text height?
	; I wanted to reduce the max height and see at what max the framerate might reach 50.
	; Also TODO: Later, we need not clear ALL the lines above the text, only 1 row above, because we know the line before has already been cleared!
	; No sleepy man, that is not guaranteed, due to the wobbling.
	; As for lines below the text, well we might reduce these a bit if we reduce the maxsize and calculate the resulting MAX_ROWS.
	; We *could* hold a lastTop and lastBottom marker count for each column, so when we next come around we do know what range is already clear and what range needs to be cleared.

	; Text waves up and down
	.ifdef WAVY_TEXT
	ldx textWobble : lda sinLookup,x
	lsr a : lsr a : lsr a
	@invert_if_negative textWobble
	clc : adc #32
	sta %offsetTop
	; CONSIDER: Maybe waviness should increase when size is low, so it will still visit the whole screen?
	.else
	lda #32 : sta %offsetTop
	.endif

	.ifdef WOBBLE_TOP_WITH_HEIGHT
	; For better alignment, we do it always, and don_t change sizeWobble until it is used.
	; ldx frameCounter+1 : beq noWobbleTop
		; Compensate for resizing
		.if 0
		; ldx sizeWobble : lda sinLookup,x
		lda sizeWobble : sec : sbc #8 : tax
		lda sinLookup,x
		; Later we use +/-4 but this gets multiplied by fontHeight
		; but we only want half of that.
		lsr a : lsr a : lsr a
		cpx #0 : @signed_byte_abs_fast
		; but we want to subtract!
		sec : sbc #1 : eor #255
		clc : adc #32
		clc : adc %offsetTop
		.endif
		; Move towards the middle of the screen somewhat
		lda %offsetTop : clc : adc #90 : sta %offsetTop
		; Now come back relative to fontHeight/2*sizeLO,HI
		lda #128 : sta %currentRemainder
		ldx #[fontHeight/2]
		; stx %tmpa
		; @multiply8to16 %tmpa, %sizeHI, %toSub, %toSub+1
		; ldx #[fontHeight/2] : stx %tmpa
		; @multiply8to16_fragA %tmpa, %sizeLO, %toSubLOmore
		; lda %toSubLOmore : @adcWord %toSub
		; @sub16from16 %cR/oT, %toSub, %cR/oT
		; TODO: This is a lazy multiplication - could be done faster
		loopTopWobble:
			lda %currentRemainder : sec : sbc %sizeLO : sta %currentRemainder
			lda %offsetTop : sbc %sizeHI : sta %offsetTop
		dex : bne loopTopWobble
		; _cursorTo(0,2)
		; lda %offsetTop : jsr print_hex
	; noWobbleTop:
	.endif

	; Now we have offsetTop
	; Clear all rows above the text 
	; First blocks of 8
	loopTop:
		lda %offsetTop : cmp #8 : bcc doneLoopTop
		sec : sbc #8 : sta %offsetTop
		@empty8Rows %plotPos
		jmp loopTop
	doneLoopTop:
	; Then deal with the remainder (<8)
	tax
	lda #0
	; TODO: The new fillXRowsFrom is overkill for this!
	@fillXRowsFrom %plotPos, %offset

	.if .defined(WOBBLE_HEIGHT) & ~.defined(WOBBLE_TOP_WITH_HEIGHT)
	lda #128 : sta %currentRemainder
	.endif

	lda #0 : sta %block
	loopBlocks:

		; Peek into our font to see if the current pixel is set or not
		; (readingPos is already set for us)
		.if fontWidth > 8
			lda %block
			.if fontWidth <= 16
			asl a ; multiply by 2 for font with 2 bytes per row (8<fontWidth<=16)
			.else
			;#error "Need moar code for fontWidth>16"
			.endif
			tay
		.else
			ldy %block
		.endif
		lda (%readingPos),y
		and readingBit
		beq gotBlockColor   ; got zero
			; Font pixel is set!
			; Choose the color

			; 7 in both pixels
			;lda #0b00111111

			; Choose something rainbow
			;lda %block : clc : adc textWobble
			;lda textWobble : lsr a : lsr a : lsr a : clc : adc %block

			.if 0
			; THIS LOOKS RUBBISH because we are writing in blocks of 8, so it jumps
			; We_d be better of just spamming VDU 19 ;]
			; Yeah that_s still gonna look horrid thanks to the blocks
			; I forget what this was supposed to do, but it looks fairly crazy :]
			ldx colorWobble : lda sinLookup,x
			; lda positionInText : clc : adc colorWobble : tax : lda sinLookup,x   ; positionInText changes rather slowly :P
			lsr a : lsr a : lsr a
			@invert_if_negative colorWobble
			clc : adc #32
			adc %block
			.endif

			; Vertical stripes:
			lda colorWobble : lsr a
			; Chunky diagonal stripes:
			clc : adc %block
			; sec : sbc %block ; flatter!

			.if 0
			; Horizontal stripes:
			; TODO CONSIDER: Optimizations here if needed.
			;   - Increment and maintain color with block instead of @slowMod.
			;   - Use lookup instead of interlaceNum routine.
			lda %block
			; Different phase on different chars
			; lda %readingPos : sec : sbc #1 : lsr a : asl a : asl a
			; clc : adc %block
			.endif

			lsr a ; : lsr a
			; @slowMod 7
			and #7
			clc : adc #1
			; Yay now bits need to be interlaced
			jsr interlaceNum

		gotBlockColor:

		; Vague attempt at coke-can (wrap text around cylinder) effect
		.if 0
		pha
		lda %block : asl a : asl a : asl a
		; clc : adc textWobble
		tax : lda sinLookup,x
		lsr a : lsr a : lsr a : lsr a : lsr a
		clc : adc #2
		tax
		pla
		.endif

		; Put number of lines to plot into X
		.ifdef WOBBLE_HEIGHT
		; Does not happen for first 256 frames
		; ldx frameCounter+1 : bne wobbleHeight
			; ldx #9 : jmp doneWobbleHeight
		; wobbleHeight:
		; Wobble height over time
		.if 0
			; Old method used sin table with offset phases to change some blocks slightly earlier than others, making it change more smoothly!
			pha
			lda sizeWobble
			sec : sbc %block
			sec : sbc %block
			tax
			lda sinLookup,x 
			; lsr a : lsr a : lsr a : lsr a : lsr a : lsr a ; [0,3]
			rol a : rol a : rol a : and #3 ; [0,3]
			cpx #0 : @signed_byte_abs_fast ; [-3,+3]
			clc : adc #6
			tax
			pla
		.endif
		tay
		ldx %sizeHI
		lda %currentRemainder : clc : adc %sizeLO : sta %currentRemainder
		bcc normalSize
			inx
		normalSize:
		tya
		doneWobbleHeight:
		.else
		ldx #10
		.endif

		@fillXRowsFrom %plotPos, %offset

		; TODO: Improve this bit so we can do fine-grained color changes
		; instead of ldx #6, we will need to prepare num to do of old color, and num to do of new color (and therefore prepare both colors).  if pixel is not set, we can just fill 6 black instead.
		; My goal is to have the color bands make an independent, or relative sine-wave.

	inc %block : lda %block : cmp #fontHeight : bne loopBlocks

	; Clear rows below the text (sometimes there is earlier crap there)
	.if 0
	lda #64 : sec : sbc %offsetTop
	tax
	lda #0
	@fillXRowsFrom %plotPos, %offset
	.endif

	; Clear remainder of current row
	lda #8 : sec : sbc %offset
	tax : lda #0 : @fillXRowsFrom %plotPos, %offset

	.if 0
	loopClear:
		; FAILURE here - there is no sensible way to compare them,
		; it seems we aren_t guaranteed to hit it spot on, sometimes we jump it!
		lda %plotPos+1 : cmp currentScrStart+1 : beq doneClear
		ldx #8 : lda #0 : @fillXRowsFrom %plotPos, %offset
		jmp loopClear
	doneClear:
	.endif

	; _cursorTo(0,0)
	; lda #MODE_ROWS : sec : sbc %rowsFromTop : jsr print_hex

	clearLoop:
		lda %rowsFromTop
		cmp #MODE_ROWS-1 : beq clearDone
		; ldx #8 : lda #0 : @fillXRowsFrom %plotPos, %offset
		; Faster?
		@empty8Rows %plotPos
		jmp clearLoop
	clearDone:

	; BUG TODO: We are still getting a few old bits left over :F
	; I think this may be due to the top - we often write a lot of black rows, but occasionally we write none.
	; Alternatively consider, when we wrap back around to the top, what does that mean in terms of clearing the last plots?
	; There is supporting evidence that it is a combination of two issues - it only happens occasionally, and sometimes a bit more so, presumably when two numbers get in phase. ^^

	; Will we move right one pixel in the font?

	.ifdef VARY_TEXT_WIDTH
		; Crude decision: always advance if current height is below a certain
		; threshold, otherwise advance every other column.  This makes large text
		; wider and small text thinner (x-scaling to correspond to y).  Despite the
		; granularity (width either 1 or 2, no inbetween), the human eye does not
		; really mind this at setting 5 (there is enough else going on!)
		; This looks better in terms of readability, but loses something in terms of flow.
		; Can go up to 8 to keep the text moving fast.
		lda %sizeHI : cmp #5 : bcc doChangeFontPos
		lda %numDone : cmp #0 : beq doChangeFontPos
		jmp noChangeFontPos
	.else
		.ifdef WIDER_TEXT
			.ifdef DOUBLE_SPEED
				lda %numDone : cmp #0 : bne noChangeFontPos
			.else
				; Does make things wider ... but that looks slower!
				; lda frameCounter : and #1 : bne noChangeFontPos
			.endif
		.endif
	.endif

	doChangeFontPos:
		lda readingBit : lsr a : sta readingBit
		cmp #0 : bne noChangeFontPos
			; Not firstBitToRead - that_s for the top byte only
			lda #128 : sta readingBit
			@decword %readingPos
			; lda #1 : @sbcWord %readingPos
			dec readingChar : bne noAdvanceChar
				jsr moveToNextCharacter
			noAdvanceChar:
	noChangeFontPos:

	; move right 1 "char" (aka 1 horizontal-screen-byte) aka 2 pixels (in mode le deux)
	@incword scrollRegisters
	lda #8 : @adcWord currentScrStart

	lda currentScrStart+1 : cmp #>screenEnd
	bne dontResetScrStart
		; lda #0 : sta scrollRegisters
		jsr resetScrollRegisters
	dontResetScrStart:

	lda frameCounter : and #7
	cmp #0 : bne noSpecial

		;@showFPS

		; _cursorTo(4,4)
		; lda scrollRegisters+1 : jsr print_hex
		; lda scrollRegisters : jsr print_hex

	noSpecial:

	inc textWobble
	; I quite like the colorWobble in phase with textWobble (it goes the opposite direction!).
	; inc colorWobble

	.ifdef WOBBLE_HEIGHT
		; lda %numDone : cmp #1 : bne noMoveSizeWobble ; Progress on second column only - causes old unwanted jaggies
		.ifdef SUPER_SQUIDGY
			lda frameCounter+1 : and #1 : beq noMoveSizeWobble
			; TODO: I want to slow down the squidginess more, to show off the text wobble for longer.  We could take frameCounter[0,1] and rol them right twice.  And the top with 3, then add to sizeWobbleLO.
			; lda frameCounter+1 : and #7
			; clc : adc sizeWobble : sta sizeWobble
			; But we want to add this halved, not this whole.  So we need sizeWobbleLO!
			; Simulated a 0/1 sizeWobbleLO with low bit of frameCounter.
			lda frameCounter+1 : ror a : and #3   ; #7 for super silly
			bcc noMinor
				tax
				;; One half speed
				lda frameCounter : ror a : bcc noMinorAdd
				;; One quarter speed (not!  hi still increases as normal)
				; lda frameCounter : and #3 : bne noMinorAdd
					inc sizeWobble
				noMinorAdd:
				txa
			noMinor:
			clc : adc sizeWobble : sta sizeWobble
			; Always add some fraction
			lda sizeWobbleLO : clc : adc #74 : sta sizeWobbleLO
			lda sizeWobble : adc #0 : sta sizeWobble
		.else
			lda frameCounter+1 : and #1 : beq noMoveSizeWobble
				inc sizeWobble
		.endif
		inc colorWobble
		noMoveSizeWobble:
	.endif

	.ifdef DOUBLE_SPEED
	inc %numDone : lda %numDone : cmp #2 : bne loopDo
	.endif

	; dec colorWobble

	; Set low byte (R13) register
	lda #13
	sta CRTC_ADDR
	lda scrollRegisters
	sta CRTC_DATA

	; Set high byte of HW scroll register
	lda #12
	sta CRTC_ADDR
	lda scrollRegisters+1
	sta CRTC_DATA

	rts

.)
.ctxend

.include "../lib/mos.s"

frameCounter
	.dword 0
