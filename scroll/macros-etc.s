
; It_s rather ugly with the rts, but it_s also unneccessary to use a jmp.

.macro startScreenHack setupHack doPlot

	; This macro does nothing if the effect/hack is being imported into a demo.
	.ifndef _LOADED_DEMO2LIB
	; However CONSIDER TODO: it could add its event handler to the global event array.
	; But we will still need to call this routine for that to happen.

	jsr %setupHack

	.ifdef SHOW_FPS
	jsr clearClock
	.endif

	@interceptEvent 4, oldVector, myEvent

	rts

	; oldVector:    .word 0x0000
	oldVector:    nop : nop

	myEvent:
		; vgmplayer
		; jsr $e06

		jsr %doPlot

		@incdword frameCounter

		; TODO: if we make the above loop 40x25 instead of 256*4, we could include the setupColors in that loop
		; jsr setupColors
		; OK now the screenblit only does the centre of the screen, so the color chars on the left are unaffected

		;; Now assumed to be done by hack
		; #ifdef SHOW_FPS
		; jsr showfps
		; #endif

		leaveEvent:

		rts

	.endif

.mend

.macro vdu c
	lda #%c
	jsr oswrch
.mend

.macro newline
	jsr osnewl
.mend

.macro mode n
	@vdu 22
	@vdu %n
.mend

.macro hideCursor
	@vdu 23
	@vdu 1
	; 8 x vdu 0
	lda #0
	ldy #8
	hideCursorLoop:
		jsr oswrch
	dey : bne hideCursorLoop
.mend

.macro adcWord v
	clc : adc %v : sta %v : bcc doneAdc
	inc %v+1
	doneAdc:
.mend

.macro incword v
	inc %v : bne skipHi
		inc %v+1
	skipHi:
.mend

.macro invert_if_negative num
	; Inverts the sign of the signed byte in the accumulator, if num is negative (has top bit set).
	; A <- sgn(num) * A, !X, A e [0-127]
	ldx %num : cpx #128 : bcc noInvert
		;; We know carry is set!
		sec_implied : sbc #1 : eor #255
	noInvert:
.mend

.macro invert_if_x_negative
	cpx #0 : @signed_byte_abs_fast
.mend

; Inverts the sign of the signed byte in the accumulator, if it is negative.
; In other words makes it positive :P
.macro signed_byte_abs
	cmp #128 : bcc noInvert
		sec_implied : sbc #1 : eor #255
	noInvert:
.mend

.macro decword v
	; TODO: faster?!
	; dec %v : lda %v : cmp #255 : bne skipHi
	; We can speed up half the cases, whilst making the other half longer.
	; We save 2 instructions when we skip, gain 1 when we don_t, net win.
	dec %v : bpl skipHi : lda %v : cmp #255 : bne skipHi
		dec %v+1
	skipHi:
.mend

.macro signed_byte_abs_fast
	bpl noInvert
		sec : sbc #1 : eor #255
	noInvert:
.mend

.macro simpleInterceptEvent eventNum oldVector eventHandler

	.ifdef RESPECT_EXISTING_HANDLERS
	lda $220 : sta %oldVector
	lda $221 : sta %oldVector+1
	.endif

	lda #<considerEvent : sta $220
	lda #>considerEvent : sta $221
	lda #14 : ldx #%eventNum : jsr osbyte

.mend

.macro interceptEvent eventNum oldVector eventHandler

	@simpleInterceptEvent %eventNum, %oldVector, considerEvent

	jmp skipEventHandler

	considerEvent:
		php
		cmp #%eventNum
		bne eventHandlerOut
		txa : pha

		.ifdef KEEP_INTERRUPTS
			;; Continue to let events be processed.  But prevent our own event
			;; from causing a fresh run while we are running!
			lda alreadyRunning : cmp #0 : bne skipEvent
			lda #1 : sta alreadyRunning
			cli
		.endif

		redoEvent:
		jsr %eventHandler

		.ifdef LOOP_IN_FOREGROUND
			; Most of the time we repeat the event call
			; but we still make occasional dropouts to allow BASIC to run slowly
			@getRandom : cmp #17 : bne redoEvent
		.endif

		.ifdef KEEP_INTERRUPTS
			sei
			lda #0 : sta alreadyRunning
			skipEvent:
		.endif

		pla : tax
		lda #%eventNum
	eventHandlerOut:
		plp
		.ifdef RESPECT_EXISTING_HANDLERS
		jmp (%oldVector)
		.else
		rts
		.endif

	skipEventHandler:
.mend

.macro incdword v
	inc %v   : bne doneInc
	inc %v+1 : bne doneInc
	inc %v+2 : bne doneInc
	inc %v+3
	doneInc:
.mend

; Example: @addWordToWord mem2_var, 123
.macro addWordToWord w word
	clc : lda %w : adc #<%word : sta %w
	lda %w+1 : adc #>%word : sta %w+1
.mend
