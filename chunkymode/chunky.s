	.org $e00

	.alias SCREENSTART $7C00
	.temps $70..$7f

start:
	.(
	lda #2
	jsr mos_setmode
	jsr setup
	lda #19
	jsr oswrch
	lda #15
	jsr oswrch
	lda #7
	jsr oswrch
	lda #0
	jsr oswrch
	jsr oswrch
	jsr oswrch
	jsr filltest
	rts
	.)

	.include "../lib/mos.s"

	; Set CRTC register A to X.
crtc_write:
	.(
	sta CRTC_ADDR
	stx CRTC_DATA
	rts
	.)

setup
	.(
	; set start address
	lda #>SCREENSTART
	sec
	sbc #$74
	eor #$20
	tax
	lda #12
	jsr crtc_write
	lda #13
	ldx #<SCREENSTART
	jsr crtc_write
	
	; scan lines per character
	lda #9
	ldx #5
	jsr crtc_write
	
	; horizontal displayed
	lda #1
	ldx #73
	jsr crtc_write
	
	; vertical displayed
	lda #6
	ldx #28
	jsr crtc_write
	
	rts
	.)

	.context filltest
	.var2 ptr
filltest:
	lda #<SCREENSTART
	sta %ptr
	lda #>SCREENSTART
	sta %ptr+1
	ldy #0
loop:
	tya
	and #1
	beq zero
	lda #$ff
zero:
	sta (%ptr),y

	pha
	phx
	phy
	lda #19
	jsr osbyte
	ply
	plx
	pla

	iny
	bne loop
	
	inc %ptr+1
	lda %ptr+1
	cmp #$80
	bne loop
	
	rts
	.ctxend
