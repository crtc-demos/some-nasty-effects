	; Choose another SRAM bank if necessary.
	.alias use_sram_bank 4

old_lang:
	.byte 0

select_sram:
	.(
	pha
	lda $f4
	sta old_lang
	pla
	; must save to ram copy of ROMSEL first!
	sta $f4
	sta $fe30
	rts
	.)

select_old_lang:
	.(
	lda old_lang
	sta $f4
	sta $fe30
	rts
	.)

	; copy A * 256 bytes from $3000 to YX.
	; corrupts tmp1, tmp2

	.context copy_to_sram
	.var2 tmp1, tmp2

copy_to_sram:
	stx %tmp1
	sty %tmp1 + 1
	tax
	lda #<$3000
	sta %tmp2
	lda #>$3000
	sta %tmp2 + 1
	ldy #0
loop:
	lda (%tmp2),y
	sta (%tmp1),y
	iny
	bne loop
	inc %tmp2 + 1
	inc %tmp1 + 1
	dex
	bne loop
	rts
	.ctxend
