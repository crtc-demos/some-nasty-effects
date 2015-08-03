	.ifdef FROM_SHADOW_RAM

	.macro main_screen_in_ram
	lda ACCCON
	and #~4
	sta ACCCON
	.mend
	
	.macro shadow_screen_in_ram
	lda ACCCON
	ora #4
	sta ACCCON
	.mend
	
	.else
	
	.macro main_screen_in_ram
	.mend
	
	.macro shadow_screen_in_ram
	.mend
	
	.endif

	.context decompress_lz4
	.var2 input, output
	.var2 inptr, blk_end
	.var2 literal_length, match_pos, match_length
	.ifdef FROM_SHADOW_RAM
	.var tmp
	.endif
decompress_lz4:

	lda %input
	clc
	adc #7
	sta %inptr
	lda %input+1
	adc #0
	sta %inptr+1

	@shadow_screen_in_ram

do_block:
	lda (%inptr)
	clc
	adc %inptr
	sta %blk_end
	ldy #1
	lda (%inptr),y
	clc
	adc %inptr+1
	sta %blk_end+1

	lda %inptr
	clc
	adc #4
	sta %inptr
	.(
	bcc nohi
	inc %inptr+1
nohi:	.)

get_token:
	lda (%inptr)
	tax
	lsr a
	lsr a
	lsr a
	lsr a

	sta %literal_length
	stz %literal_length+1

	ldy #1
	cmp #15
	bcc got_lit_length
add_more_length
	lda (%inptr),y
	pha
	clc
	adc %literal_length
	sta %literal_length
	.(
	bcc nohi
	inc %literal_length+1
nohi:	.)
	iny
	pla
	cmp #255
	beq add_more_length
got_lit_length
		
	; Bump up input pointer by Y bytes.
	tya
	clc
	adc %inptr
	sta %inptr
	.(
	bcc nohi
	inc %inptr+1
nohi:	.)
	
	.(
	; Now copy LITERAL_LENGTH bytes from INPTR to OUTPUT.
copy_loop:
	lda %literal_length+1
	beq last_part
	ldy #0
copy_page
	lda (%inptr),y
	.ifdef FROM_SHADOW_RAM
	sta %tmp
	.endif
	
	@main_screen_in_ram
	
	.ifdef FROM_SHADOW_RAM
	lda %tmp
	.endif
	sta (%output),y
	
	@shadow_screen_in_ram
	
	iny
	bne copy_page
	
	dec %literal_length+1
	inc %inptr+1
	inc %output+1
	bra copy_loop

last_part
	ldy #0
copy_last_part
	cpy %literal_length
	beq done_last_part
	lda (%inptr),y
	.ifdef FROM_SHADOW_RAM
	sta %tmp
	.endif

	@main_screen_in_ram

	.ifdef FROM_SHADOW_RAM
	lda %tmp
	.endif
	sta (%output),y

	@shadow_screen_in_ram

	iny
	bra copy_last_part
done_last_part
	.)

	; Add last sub-page amount to input & output pointers
	lda %inptr
	clc
	adc %literal_length
	sta %inptr
	.(
	bcc nohi
	inc %inptr+1
nohi:	.)

	lda %output
	clc
	adc %literal_length
	sta %output
	.(
	bcc nohi
	inc %output+1
nohi:	.)

	; Now handle matches.

	lda %output
	sec
	sbc (%inptr)
	sta %match_pos
	lda %output+1
	ldy #1
	sbc (%inptr),y
	sta %match_pos+1

	txa
	and #15
	clc
	adc #4
	sta %match_length
	stz %match_length+1
	
	ldy #2
	cmp #19
	bcc got_match_length
add_more_matchlen
	lda (%inptr),y
	pha
	clc
	adc %match_length
	sta %match_length
	.(
	bcc nohi
	inc %match_length+1
nohi:	.)
	iny
	pla
	cmp #255
	beq add_more_matchlen
got_match_length

	; Bump up input pointer by Y bytes.
	tya
	clc
	adc %inptr
	sta %inptr
	.(
	bcc nohi
	inc %inptr+1
nohi:	.)

	@main_screen_in_ram
	
	; Copy MATCH_LENGTH bytes from MATCH_POS to OUTPUT.
	.(
copy_loop
	lda %match_length+1
	beq last_part
	ldy #0
copy_page
	lda (%match_pos),y
	sta (%output),y
	iny
	bne copy_page
	
	dec %match_length+1
	inc %match_pos+1
	inc %output+1
	bra copy_loop

last_part
	ldy #0
copy_last_part
	cpy %match_length
	beq done_last_part
	lda (%match_pos),y
	sta (%output),y
	iny
	bra copy_last_part
done_last_part
	.)

	@shadow_screen_in_ram

	; Add last sub-page amount to output pointer
	lda %output
	clc
	adc %match_length
	sta %output
	.(
	bcc nohi
	inc %output+1
nohi:	.)

	@if_ltu_abs %inptr, %blk_end, get_token

	@main_screen_in_ram

	rts
	.ctxend
