; Load a file into memory.

	; load a file named YX to load_address.
load_file:
	.(
	stx osfile_blk
	sty osfile_blk + 1
	stz osfile_blk + 6
	lda load_address
	sta osfile_blk + 2
	lda load_address + 1
	sta osfile_blk + 3
	ldx #<osfile_blk
	ldy #>osfile_blk
	lda #$ff
	jmp osfile
	.)

osfile_blk:
	.dsb 18,0

load_address:
	.word 0x0

	; Helper macro for loading a file. Call as:
	;   @load_file_to ptr_to_filename, 0xfoobar

	.macro load_file_to filename address
	lda #<%address
	sta load_address
	lda #>%address
	sta load_address + 1
	ldx #<%filename
	ldy #>%filename
	jsr load_file
	.mend

	.macro load_file_via filename address
	lda %address
	sta load_address
	lda %address + 1
	sta load_address + 1
	ldx #<%filename
	ldy #>%filename
	jsr load_file
	.mend
