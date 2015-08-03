	.org $e00
	
	.temps $70..$7f
	
	.alias FROM_SHADOW_RAM -1
	
start:
	lda #1
	jsr mos_setmode

	@shadow_screen_in_ram

	@load_file_to filename, $3000
	
	@main_screen_in_ram
	
	lda #12
	jsr osbyte
	
	lda #<$3000
	sta %decompress_lz4.input
	lda #>$3000
	sta %decompress_lz4.input+1

	lda #<$3000
	sta %decompress_lz4.output
	lda #>$3000
	sta %decompress_lz4.output+1
	
	jsr decompress_lz4
	
	jsr select_old_lang
	
	rts

filename:
	.asc "z.sprdmp",13

	.include "lz4.s"
	.include "../lib/mos.s"
	.include "../lib/sram.s"
	.include "../lib/srambanks.s"
	.include "../lib/load.s"
	.include "../lib/cmp.s"
