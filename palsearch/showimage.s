	.org $e00

	.temps $70..$82

	.macro crtc_write addr data
	lda #%addr
	sta CRTC_ADDR
	lda %data
	sta CRTC_DATA
	.mend
	
	.alias WITH_TUNE -1
	
entry_point:
	.(

	lda #1
	jsr mos_setmode
	;jsr mos_cursoroff

	@crtc_write 7, {#34}
	;@crtc_write 8, {#0b11000000}
	@crtc_write 8, {#0}

	lda #1
	jsr player_pulser

	.ifdef WITH_TUNE
	;; Nothingth pic
	@shadow_screen_in_ram
	@load_file_to nasty, $3000
	@main_screen_in_ram
	
	lda #<$3000
	sta %decompress_lz4.input
	lda #>$3000
	sta %decompress_lz4.input+1
	
	lda #<$3000
	sta %decompress_lz4.output
	lda #>$3000
	sta %decompress_lz4.output+1
	
	jsr decompress_lz4
	.endif

	;; First pic

	@shadow_screen_in_ram
	@load_file_to pic_0, $3000
	@main_screen_in_ram
	
	.ifdef WITH_TUNE
	lda #0
	sta timer
loop
	lda #19
	jsr osbyte
	inc timer
	bne loop
	.endif
	
	jsr regular_palette_black
	jsr do_decompress

	.ifdef WITH_TUNE

	jsr extract_palettes_and_blank

	jsr player_vsync_disable

	lda #BANK0
	sta $f4
	sta $fe30

	jsr initvsync

	jsr fade_in
	@wait_for $8fc0
	jsr fade_out
	
	jsr deinit_effect
	jsr regular_palette_black

	jsr player_vsync_enable
	
	;; Second pic
	
	@shadow_screen_in_ram
	@load_file_to pic_1, $3000
	@main_screen_in_ram
	
	jsr do_decompress
	jsr extract_palettes_and_blank
	
	jsr player_vsync_disable
	jsr initvsync
	
	jsr fade_in
	@wait_for $9140
	jsr fade_out
	
	jsr deinit_effect
	jsr regular_palette_black

	jsr player_vsync_enable

	;; Third pic
	
	@shadow_screen_in_ram
	@load_file_to pic_2, $3000
	@main_screen_in_ram
	
	jsr do_decompress
	jsr extract_palettes_and_blank
	
	jsr player_vsync_disable
	jsr initvsync
	
	jsr fade_in
	@wait_for $92c0
	jsr fade_out
	
	jsr deinit_effect
	jsr regular_palette_black

	jsr player_vsync_enable

	.else

	; *** debug code ***

	jsr extract_palettes_and_blank

	jsr initvsync

	jsr fade_in
	jsr fade_out

halt:
	jmp halt

	; *** debug code ends ***

	.endif

	lda #0
	jsr player_pulser
	@crtc_write 8, {#0b11000000}

	;; Run next effect
	
	ldx #<next_effect
	ldy #>next_effect
	jsr oscli
	
	rts
	.)

timer
	.byte 0

	.context do_decompress
do_decompress:
	lda #<$3000
	sta %decompress_lz4.input
	lda #>$3000
	sta %decompress_lz4.input+1
	
	lda #<$2b70
	sta %decompress_lz4.output
	lda #>$2b70
	sta %decompress_lz4.output+1
	
	jsr decompress_lz4

	rts
	.ctxend

	.macro fade_chunk pal_in pal_out fade_at fade_stop rownum intensity flipmask tmp tmp2
	; next rows
	ldy %fade_at
rown_loop
	lda #<[16+palette_copy]
	sta %pal_in
	lda #>[16+palette_copy]
	sta %pal_in+1
	
	lda #<$2b80
	sta %pal_out
	lda #>$2b80
	sta %pal_out+1

	sty %rownum
	
	tya
	sec
	sbc %fade_at
	eor #%flipmask
	sta %intensity

	ldx #0
colour_loop
	lda (%pal_in),y
	sta %tmp
	and #$0f
	eor #7
	tay
	lda inv_intens_pal,y
	sec
	sbc %intensity
	bvc skip
	eor #$80
skip:
	bpl no_clamp
	lda #0
no_clamp
	tay
	lda intens_pal,y
	sta %tmp2
	lda %tmp
	and #$f0
	ora %tmp2
	ldy %rownum
	sta (%pal_out),y
	
	lda %pal_in
	clc
	adc #128
	sta %pal_in
	bcc nohi_1
	inc %pal_in+1
nohi_1

	lda %pal_out
	clc
	adc #128
	sta %pal_out
	bcc nohi_2
	inc %pal_out+1
nohi_2

	inx
	cpx #9
	bne colour_loop

	iny
	cpy %fade_stop
	bne rown_loop
	.mend

	.context regular_palette_black
regular_palette_black:
	ldx #0
loop
	txa
	asl a
	asl a
	asl a
	asl a
	ora #7
	sta PALCONTROL
	inx
	cpx #16
	bne loop
	rts
	.ctxend

	.context extract_palettes_and_blank
extract_palettes_and_blank
	@copy_block_imm palette_copy, $2b70, 9*128+16
	@zero_block_imm $2b70, 9*128+16
	rts
	.ctxend

	.context fade_in
	.var2 pal_in, pal_out
	.var tmp, rownum, colournum, intensity, tmp2, fade_at, fade_stop
fade_in
	; zero'th row.
	ldx #0
row0_loop
	lda palette_copy,x
	sta $2b70,x
	inx
	cpx #16
	bne row0_loop

	jsr waitforvsync
	
	lda #0
	sta %fade_at
fade_loop
	
	.(
	lda %fade_at
	clc
	adc #8
	cmp #127
	bcc no_clip
	lda #127
no_clip
	sta %fade_stop
	.)
	
	@fade_chunk %pal_in, %pal_out, %fade_at, %fade_stop, %rownum, %intensity, 0, %tmp, %tmp2

	jsr waitforvsync

	inc %fade_at
	lda %fade_at
	cmp #127
	bcc fade_loop

	rts
	.ctxend

	.context fade_out
	.var2 pal_in, pal_out
	.var tmp, rownum, colournum, intensity, tmp2, fade_at, fade_stop
fade_out
	; zero'th row.
	ldx #0
row0_loop
	lda palette_copy,x
	and #$f0
	ora #7
	sta $2b70,x
	inx
	cpx #16
	bne row0_loop

	jsr waitforvsync
	
	lda #0
	sta %fade_at
fade_loop
	
	.(
	lda %fade_at
	clc
	adc #8
	cmp #127
	bcc no_clip
	lda #127
no_clip
	sta %fade_stop
	.)
	
	@fade_chunk %pal_in, %pal_out, %fade_at, %fade_stop, %rownum, %intensity, 7, %tmp, %tmp2

	jsr waitforvsync

	inc %fade_at
	lda %fade_at
	cmp #120
	bcc fade_loop

	@zero_block_imm $2b80+120, 8
	@zero_block_imm $2c00+120, 8
	@zero_block_imm $2c80+120, 8
	@zero_block_imm $2d00+120, 8
	@zero_block_imm $2d80+120, 8
	@zero_block_imm $2e00+120, 8
	@zero_block_imm $2e80+120, 8
	@zero_block_imm $2f00+120, 8
	@zero_block_imm $2f80+120, 8

	rts
	.ctxend

palette_copy
	.dsb 9*128+16,0

	.context waitforvsync
waitforvsync
	.(
	lda framectr
wait_for_vsync
	cmp framectr
	beq wait_for_vsync

	phy
	phx
	jsr player_poll
	plx
	ply

	rts
	.)
	.ctxend

	.macro copy_block_imm dst src len
	lda #<%dst
	sta %copy_block.outptr
	lda #>%dst
	sta %copy_block.outptr+1
	lda #<%src
	sta %copy_block.inptr
	lda #>%src
	sta %copy_block.inptr+1
	lda #<%len
	sta %copy_block.len
	lda #>%len
	sta %copy_block.len+1
	jsr copy_block
	.mend

	.context copy_block
	.var2 inptr, outptr, len
copy_block

copy_pages
	lda %len+1
	beq last_part

	ldy #0
page_loop
	lda (%inptr),y
	sta (%outptr),y
	iny
	bne page_loop
	
	dec %len+1
	inc %inptr+1
	inc %outptr+1
	bra copy_pages

last_part
	ldy #0
	cpy %len
	beq done_copy
subpage_loop
	lda (%inptr),y
	sta (%outptr),y
	iny
	cpy %len
	bne subpage_loop
done_copy
	
	rts
	.ctxend

	.macro zero_block_imm dst len
	lda #<%dst
	sta %zero_block.outptr
	lda #>%dst
	sta %zero_block.outptr+1
	lda #<%len
	sta %zero_block.len
	lda #>%len
	sta %zero_block.len+1
	jsr zero_block
	.mend

	.context zero_block
	.var2 outptr, len
zero_block

zero_pages
	lda %len+1
	beq last_part

	ldy #0
page_loop
	lda (%outptr),y
	and #$f0
	ora #$07
	sta (%outptr),y
	iny
	bne page_loop
	
	dec %len+1
	inc %outptr+1
	bra zero_pages

last_part
	ldy #0
	cpy %len
	beq done_zero
subpage_loop
	lda (%outptr),y
	and #$f0
	ora #$07
	sta (%outptr),y
	iny
	cpy %len
	bne subpage_loop
done_zero
	
	rts
	.ctxend

	.context wait
	.var2 time
wait:
loop_a_while:
	jsr waitforvsync

	sei
	lda $50
	sta playpos_copy
	lda $51
	sta playpos_copy+1
	cli
	
	@if_leu_abs playpos_copy, %time, loop_a_while

	rts
	.ctxend
	
	.macro wait_for time
	lda #<%time
	sta %wait.time
	lda #>%time
	sta %wait.time+1
	jsr wait
	.mend

playpos_copy
	.word 0

nasty
	.asc "z.nasty",13

pic_0
	.asc "z.mduck",13
pic_1
	.asc "z.frog",13
;pic_2
;	.asc "z.cham",13
pic_2
	.asc "z.parrot",13

intens_pal
	.byte 0^7
	.byte 4^7
	.byte 1^7
	.byte 5^7
	.byte 2^7
	.byte 6^7
	.byte 3^7
	.byte 7^7
inv_intens_pal
	.byte 0
	.byte 2
	.byte 4
	.byte 6
	.byte 1
	.byte 3
	.byte 5
	.byte 7

next_effect
	.asc "plasma",13

curs1:
	.word 0
curs2:
	.word 0
curs3:
	.word 0
frameno:
	.word 0
vsync_ours:
	.byte 0xff

	.alias FROM_SHADOW_RAM -1

	.include "lz4.s"
	.include "../lib/mos.s"
	.include "../lib/sram.s"
	.include "../lib/load.s"
	.include "../lib/srambanks.s"
	.include "../lib/cmp.s"
	.include "../lib/player.s"

initvsync
	.(
	sei

        lda $204
        ldx $205
        sta oldirq1v
        stx oldirq1v+1

        ; Set one-shot mode for timer 1
        ;lda USR_ACR
        ;and #$0b00111111
        ;sta USR_ACR
        
        ; Sys VIA CA1 interrupt on positive edge
	lda SYS_PCR
	sta old_sys_pcr
        lda #4
        sta SYS_PCR

	lda SYS_ACR
	sta old_sys_acr
	lda #0
	sta SYS_ACR
       
        ; Point at IRQ handler
        lda #<irq1
        ldx #>irq1
        sta $204
        stx $205

        ; Enable Usr timer 1 interrupt
        ;lda #$c0
        ;sta USR_IER
	
	; Disable USR_IER bits
	;lda #0b00111111
	;sta USR_IER
        
	lda USR_IER
	sta old_usr_ier
	
	lda SYS_IER
	sta old_sys_ier
	
        ; Enable Sys CA1 interrupt.
        lda #0b10000010
        sta SYS_IER
        
	; Disable Sys CB1, CB2, timer1 interrupts
	; Note turning off sys timer1 interrupt breaks a lot of stuff!
	lda #0b01011000
	; CB1 & CB2 only
	;lda #0b00011000
	; or everything!
	;lda #0b01111101
	sta SYS_IER

        cli
        
        rts
	.)

deinit_effect
	.(
	sei
	lda #0b01000000
	sta USR_IER
	
	lda old_sys_pcr
	sta SYS_PCR
	
	lda old_sys_acr
	sta SYS_ACR
	
	lda #$7f
	sta SYS_IER
	lda old_sys_ier
	sta SYS_IER

	lda #$7f
	sta USR_IER
	lda old_usr_ier
	sta USR_IER

	lda #<1000
	sta SYS_T1C_L
	lda #>1000
	sta SYS_T1C_H
	
	lda #<10000
	sta SYS_T1L_L
	lda #>10000
	sta SYS_T1L_H
	
	lda oldirq1v
	sta $204
	lda oldirq1v+1
	sta $205
	cli
	
	rts
	.)

old_sys_ier
	.byte 0
oldirq1v
	.word 0
old_sys_pcr
	.byte 0
old_sys_acr
	.byte 0
old_usr_ier
	.byte 0

.alias framectr $8e

.alias index $8f

irq1:	.(
	lda $fc
        pha

        ; Is it our User VIA timer1 interrupt?
        lda #64
        bit USR_IFR
        bne timer1
        ; Is it our System VIA CA1 interrupt?
	lda #2
        bit SYS_IFR
        bne vsync
        
        pla
	sta $fc
        jmp (oldirq1v)

timer1
	; Clear interrupt
	lda USR_T1C_L

	phx
	phy

	; This is supposed to happen within the h-blanking period.
palette_write:
	ldx #0b11100111
	ldy #0b11110111
	lda #0b00000111 : sta PALCONTROL	    ; 0
	lda #0b00010111 : sta PALCONTROL	    ; 1
	lda #0b00100111 : sta PALCONTROL	    ; 2
	lda #0b00110111 : sta PALCONTROL	    ; 3
	lda #0b01000111 : sta PALCONTROL	    ; 4
	lda #0b01010111 : sta PALCONTROL	    ; 5
	lda #0b01100111 : sta PALCONTROL	    ; 6
	stx PALCONTROL				    ; 7
	sty PALCONTROL				    ; 8

	; Queue up the next colour: it doesn't matter if this takes a little
	; longer.
	ldx index

	.(
	cpx #126
	bne not_penultimate
	lda #255
	sta USR_T1L_L
	sta USR_T1L_H
	bra not_last
not_penultimate
	cpx #127
	bne not_last
	; Disable usr timer1 interrupt
	lda #0b01000000
	sta USR_IER
	bra exit_irq
not_last
	.)

	lda $2f00,x : sta palette_write+1
	lda $2f80,x : sta palette_write+3
	lda $2b80,x : sta palette_write+5
	lda $2c00,x : sta palette_write+10
	lda $2c80,x : sta palette_write+15
	lda $2d00,x : sta palette_write+20
	lda $2d80,x : sta palette_write+25
	lda $2e00,x : sta palette_write+30
	lda $2e80,x : sta palette_write+35

	inx
	stx index

exit_irq:
	ply
	plx
	pla
	sta $fc
	rti

	.alias fliptime 64 * 38 + 29

vsync
	phx
	phy

	; Clear interrupt
	;lda USR_T1C_L

        ; Trigger after 'fliptime' microseconds
        lda #<fliptime
        sta USR_T1C_L
        lda #>fliptime
        sta USR_T1C_H

	lda #<[64*2-2]
	sta USR_T1L_L
	lda #>[64*2-2]
	sta USR_T1L_H

	; Clear IFR
	lda SYS_ORA
	
	; Generate stream of interrupts
	lda USR_ACR
	and #0b00111111
	ora #0b01000000
	sta USR_ACR
       
	lda #1
	sta index

	; Enable usr timer1 interrupt
	lda #0b11000000
	sta USR_IER

	.(
	ldx #15
firstrow
	lda $2b70,x
	sta PALCONTROL
	dex
	bpl firstrow
	.)

	lda $2f00 : sta palette_write+1
	lda $2f80 : sta palette_write+3
	lda $2b80 : sta palette_write+5
	lda $2c00 : sta palette_write+10
	lda $2c80 : sta palette_write+15
	lda $2d00 : sta palette_write+20
	lda $2d80 : sta palette_write+25
	lda $2e00 : sta palette_write+30
	lda $2e80 : sta palette_write+35

	inc framectr

	; gtfo
	ply
	plx
	pla
	sta $fc
	rti
	; jmp (oldirq1v)
	.)
