	.org $e03

	.temps $70..$7f

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
	jsr mos_cursoroff

	@crtc_write 8, {#0b11000000}
	@crtc_write 7, {#34}

	;; First pic

	@shadow_screen_in_ram
	@load_file_to pic_0, $3000
	@main_screen_in_ram
	
	jsr do_decompress

	.ifdef WITH_TUNE
	jsr player_vsync_disable

	lda #BANK0
	sta $f4
	sta $fe30
	.endif

	jsr initvsync

	@wait_for $8b00
	
	jsr deinit_effect
	jsr player_vsync_enable
	
	;; Second pic
	
	@shadow_screen_in_ram
	@load_file_to pic_1, $3000
	@main_screen_in_ram
	
	jsr do_decompress
	
	jsr player_vsync_disable
	jsr initvsync
	
	@wait_for $8c00
	
	jsr deinit_effect
	jsr player_vsync_enable

	;; Third pic
	
	@shadow_screen_in_ram
	@load_file_to pic_2, $3000
	@main_screen_in_ram
	
	jsr do_decompress
	
	jsr player_vsync_disable
	jsr initvsync
	
	@wait_for $8d00
	
	jsr deinit_effect
	jsr player_vsync_enable

	;; Fourth pic
	
	@shadow_screen_in_ram
	@load_file_to pic_3, $3000
	@main_screen_in_ram
	
	jsr do_decompress
	
	jsr player_vsync_disable
	jsr initvsync
	
	@wait_for $8e00
	
	jsr deinit_effect
	jsr player_vsync_enable

	;; Run next effect
	
	ldx #<next_effect
	ldy #>next_effect
	jsr oscli
	
	rts
	.)

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

	.context wait
	.var2 time
wait
	.(
loop_for_a_bit
	lda framectr

	.(
wait_for_vsync
	cmp framectr
	beq wait_for_vsync
	.)

	.ifdef WITH_TUNE
	jsr player_poll
	.endif

	sei
	lda $50
	sta playpos_copy
	lda $51
	sta playpos_copy+1
	cli
	
	@if_leu_abs playpos_copy, %time, loop_for_a_bit
	.)
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

pic_0
	.asc "z.mduck",13
pic_1
	.asc "z.frog",13
pic_2
	.asc "z.cham",13
pic_3
	.asc "z.parrot",13

intens_pal
	.byte 0
	.byte 4
	.byte 1
	.byte 5
	.byte 2
	.byte 6
	.byte 3
	.byte 7
inv_intens_pal
	.byte 0
	.byte 2
	.byte 5
	.byte 6
	.byte 1
	.byte 3
	.byte 5
	.byte 7

next_effect
	.asc "demo",13

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

	.alias fliptime 64 * 38 + 28

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
