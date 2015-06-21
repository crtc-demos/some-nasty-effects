	.org $e00

	.alias SCREENSTART $7C00
	.temps $70..$7f

	.alias which_irq $60

start:
	.(
	lda #1
	jsr mos_setmode

	@load_file_to rings, 0x3000

	; This appears to realign the ULA's 1MHz output. Who knows how!
	lda #19
	jsr osbyte
	sei
	lda #$08
	sta ULACONTROL
	lda #$18
	sta ULACONTROL
	cli

	lda #BANK0
	jsr select_sram
	
	ldx #<$8000
	ldy #>$8000
	lda #64
	jsr copy_to_sram

	jsr setup

	jsr set_palette
	jsr expand_colour_bytes
	
	jsr initvsync
	;jsr filltest
	jsr plasma_loop
stuck:
	jmp stuck
	jsr deinit_effect
	rts
	.)

rings
	.asc "rings",13

	.include "../lib/mos.s"
	.include "../lib/sram.s"
	.include "../lib/srambanks.s"
	.include "../lib/load.s"

	.include "sintab.s"

	; Set CRTC register A to X.
crtc_write_ax:
	.(
	sta CRTC_ADDR
	stx CRTC_DATA
	rts
	.)

	.macro crtc_write addr data
	lda #%addr
	sta CRTC_ADDR
	lda %data
	sta CRTC_DATA
	.mend

setup
	.(
	; set start address
	lda #>SCREENSTART
	sec
	sbc #$74
	eor #$20
	tax
	lda #12
	jsr crtc_write_ax
	lda #13
	ldx #<SCREENSTART
	jsr crtc_write_ax
	
	; scan lines per character
	lda #9
	ldx #5
	jsr crtc_write_ax

	; these two retain 304 total lines (50*6 + 4)
	; vertical total
	lda #4
	ldx #50
	jsr crtc_write_ax
	
	; vertical total adjust
	lda #5
	ldx #0
	jsr crtc_write_ax
	
	; horizontal displayed
	lda #1
	ldx #64
	jsr crtc_write_ax
	
	; vertical displayed
	lda #6
	ldx #32
	jsr crtc_write_ax
	
	; vertical sync position
	lda #7
	ldx #49
	jsr crtc_write_ax
	
	lda #8
	ldx #0b00000000
	jsr crtc_write_ax
	
	lda #2
	ldx #90
	jsr crtc_write_ax
	
	;lda #154
	;ldx #0b11011100
	;jsr osbyte
	
	rts
	.)

	.alias char_rows 6
	.alias half_rows 16

	.alias new_frame 64*char_rows*10 - 64*2 + 24
	.alias shadow_switch 64*char_rows*half_rows - 64*3
	.alias mid_2nd_frame 64*char_rows*[half_rows/2]

irq1:
	.(
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

	lda which_irq
	beq first_after_vsync
	cmp #1
	beq second_frame_start

	; This is the middle of the 2nd frame (lower part of the screen).
	; Set up registers right for it.
	@crtc_write 4, {#52-half_rows-1}
	@crtc_write 6, {#half_rows}
	@crtc_write 7, {#26}

	; disable usr timer1 interrupt
	lda #0b01000000
	sta USR_IER

	;lda #0b00000111 ^ 5 : sta PALCONTROL

	bra next_irq
	
first_after_vsync
	; We're somewhere at the start of the top frame: make sure registers
	; are set properly.
	@crtc_write 4, {#half_rows-1}
	@crtc_write 6, {#half_rows}
	@crtc_write 7, {#255}

	;lda #0b00000111 ^ 4 : sta PALCONTROL

	bra next_irq

second_frame_start
	lda #<mid_2nd_frame
	sta USR_T1C_L
	lda #>mid_2nd_frame
	sta USR_T1C_H

	; Screen uses shadow RAM
	lda ACCCON
	ora #1
	sta ACCCON

	;lda #0b00000111 ^ 3 : sta PALCONTROL

next_irq
	inc which_irq

	pla
	sta $fc
	rti

vsync
	; Clear (timer1) interrupt
	lda USR_T1C_L

	lda #<new_frame
	sta USR_T1C_L
	lda #>new_frame
	sta USR_T1C_H

	lda #<shadow_switch
	sta USR_T1L_L
	lda #>shadow_switch
	sta USR_T1L_H

	; Generate stream of interrupts
	lda USR_ACR
	and #0b00111111
	ora #0b01000000
	sta USR_ACR

	; Enable usr timer1 interrupt
	lda #0b11000000
	sta USR_IER

	; Clear IFR
	lda SYS_ORA
	
	;lda #0b00000111 ^ 1 : sta PALCONTROL

	; Screen uses main RAM
	lda ACCCON
	and #~1
	sta ACCCON

	lda #0
	sta which_irq

	pla
	sta $fc
	rti
	.)

oldirq1v
	.word 0
old_usr_ier
	.byte 0
old_sys_ier
	.byte 0
old_sys_acr
	.byte 0
old_sys_pcr
	.byte 0

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
        
	lda USR_IER
	sta old_usr_ier
	
        ; Sys VIA CA1 interrupt
	lda SYS_PCR
	sta old_sys_pcr
	lda #4
        sta SYS_PCR

	; This removes jitters, but stops the keyboard from working!
	lda SYS_ACR
	sta old_sys_acr
	lda #0
	sta SYS_ACR

	;lda #15:sta SYS_DDRB
	;lda #4:sta SYS_ORB:inc a:sta SYS_ORB
	;lda #3:sta SYS_ORB
	;lda #$7f:sta SYS_DDRA

        ; Point at IRQ handler
        lda #<irq1
        ldx #>irq1
        sta $204
        stx $205

	; Enable Usr timer 1 interrupt
	lda #0b11000000
	sta USR_IER
	
	; Disable USR_IER bits
	;lda #0b00111111
	;sta USR_IER
        
	lda SYS_IER
	sta old_sys_ier
	
        ; Enable Sys CA1 interrupt.
        lda #0b10000010
        sta SYS_IER
        
	; Disable Sys CB1, CB2, timer1 interrupts
	; Note turning off sys timer1 interrupt breaks a lot of stuff!
	;lda #0b01011000
	; CB1 & CB2 only
	lda #0b00011000
	; or everything!
	;lda #0b01111101
	sta SYS_IER

        cli
	
        rts
	.)

deinit_effect
	.(
	sei
	
	lda oldirq1v
	sta $204
	lda oldirq1v+1
	sta $205
	
	lda #$7f
	sta SYS_IER
	lda old_sys_ier
	sta SYS_IER
	
	lda old_sys_pcr
	sta SYS_PCR
	
	lda old_sys_acr
	sta SYS_ACR
	
	lda #<1000
	sta SYS_T1C_L
	lda #>1000
	sta SYS_T1C_H
	
	lda #<10000
	sta SYS_T1L_L
	lda #>10000
	sta SYS_T1L_H
	
	lda #$7f
	sta USR_IER
	lda old_usr_ier
	sta USR_IER
	
	@crtc_write 4, {#38}
	@crtc_write 5, {#0}
	@crtc_write 6, {#32}
	@crtc_write 7, {#34}
	@crtc_write 8, {#0b11000001}
	@crtc_write 12, {#>[$3000/8]}
	@crtc_write 13, {#<[$3000/8]}
	
	cli
	rts
	.)

which_mem
	.byte 0

	.alias R 0b00000001
	.alias G 0b00000100
	.alias B 0b00010000
	.alias Y [R|G]
	.alias M [R|B]
	.alias C [G|B]
	.alias W [R|G|B]

amazing_palette
	.byte 0
	.byte B << 1
	.byte B | [B << 1]
	.byte B | [M << 1]
	.byte M | [M << 1]
	.byte M | [W << 1]
	.byte W | [W << 1]
	.byte W | [C << 1]
	.byte C | [C << 1]
	.byte C | [G << 1]
	.byte G | [G << 1]
	.byte G | [Y << 1]
	.byte Y | [Y << 1]
	.byte Y | [R << 1]
	.byte R | [R << 1]
	.byte R

	.context set_palette

pal_bytes:
	.byte $00 | [3 ^ 7]
	.byte $10 | [3 ^ 7]
	.byte $20 | [1 ^ 7]
	.byte $30 | [2 ^ 7]
	.byte $40 | [6 ^ 7]
	.byte $50 | [1 ^ 7]
	.byte $60 | [6 ^ 7]
	.byte $70 | [2 ^ 7]
	.byte $80 | [6 ^ 7]
	.byte $90 | [4 ^ 7]
	.byte $a0 | [5 ^ 7]
	.byte $b0 | [5 ^ 7]
	.byte $c0 | [4 ^ 7]
	.byte $d0 | [6 ^ 7]
	.byte $e0 | [6 ^ 7]
	.byte $f0 | [0 ^ 7]
	
set_palette:
	ldx #15
loop:
	lda pal_bytes,x
	sta PALCONTROL
	dex
	bpl loop
	rts
	.ctxend
	
colour_bytes
	.byte 255
	.byte 174
	.byte 12
	.byte 8
	.byte 0
	.byte 10
	.byte 15
	.byte 45
	.byte 60
	.byte 56
	.byte 248
	.byte 242
	.byte 216
	.byte 240
	.byte 245
	.byte 255

	.alias exp_colour_bytes $2f00

	.context expand_colour_bytes
	.var2 outptr
expand_colour_bytes
	lda #<exp_colour_bytes
	sta %outptr
	lda #>exp_colour_bytes
	sta %outptr+1

	ldx #0
loop
	phx
	txa
	and #15
	tax
	lda colour_bytes,x
	plx
	ldy #0
loop2
	sta (%outptr),y
	iny
	cpy #8
	bne loop2
	
	lda %outptr
	clc
	adc #8
	sta %outptr
	.(
	bcc nohi
	inc %outptr+1
nohi:	.)

	inx
	cpx #32
	bne loop
	
	rts
	.ctxend

	.context filltest
	.var2 ptr
filltest:
	lda #0
	sta which_mem
repeat:
	lda #<SCREENSTART
	sta %ptr
	lda #>SCREENSTART
	sta %ptr+1
	ldy #0
loop:
	tya
	and #63
	lsr a
	lsr a
	tax
	lda colour_bytes,x
	sta (%ptr),y

	;pha
	;phx
	;phy
	;lda #19
	;jsr osbyte
	;ply
	;plx
	;pla

	iny
	bne loop
	
	inc %ptr+1
	lda %ptr+1
	cmp #$80
	bne loop
	
	lda ACCCON
	ora #4
	sta ACCCON
	
	inc which_mem
	lda which_mem
	cmp #2
	bne repeat
	
	rts
	.ctxend

x_offset
	.byte 0
y_offset
	.byte 0
ring_topleft
	.word 0

	.context plasma
	.var2 rowptr
	.var xidx, yidx
	.var tmp, ytmp
plasma:
	lda #<SCREENSTART
	sta %rowptr
	lda #>SCREENSTART
	sta %rowptr+1
	
	lda ring_topleft
	sta mod_rings_0 + 1
	sta mod_rings_1 + 1
	lda ring_topleft+1
	sta mod_rings_0 + 2
	sta mod_rings_1 + 2
	
	; Non-shadow screen
	lda ACCCON
	and #~4
	sta ACCCON
	
	lda #0
	sta %yidx
yloop
	;lda #0
	;sta %xidx
	lda %yidx
	clc
	adc y_offset
	tax
	
	lda #<sintab
	clc
	adc x_offset
	sec
	sbc %yidx
	sta mod_sintab_0 + 1
	sta mod_sintab_1 + 1
	lda #>sintab
	adc #0
	sta mod_sintab_0 + 2
	sta mod_sintab_1 + 2
	
	; Render the even bytes
	
	ldy #62
xloop_0:
mod_sintab_0:
	lda sintab,y
	sbc sintab,y
mod_rings_0:
	adc rings,y
	adc sintab,x
	sta mod_store_0 + 1
mod_store_0:
	lda exp_colour_bytes
	sta (%rowptr),y
	dey
	dey
	bpl xloop_0

	; And the odd bytes

	lda %rowptr
	eor #64
	sta %rowptr

	ldy #63
xloop_1:
mod_sintab_1:
	lda sintab,y
	sbc sintab,y
mod_rings_1:
	adc rings,y
	adc sintab,x
	sta mod_store_1 + 1
mod_store_1:
	lda exp_colour_bytes
	sta (%rowptr),y
	dey
	dey
	bpl xloop_1

	lda %rowptr
	eor #64
	sta %rowptr

	inc %yidx
	lda %yidx
	.(
	cmp #16
	bne noswitch
	lda ACCCON
	ora #4
	sta ACCCON
	lda #<SCREENSTART
	sta %rowptr
	lda #>SCREENSTART
	sta %rowptr+1
	bra done
noswitch
	lda %rowptr
	clc
	;adc #73
	adc #64
	sta %rowptr
	bcc done
	inc %rowptr+1
done:	.)

	inc mod_rings_0 + 2
	inc mod_rings_1 + 2

	lda %yidx
	cmp #32
	bne yloop
	
	rts
	.ctxend

ring_x
	.word 0
ring_y
	.word 0

	.context plasma_loop
	.var2 y_partial
plasma_loop
	lda #0
	sta %y_partial
	sta %y_partial+1
repeat
	lda ring_x
	clc
	adc #250
	sta ring_x
	.(
	bcc nohi
	inc ring_x+1
nohi:	.)

	lda ring_y
	clc
	adc #33
	sta ring_y
	lda ring_y+1
	adc #1
	sta ring_y+1

	; rings follow lissajous figure.
	ldx ring_y+1
	lda sintab,x
	cmp #$80
	ror
	cmp #$80
	ror
	cmp #$80
	ror
	clc
	adc #[$80 + 32 - 16]
	sta ring_topleft+1

	ldx ring_x+1
	lda sintab,x
	cmp #$80
	ror
	clc
	adc #[128 - 32]
	sta ring_topleft

	jsr plasma
	inc x_offset
	lda %y_partial
	sec
	sbc #84
	sta %y_partial
	lda %y_partial+1
	sbc #0
	sta %y_partial+1
	sta y_offset
	bra repeat
	rts
	.ctxend
