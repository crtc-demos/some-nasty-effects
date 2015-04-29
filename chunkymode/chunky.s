	.org $e00

	.alias SCREENSTART $7C00
	.temps $70..$7f

	.alias which_irq $60

start:
	.(
	lda #2
	jsr mos_setmode
	jsr setup
	;lda #19
	;jsr oswrch
	;lda #15
	;jsr oswrch
	;lda #7
	;jsr oswrch
	;lda #0
	;jsr oswrch
	;jsr oswrch
	;jsr oswrch
	jsr initvsync
	jsr filltest
stuck:
	jmp stuck
	jsr deinit_effect
	rts
	.)

	.include "../lib/mos.s"

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
	ldx #73
	jsr crtc_write_ax
	
	; vertical displayed
	lda #6
	ldx #28
	jsr crtc_write_ax
	
	; vertical sync position
	lda #7
	ldx #44
	jsr crtc_write_ax
	
	rts
	.)

	.alias new_frame 64*6*6 - 64*2 + 24
	.alias shadow_switch 64*6*14 - 64*3
	.alias mid_2nd_frame 64*6*7

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
	@crtc_write 4, {#52-14-1}
	@crtc_write 6, {#14}
	@crtc_write 7, {#32}

	; disable usr timer1 interrupt
	lda #0b01000000
	sta USR_IER

	;lda #0b00000111 ^ 5 : sta PALCONTROL

	bra next_irq
	
first_after_vsync
	; We're somewhere at the start of the top frame: make sure registers
	; are set properly.
	@crtc_write 4, {#14-1}
	@crtc_write 6, {#14}
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
	and #1
	beq zero
	lda #$ff
zero:
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
