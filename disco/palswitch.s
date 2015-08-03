	.org $2000

	.temps $70..$7f

	.macro crtc_write addr data
	lda #%addr
	sta CRTC_ADDR
	lda %data
	sta CRTC_DATA
	.mend
	
	.word entry_point
	.word deinit_effect
	.word wait_for_vsync
	.word preinit_effect

preinit_effect
	.(
	@crtc_write 7, {#34}
	jsr mos_cursoroff
	rts
	.)

entry_point:
	.(
	jsr initvsync
	rts
	.)

	.include "../lib/mos.s"
	;.include "../lib/sram.s"
	;.include "../lib/load.s"
	;.include "../lib/srambanks.s"
	.include "../lib/cmp.s"

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

.alias index $8f

.alias switch_rows 53

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

	; Latch next timeout
	lda #<[64*switch_rows-2]
	sta USR_T1L_L
	lda #>[64*switch_rows-2]
	sta USR_T1L_H

	ldx index

	lda $1f00,x : sta PALCONTROL	    ; 0
	lda $1f05,x : sta PALCONTROL	    ; 1
	lda $1f0a,x : sta PALCONTROL	    ; 2
	lda $1f0f,x : sta PALCONTROL	    ; 3
	lda $1f14,x : sta PALCONTROL	    ; 4
	lda $1f19,x : sta PALCONTROL	    ; 5
	lda $1f1e,x : sta PALCONTROL	    ; 6
	lda $1f23,x : sta PALCONTROL	    ; 7
	lda $1f28,x : sta PALCONTROL	    ; 8
	lda $1f2d,x : sta PALCONTROL	    ; 9
	lda $1f32,x : sta PALCONTROL	    ; 10
	lda $1f37,x : sta PALCONTROL	    ; 11
	lda $1f3c,x : sta PALCONTROL	    ; 12
	lda $1f41,x : sta PALCONTROL	    ; 13
	lda $1f46,x : sta PALCONTROL	    ; 14

	cpx #4
	.(
	bne not_last
	; Disable usr timer1 interrupt
	lda #0b01000000
	sta USR_IER
	lda #255
	sta USR_T1L_L
	sta USR_T1L_H
	bra exit_irq
not_last
	.)

	inx
	stx index

exit_irq:
	ply
	plx
	pla
	sta $fc
	rti

fliptime
	;.word 64 * 34 + 29
	.word 64 * 32 + 57

vsync
	phx
	phy

	; Clear interrupt
	lda USR_T1C_L

        ; Trigger after 'fliptime' microseconds
        lda fliptime
        sta USR_T1C_L
        lda fliptime+1
        sta USR_T1C_H

	lda #<[64*switch_rows-2]
	sta USR_T1L_L
	lda #>[64*switch_rows-2]
	sta USR_T1L_H

	; Clear IFR
	lda SYS_ORA
	
	; Generate stream of interrupts
	lda USR_ACR
	and #0b00111111
	ora #0b01000000
	sta USR_ACR
       
	lda #0
	sta index

	inc vsync_ctr

	; Enable usr timer1 interrupt
	lda #0b11000000
	sta USR_IER

	; gtfo
	ply
	plx
	pla
	sta $fc
	rti
	; jmp (oldirq1v)
	.)

vsync_ctr
	.byte 0

wait_for_vsync
	.(
	lda vsync_ctr
loop
	cmp vsync_ctr
	beq loop
	rts
	.)
