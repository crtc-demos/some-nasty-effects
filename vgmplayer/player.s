	.org $2700

	.alias zp_base $50

	.temps zp_base+8..zp_base+17
	
	.if 0
start:
	jsr initialize
loop:
	jsr poll
	lda #19
	jsr osbyte
	bra loop
	.endif
	
header:
	jmp initialize
	jmp poll
	jmp deinitialize
	jmp copy_effect_from_shadow
	jmp start_eventv
	jmp stop_eventv

	.alias tune $8000

	.alias song_freqtab tune
	.alias song_volenv tune+2
	.alias song_pitchenv tune+4
	.alias song_notes tune+6
	.alias song_looppoint tune+8
	.alias song_end tune+10

	.alias playpos zp_base
	.alias beatpos zp_base+2

initialize:
	@load_file_to songname, $3000
	
	lda #BANK0
	jsr select_sram
	ldx #<tune
	ldy #>tune
	lda #[16*1024]/256
	jsr copy_to_sram

	lda song_notes
	sta playpos
	lda song_notes+1
	sta playpos+1
	
	stz beatpos

	rts

songname:
	.if 1
	.asc "sonic",13
	.alias beat_length 24
	.elif 0
	.asc "serbian",13
	.alias beat_length 20
	.elif 0
	.asc "froz",13
	.else
	.asc "intens",13
	.alias beat_length 12
	.endif

	.alias chain_next_effect $1200
	.notemps chain_next_effect

deinitialize:
	jsr select_old_lang
	rts

	.alias src zp_base+3
	.alias dst zp_base+5
	.alias tmp zp_base+7

	; Copy A*256 bytes from $3000 to $1200. Entry with shadow bank in
	; memory space.
copy_effect_from_shadow
	.(
	tax
	lda #<$3000
	sta src
	lda #>$3000
	sta src+1
	lda #<$1200
	sta dst
	lda #>$1200
	sta dst+1
	ldy #0
loop
	; Get byte, maybe from shadow RAM...
	sei
	lda ACCCON
	ora #4
	sta ACCCON
	cli
	lda (src),y
	sta tmp

	; Stick it back in normal RAM.
	
	sei
	lda ACCCON
	and #~4
	sta ACCCON
	cli

	lda tmp
	
	sta (dst),y
	iny
	bne loop
	inc src + 1
	inc dst + 1
	dex
	bne loop
	
	lda ACCCON
	and #~4
	sta ACCCON
	
	; Chain to next effect
	jmp chain_next_effect
	
	.)

	.include "../lib/mos.s"
	.include "../lib/load.s"
	.include "../lib/sram.s"
	.include "../lib/srambanks.s"
	.include "../lib/cmp.s"

chan0_pitch:
	.byte 0
chan1_pitch:
	.byte 0
chan2_pitch:
	.byte 0
chan3_pitch:
	.byte 0

c0_pitch_real:
	.word 0xffff
c1_pitch_real:
	.word 0xffff
c2_pitch_real:
	.word 0xffff
c3_pitch_real:
	.byte 0xff

pe0_pos:
	.word 0
pe1_pos:
	.word 0
pe2_pos:
	.word 0
pe3_pos:
	.word 0

ve0_pos:
	.word 0
ve1_pos:
	.word 0
ve2_pos:
	.word 0
ve3_pos:
	.word 0

	.if beat_length = 8
	.context tx8
	.var2 tmp
tx8
	stz %tmp+1
	asl a
	rol %tmp+1
	asl a
	rol %tmp+1
	asl a
	rol %tmp+1
	ldx %tmp+1
	rts
	.ctxend
	.endif

	.if beat_length = 12
	; Input: A
	; Output: A (lo), X (hi)
	.context tx12
	.var2 tmp, tmp2    
tx12
	sta %tmp
	stz %tmp+1
	
	stz %tmp2+1
	asl a
	rol %tmp2+1
	sta %tmp2
	
	clc
	adc %tmp
	sta %tmp
	lda %tmp+1
	adc %tmp2+1
	sta %tmp+1
	
	lda %tmp
	asl a
	rol %tmp+1
	asl a
	rol %tmp+1

	ldx %tmp+1
	
	rts
	.ctxend
	.endif

	.if beat_length = 16
	.context tx16
	.var2 tmp
tx16
	stz %tmp+1
	asl a
	rol %tmp+1
	asl a
	rol %tmp+1
	asl a
	rol %tmp+1
	asl a
	rol %tmp+1
	ldx %tmp+1
	rts
	.ctxend
	.endif

	.if beat_length = 20
	.context tx20
	.var2 tmp, tmp2
tx20
	sta %tmp
	stz %tmp+1

	; tmp2 = a*4
	stz %tmp2+1
	asl a
	rol %tmp2+1
	asl a
	rol %tmp2+1
	
	; tmp2 = tmp2 + tmp (* 5)
	clc
	adc %tmp
	sta %tmp2
	lda %tmp2+1
	adc %tmp+1
	sta %tmp2+1
	
	; tmp2 * 4 (* 20)
	lda %tmp2
	asl a
	rol %tmp2+1
	asl a
	rol %tmp2+1

	ldx %tmp2+1
	
	rts
	.ctxend
	.endif

	.if beat_length = 24
	.context tx24
	.var2 tmp, tmp2
tx24
	sta %tmp
	stz %tmp+1
	
	stz %tmp2+1
	asl a
	rol %tmp2+1
	sta %tmp2
	
	clc
	adc %tmp
	sta %tmp
	lda %tmp+1
	adc %tmp2+1
	sta %tmp+1
	
	lda %tmp
	asl a
	rol %tmp+1
	asl a
	rol %tmp+1
	asl a
	rol %tmp+1

	ldx %tmp+1
	
	rts
	.ctxend
	.endif

	.if beat_length = 28
	.context tx28
	.var2 tmp, tmp2
tx28
	sta %tmp
	stz %tmp+1

	stz %tmp2+1

	; { tmp2, a } * 8
	asl a
	rol %tmp2+1
	asl a
	rol %tmp2+1
	asl a
	rol %tmp2+1
	sta %tmp2

	; tmp2 = tmp2 - tmp (* 7)
	lda %tmp2
	sec
	sbc %tmp
	sta %tmp2
	lda %tmp2+1
	sbc %tmp+1
	sta %tmp2+1
	
	; tmp2 * 4 (* 28)
	lda %tmp2
	asl a
	rol %tmp2+1
	asl a
	rol %tmp2+1
	
	ldx %tmp2+1
	
	rts
	.ctxend
	.endif

	.if beat_length = 36
	; Input: A
	; Output: A (lo), X (hi)
;	.context tx36
;	.var2 tmp, tmp2    
;tx36
;	; %tmp = <input>*4
;	stz %tmp+1
;	asl a
;	rol %tmp+1
;	asl a
;	rol %tmp+1
;	sta %tmp
	
	; %tmp2 = <input>*32
;	ldx %tmp+1
;	stx %tmp2+1
;	asl a
;	rol %tmp2+1
;	asl a
;	rol %tmp2+1
;	asl a
;	rol %tmp2+1
	
;	clc
;	adc %tmp
;	tay
;	lda %tmp2+1
;	adc %tmp
;	tax
;	tya
	
;	rts
;	.ctxend
	.endif

	.alias PSG_STROBE_SEI_INSN psg_strobe
	.alias PSG_STROBE_CLI_INSN psg_strobe + 23

	; WARNING: sei/cli are removed temporarily when running from eventv.
	.context psg_strobe
psg_strobe:
	sei
	ldy #255
	sty $fe43
	
	sta $fe41
	stz $fe40
	nop
	nop
	nop
	nop
	nop
	nop
	lda #$08
	sta $fe40
	cli
	rts
	.ctxend

	.macro init_vpos playpos offset addrbase posidx
	ldy #%offset
	lda (%playpos),y
	.if beat_length = 8
	jsr tx8
	.elif beat_length = 12
	jsr tx12
	.elif beat_length = 20
	jsr tx20
	.elif beat_length = 24
	jsr tx24
	.elif beat_length = 28
	jsr tx28
	.else
	@error
	.endif
	clc
	adc %addrbase
	sta %posidx
	txa
	adc %addrbase+1
	sta %posidx+1
	.mend

	.if 0
	.macro init_ppos playpos offset addrbase posidx
	ldy #%offset
	lda (%playpos),y
	jsr tx36
	clc
	adc #<%addrbase
	sta %posidx
	txa
	adc #>%addrbase
	sta %posidx+1
	.mend
	.endif

	.macro psg_write_tone_cached write cache latchmask
	ldy #1
	lda (%write),y
	cmp %cache+1
	bne write_psg
	lda (%write)
	cmp %cache
	beq skip
	lda (%write),y
write_psg
	sta %cache+1
	ora #%latchmask
	jsr psg_strobe
	lda (%write)
	sta %cache
	jsr psg_strobe
skip:
	.mend

	.macro psg_write_tone_uncached write latchmask
	ldy #1
	lda (%write),y
	ora #%latchmask
	jsr psg_strobe
	lda (%write)
	jsr psg_strobe
	.mend

	.context poll
	.var2 pitchenvpos
	.var2 volenvpos, tmp
poll:
	lda beatpos
	bne continue_beat

	ldy #0
	lda (playpos),y
	sta chan0_pitch
	ldy #3
	lda (playpos),y
	sta chan1_pitch
	ldy #6
	lda (playpos),y
	sta chan2_pitch
	ldy #9
	lda (playpos),y
	sta chan3_pitch
	
	@init_vpos playpos, 1, song_pitchenv, pe0_pos
	@init_vpos playpos, 2, song_volenv, ve0_pos
	@init_vpos playpos, 4, song_pitchenv, pe1_pos
	@init_vpos playpos, 5, song_volenv, ve1_pos
	@init_vpos playpos, 7, song_pitchenv, pe2_pos
	@init_vpos playpos, 8, song_volenv, ve2_pos
	@init_vpos playpos, 10, song_pitchenv, pe3_pos
	@init_vpos playpos, 11, song_volenv, ve3_pos
continue_beat
	
	; channel 0
	; pitch
	ldy beatpos
	lda pe0_pos
	sta %pitchenvpos
	lda pe0_pos+1
	sta %pitchenvpos+1
	lda (%pitchenvpos),y
	clc
	adc chan0_pitch
	stz %tmp+1
	asl a
	rol %tmp+1
	clc
	adc song_freqtab
	sta %tmp
	lda %tmp+1
	adc song_freqtab+1
	sta %tmp+1

	@psg_write_tone_cached %tmp, c0_pitch_real, 0b10000000

	; volume
	ldy beatpos
	lda ve0_pos
	sta %volenvpos
	lda ve0_pos+1
	sta %volenvpos+1
	lda (%volenvpos),y
	ora #0b10010000
	jsr psg_strobe

	; channel 1
	; pitch
	ldy beatpos
	lda pe1_pos
	sta %pitchenvpos
	lda pe1_pos+1
	sta %pitchenvpos+1
	lda (%pitchenvpos),y
	clc
	adc chan1_pitch
	stz %tmp+1
	asl a
	rol %tmp+1
	clc
	adc song_freqtab
	sta %tmp
	lda %tmp+1
	adc song_freqtab+1
	sta %tmp+1

	@psg_write_tone_cached %tmp, c1_pitch_real, 0b10100000
	
	; volume
	ldy beatpos
	lda ve1_pos
	sta %volenvpos
	lda ve1_pos+1
	sta %volenvpos+1
	lda (%volenvpos),y
	ora #0b10110000
	jsr psg_strobe

	; channel 2
	; pitch
	ldy beatpos
	lda pe2_pos
	sta %pitchenvpos
	lda pe2_pos+1
	sta %pitchenvpos+1
	lda (%pitchenvpos),y
	clc
	adc chan2_pitch
	stz %tmp+1
	asl a
	rol %tmp+1
	clc
	adc song_freqtab
	sta %tmp
	lda %tmp+1
	adc song_freqtab+1
	sta %tmp+1
		
	@psg_write_tone_cached %tmp, c2_pitch_real, 0b11000000

	; volume
	ldy beatpos
	lda ve2_pos
	sta %volenvpos
	lda ve2_pos+1
	sta %volenvpos+1
	lda (%volenvpos),y
	ora #0b11010000
	jsr psg_strobe

	; channel 3
	; pitch
	ldy beatpos
	lda pe3_pos
	sta %pitchenvpos
	lda pe3_pos+1
	sta %pitchenvpos+1
	lda (%pitchenvpos),y
	clc
	adc chan3_pitch
	stz %tmp+1
	asl a
	rol %tmp+1
	clc
	adc song_freqtab
	sta %tmp
	lda %tmp+1
	adc song_freqtab+1
	sta %tmp+1
	
	.(
	lda (%tmp)
	cmp c3_pitch_real
	beq skip
	sta c3_pitch_real
	ora #0b11100000
	jsr psg_strobe
skip:	.)

	; volume
	ldy beatpos
	lda ve3_pos
	sta %volenvpos
	lda ve3_pos+1
	sta %volenvpos+1
	lda (%volenvpos),y
	ora #0b11110000
	jsr psg_strobe

	; ---

	.(
	inc beatpos
	lda beatpos
	cmp #beat_length
	bne finished
	
	stz beatpos
	
	lda playpos
	clc
	adc #12
	sta playpos
	.(
	bcc nohi
	inc playpos+1
nohi:	.)
	
	@if_ltu_abs playpos, song_end, finished
	
	lda song_looppoint
	sta playpos
	lda song_looppoint+1
	sta playpos+1
	
finished
	.)
	rts
	.ctxend

	.alias SEI_OP $78
	.alias CLI_OP $58
	.alias NOP_OP $ea

old_eventv
	.word 0
old_rom
	.byte 0

	.context eventv_handler
eventv_handler
	php
	cmp #4
	bne not_vsync
	pha
	phx
	phy
	lda $f4
	sta old_rom
	lda #BANK0
	sta $f4
	sta $fe30
	jsr poll
	lda old_rom
	sta $f4
	sta $fe30
	ply
	plx
	pla
not_vsync
	plp
	rts
	.ctxend

	.context start_eventv
start_eventv
	lda #NOP_OP
	sta PSG_STROBE_SEI_INSN
	sta PSG_STROBE_CLI_INSN
	
	sei
	lda #<eventv_handler
	sta EVENTV
	lda #>eventv_handler
	sta EVENTV+1
	cli
	
	; Enable VSYNC event.
	lda #14
	ldx #4
	jsr osbyte
	rts
	.ctxend
	
	.context stop_eventv
stop_eventv
	; Disable VSYNC event.
	lda #13
	ldx #4
	jsr osbyte

	lda #SEI_OP
	sta PSG_STROBE_SEI_INSN
	lda #CLI_OP
	sta PSG_STROBE_CLI_INSN
	rts
	.ctxend
