	.alias osasci $ffe3
	.alias oswrch $ffee
	.alias osbyte $fff4
	.alias osfind $ffce
	.alias osgbpb $ffd1
	.alias osfile $ffdd
	.alias osword $fff1
	.alias oscli $fff7

	.alias CRTC_ADDR $fe00
	.alias CRTC_DATA $fe01

	.alias SYS_ORB $fe40
	.alias SYS_ORA $fe41
	.alias SYS_DDRB $fe42
	.alias SYS_DDRA $fe43
	.alias SYS_T1C_L $fe44
	.alias SYS_T1C_H $fe45
	.alias SYS_T1L_L $fe46
	.alias SYS_T1L_H $fe47
	.alias SYS_T2C_L $fe48
	.alias SYS_T2C_H $fe49
	.alias SYS_SR $fe4a
	.alias SYS_ACR $fe4b
	.alias SYS_PCR $fe4c
	.alias SYS_IFR $fe4d
	.alias SYS_IER $fe4e

	.alias USR_T1C_L $fe64
	.alias USR_T1C_H $fe65
	.alias USR_T1L_L $fe66
	.alias USR_T1L_H $fe67
	.alias USR_T2C_L $fe68
	.alias USR_T2C_H $fe69
	.alias USR_SR $fe6a
	.alias USR_ACR $fe6b
	.alias USR_PCR $fe6c
	.alias USR_IFR $fe6d
	.alias USR_IER $fe6e

	.alias ULACONTROL $fe20
	.alias PALCONTROL $fe21
	
	.alias ACCCON $fe34

	.alias EVENTV $220

	; The "notemps" directive says that these functions are safe to call
	; from within a context. I.e., they use none of the automatically
	; allocated temporaries.
	.notemps oswrch, osbyte, osfind, osgbpb, osfile, osword, osasci, oscli

	; Set graphics mode to A.
mos_setmode:
	pha
	lda #22
	jsr oswrch
	pla
	jsr oswrch
	rts

mos_cursoroff:
	.(
	ldx #0
loop
	lda vdubytes,x
	jsr oswrch
	inx
	cpx #10
	bne loop
	rts

vdubytes:
	.byte 23
	.byte 0
	.byte 10
	.byte 32
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.)
