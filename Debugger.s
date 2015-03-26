.pc = $1000 "Debugger"		// Org address of the interrupt routine
.var	Screen	= $0400
.var	Colour	= $d800
.var 	Screen1	= $0400+[23*40]		// Used in dumping the SID(s) to the
.var 	Screen2	= $0400+[24*40]		// screen for debugging purposes
.var 	RasterPos	= $60
.var	SCPOS	= $a4
.var	SCCOL	= $a6
.var	SCHAR	= $a8	
.var	SX	= $aa
.var	SY	= $ab
.var	STEMP	= $ac
.var	SCOL	= $ad
//------------------------------------------------------------------------------
START:
	sei

	lda #0			// Select Song 0
	jsr InitMusic		// Init the player

	jsr ClearScreen

	lda #<ScreenText
	sta SCHAR
	lda #>ScreenText
	sta SCHAR+1
	jsr PrintTEXT

	lda #1
	sta RastertimeCurrent
	sta RastertimeMax
	lda #0
	sta $d020
	sta $d021

	lda #$7f
	sta $dc0d
	and $d011
	sta $d011
	lda #RasterPos
	sta $d012
	lda #<IRQVEC		// Low byte of interrupt vector
	sta $0314
	lda #>IRQVEC
	sta $0315		// High byte of interrupt vector
	lda #$01
	sta $d01a
	cli
!ReadKeyboard:
	lda $cb
	cmp #$40
	beq !ReadKeyboard-
!WaitForKeyRelease:
	ldx $cb
	cpx #$40
	bne !WaitForKeyRelease-	

	cmp #$17
	bne !NextKB+
	brk
!NextKB:
	cmp #$38
	bne !NextKB+
	lda #0
	jsr InitMusic
	lda #1
	sta RastertimeMax
!NextKB:
	jmp !ReadKeyboard-
//------------------------------------------------------------------------------
IRQVEC:
	dec $d019		// Acknowledge the raster interrupt
	inc $d020		// Change border colour. A visual measurement.

	lda $d012
	sta CountRaster+1

	jsr PlayMusic

	lda $d012
	sec
CountRaster:
	sbc #0
	sta RastertimeCurrent

	dec $d020		// Change the colour back. Caveat emptor!

	ldx #$10		// A slight delay before SID2Screen is called
!loop:
	dex
	bne !loop-

	jsr SIDtoScreen
	jmp $ea31		// Jump to normal vector

SIDtoScreen:
//	inc $D020
	lda SIDFreq_L+3
	sta Screen2+$00
	lda SIDFreq_H+3
	sta Screen2+$01
	lda SIDFreq_L+4
	sta Screen2+$07
	lda SIDFreq_H+4
	sta Screen2+$08
	lda SIDFreq_L+5
	sta Screen2+$0e
	lda SIDFreq_H+5
	sta Screen2+$0f
	lda SIDPulse_L+3
	sta Screen2+$02
	lda SIDPulse_H+3
	sta Screen2+$03
	lda SIDPulse_L+4
	sta Screen2+$09
	lda SIDPulse_H+4
	sta Screen2+$0a
	lda SIDPulse_L+5
	sta Screen2+$10
	lda SIDPulse_H+5
	sta Screen2+$11
	lda SIDAttackDecay+3
	sta Screen2+$05
	lda SIDSustainRelease+3
	sta Screen2+$06
	lda SIDAttackDecay+4
	sta Screen2+$0c
	lda SIDSustainRelease+4
	sta Screen2+$0d
	lda SIDAttackDecay+5
	sta Screen2+$13
	lda SIDSustainRelease+5
	sta Screen2+$14
	lda SIDFreq_L
	sta Screen1+$00
	lda SIDFreq_H
	sta Screen1+$01
	lda SIDFreq_L+1
	sta Screen1+$07
	lda SIDFreq_H+1
	sta Screen1+$08
	lda SIDFreq_L+2
	sta Screen1+$0e
	lda SIDFreq_H+2
	sta Screen1+$0f
	lda SIDPulse_L
	sta Screen1+$02
	lda SIDPulse_H
	sta Screen1+$03
	lda SIDPulse_L+1
	sta Screen1+$09
	lda SIDPulse_H+1
	sta Screen1+$0a
	lda SIDPulse_L+2
	sta Screen1+$10
	lda SIDPulse_H+2
	sta Screen1+$11
	lda SIDAttackDecay
	sta Screen1+$05
	lda SIDSustainRelease
	sta Screen1+$06
	lda SIDAttackDecay+1
	sta Screen1+$0c
	lda SIDSustainRelease+1
	sta Screen1+$0d
	lda SIDAttackDecay+2
	sta Screen1+$13
	lda SIDSustainRelease+2
	sta Screen1+$14

	lda SIDControlReg		// Set the Control on a normal SID
	sta Screen1+$04
	lda SIDControlReg+1
	sta Screen1+$0a
	lda SIDControlReg+2
	sta Screen1+$12
	lda SIDControlReg+3
	sta Screen2+$04
	lda SIDControlReg+4
	sta Screen2+$0c
	lda SIDControlReg+5
	sta Screen2+$12

	lda SIDFilterCutoff_L
	sta $b2
	and #7
	sta Screen1+$15
	lda SIDFilterCutoff_H
	lsr
	ror $b2
	lsr
	ror $b2
	lsr
	ror $b2
	lda $b2
	sta Screen1+$16
	lda SIDFilterResCont
	sta Screen1+$17
	lda SIDFilterTypeVol
	ora SongVolume+0
	sta Screen1+$18

	lda SIDFilterCutoff_L+1
	sta $b2
	and #7
	sta Screen2+$15
	lda SIDFilterCutoff_H+1
	lsr
	ror $b2
	lsr
	ror $b2
	lsr
	ror $b2
	lda $b2
	sta Screen2+$16
	lda SIDFilterResCont+$1
	sta Screen2+$17
	lda SIDFilterTypeVol+$1
	ora SongVolume+1
	sta Screen2+$18

	lda TempoCounter
	sta $0400+39

//	dec $D020	// Change border colour
//------------------------------------------------------------------------------
	lda #0
	sta $ae
	lda #10
	sta SX
!loop:
	lda #5				// Print FREQ High
	sta SY
	ldx $ae
	lda SIDFreq_H,x
	jsr PrintHEX
	
	lda #6				// Print Pulse High
	sta SY
	ldx $ae
	lda SIDPulse_H,x
	jsr PrintHEX

	lda #7				// Print Control Registers
	sta SY
	ldx $ae
	lda SIDControlReg,x
	jsr PrintHEX

	lda #8				// Print Attack/Decay
	sta SY
	ldx $ae
	lda SIDAttackDecay,x
	jsr PrintHEX
	
	lda #9				// Print Note Number
	sta SY
	ldx $ae
	lda NoteNumber,x
	jsr PrintNOTE



	lda #10				// Print Note Counter
	sta SY
	ldx $ae
	lda NoteCounter,x
	jsr PrintHEX

	lda #11				// Print Stack Index
	sta SY
	ldx $ae
	lda StackIndex,x
	jsr PrintHEX

	lda #13				// Print Stack Pointer High
	sta SY
	ldx $ae
	lda SeqPointer_H,x
	jsr PrintHEX

	lda SX
	clc
	adc #5
	sta SX
	ldx $ae
	inx
	stx $ae
	cpx #6
	bne !loop- 
//------------------------------------------------------------------------------
	lda #0
	sta $ac
	lda #12
	sta SX
!loop:
	lda #5				// Print FREQ Low
	sta SY
	ldx $ac
	lda SIDFreq_L,x
	jsr PrintHEX
	
	lda #6				// Print Pulse Low
	sta SY
	ldx $ac
	lda SIDPulse_L,x
	jsr PrintHEX

	lda #8				// Print Sustain/Release
	sta SY
	ldx $ac
	lda SIDSustainRelease,x
	jsr PrintHEX

	lda #13				// Print Stack Pointer Low
	sta SY
	ldx $ac
	lda SeqPointer_L,x
	jsr PrintHEX

	lda SX
	clc
	adc #5
	sta SX
	ldx $ac
	inx
	stx $ac
	cpx #6
	bne !loop-
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
	lda #11
	sta SX
	lda #20
	sta SY
	lda RastertimeCurrent
	jsr PrintHEX

	lda RastertimeCurrent
	cmp RastertimeMax
	bcc !NoNewMax+
	sta RastertimeMax
!NoNewMax:
	lda #18
	sta SX
	lda RastertimeMax
	jsr PrintHEX
//------------------------------------------------------------------------------
	lda #38				// Print Keypress
	sta SX
	lda #23
	sta SY
	lda $cb
	jsr PrintHEX
//------------------------------------------------------------------------------
	lda #38
	sta SX
	lda #20
	sta SY
	lda ControlRegRepeat
	jsr PrintHEX
	lda #21
	sta SY
	lda GateModify
	jsr PrintHEX

	rts
//------------------------------------------------------------------------------
RastertimeCurrent:
	.byte	0
RastertimeMax:
	.byte	0
//------------------------------------------------------------------------------
PrintHEX:
	sta SCHAR
	lda SY
	asl
	tay
	lda YTAB,y
	sta SCPOS
	lda YTAB+1,y
	sta SCPOS+1
	ldy SX
	lda SCHAR
	lsr
	lsr
	lsr
	lsr
	tax
	lda HexDigits,x
	and #$3f
	sta (SCPOS),y
	iny
	lda SCHAR
	and #$0f
	tax
	lda HexDigits,x
	and #$3f
	sta (SCPOS),y
PrintHEXExit:
	rts	
//------------------------------------------------------------------------------
PrintTEXT:
	ldy #0
!GetCoords:
	lda (SCHAR),y
	bmi PrintHEXExit
	sta SCOL
	iny
	lda (SCHAR),y
	sta SX
	iny
	lda (SCHAR),y
	iny
	sta SY
	asl
	tax
	lda YTAB,x
	sta SCPOS
	sta SCCOL
	lda YTAB+1,x
	sta SCPOS+1
	clc
	adc #$d4
	sta SCCOL+1
!GetNextChar:
	lda (SCHAR),y
	bpl !PrintIt+
	iny
	jmp !GetCoords-
!PrintIt:
	sty STEMP
	ldy SX
	and #$3f
	sta (SCPOS),y
	lda SCOL
	sta (SCCOL),y
	ldy STEMP
	iny
	inc SX
	jmp !GetNextChar-
//------------------------------------------------------------------------------
PrintNOTE:
	asl
	asl
	sta STEMP
	lda SY
	asl
	tax
	lda YTAB,x
	sta SCPOS
	lda YTAB+1,x
	sta SCPOS+1
	ldx STEMP
	ldy SX
	lda NOTES,x
	and #$3f
	sta (SCPOS),y
	iny
	lda NOTES+1,x
	and #$3f
	sta (SCPOS),y
	iny
	lda NOTES+2,x
	and #$3f
	sta (SCPOS),y
	iny
	lda NOTES+3,x
	and #$3f
	sta (SCPOS),y
	rts

ClearScreen:
	ldx #0
!loop:
	lda #$20
	sta Screen,x
	sta Screen+$100,x
	sta Screen+$200,x
	sta Screen+$300,x
	lda #1
	sta Colour,x
	sta Colour+$100,x
	sta Colour+$200,x
	sta Colour+$300,x
	dex
	bne !loop-
	rts
//------------------------------------------------------------------------------
YTAB:
	.word	Screen+[0*40],Screen+[1*40],Screen+[2*40],Screen+[3*40]
	.word	Screen+[4*40],Screen+[5*40],Screen+[6*40],Screen+[7*40]
	.word	Screen+[8*40],Screen+[9*40],Screen+[10*40],Screen+[11*40]
	.word	Screen+[12*40],Screen+[13*40],Screen+[14*40],Screen+[15*40]
	.word	Screen+[16*40],Screen+[17*40],Screen+[18*40],Screen+[19*40]
	.word	Screen+[20*40],Screen+[21*40],Screen+[22*40],Screen+[23*40]
	.word	Screen+[24*40]
//------------------------------------------------------------------------------
HexDigits:
	.text "0123456789ABCDEF"
//------------------------------------------------------------------------------
NOTES:
	.text	"C  0C# 0D  0D# 0E  0F  0F# 0G  0G# 0A  0A# 0B  0"
	.text	"C  1C# 1D  1D# 1E  1F  1F# 1G  1G# 1A  1A# 1B  1"
	.text	"C  2C# 2D  2D# 2E  2F  2F# 2G  2G# 2A  2A# 2B  2"
	.text	"C  3C# 3D  3D# 3E  3F  3F# 3G  3G# 3A  3A# 3B  3"
	.text	"C  4C# 4D  4D# 4E  4F  4F# 4G  4G# 4A  4A# 4B  4"
	.text	"C  5C# 5D  5D# 5E  5F  5F# 5G  5G# 5A  5A# 5B  5"
	.text	"C  6C# 6D  6D# 6E  6F  6F# 6G  6G# 6A  6A# 6B  6"
	.text	"C  7C# 7D  7D# 7E  7F  7F# 7G  7G# 7A  7REST"
//------------------------------------------------------------------------------
ScreenText:
	.byte	13,0,0
	.text	"ZBLEXV6 MUSIC DRIVER DEBUGGER"
	.byte	$ff
	.byte	5,0,1
	.text	"(C) GAVIN GRAHAM"
	.byte	$ff
	.byte	14,10,3
	.text	"V1   V2   V3   V4   V5   V6"
	.byte	$ff
	.byte	12,10,4
	.text	"----:----:----:----:----:----"
	.byte	$ff
	.byte	7,0,5
	.text	"FREQ:"
	.byte	$ff
	.byte	7,0,6
	.text	"PULS:"
	.byte	$ff
	.byte	7,0,7
	.text	"CTRL:"
	.byte	$ff
	.byte	7,0,8
	.text	"ADSR:"
	.byte	$ff
	.byte	7,0,9
	.text	"NOTE:"
	.byte	$ff
	.byte	7,0,10
	.text	"DURA:"
	.byte	$ff
	.byte	7,0,11
	.text	"STACK:"
	.byte	$ff
	.byte	7,0,12
	.text	"Filter:"
	.byte	$ff
	.byte	7,0,13
	.text	"ADDR:"
	.byte	$ff
	.byte	12,0,20
	.text	"RASTER CUR:   MAX:  "
	.byte	$ff
	.byte	$ff


