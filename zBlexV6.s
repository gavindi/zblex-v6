//-ZBLEX V6.0-☆██▓▒░░..ılı.! ! ! T  U  N  E S ! ! !ı.lııll .░░▒▓██  ☆---------
// A new C64 sound engine which is a huge deviation from my BlexV3 circa 1986.
// This puppy is capable of handling SIX voices. You can try that out by using
// the Vice emulator or jSidPlay2 with the second SID enabled.
//
// Also, all the parameters for the voices registers are controlled in tables
// which behave much the same as the sequencer note fetcher. The instrument
// tables allow for absolute and relative modification of all registers and 
// it is complete with looping, delay and flow control.
//
// It has been over 20 years since Ive coded anything let alone 6502 assembly
// so please pardon my sloppiness.
//
// This code is in Kick Assembler 3.21 format.
//------------------------------------------------------------------------------

.import source "Debugger.s"

//==============================================================================
//------------------------------------------------------------------------------
// START OF PLAY ROUTINE
// 'A' register holds the tune number when calling InitMusic
//------------------------------------------------------------------------------
//==============================================================================
.pc = $2000 "zBlex V6 Player"

.import source	"zBlexV6.h"

BLEXV6:
	lda #0
	jmp InitMusic
	jmp PlayMusic

InitMusic:
	asl		// Music Index table is 16 bytes
	asl		// so multiply by 16
	asl
	asl
	clc		// We will copy the whole block backwards to avoid cmp
	adc #$0f	// so add 15 bytes to the music index pointer
			// and ready the countdown

	tay		// Move multiplied 'A' to 'y' for index pointing
			// Now copy all the pointers so they are ready

			// Theres 16 byted to copy and as the temp table
			// matches format, lets copy the whole block
	ldx #$0f

!copyblock:
	lda MusicIndex,y		// Copy away Jose
	sta SeqPointer_L,x
	dey				// Decrease index to MusicIndex Table
	dex				// Decrease index to the temp pointer indexes
	bpl !copyblock-

	lda #0				// Clear the tempo counter
	sta TempoCounter

	ldx CurrentNumberofVoices
!clearvoices:
	lda #1				// Set the note counter to fetch
	sta NoteCounter,x		// a new note on song start
	lda #0				// Clear other variables used
	sta NoteTranspose,x
	sta GateModify,x
	sta PortSpeed,x
	sta SlideSpeed,x
	sta RestartModulator,x
	sta StackIndex,x		// Non negative vale also enables
					// the voice channel
	lda #$ff
	sta SlideDelay,x
	lda #8
	sta SIDControlReg,x

	dex				// Decrease the Voice Index
	bpl !clearvoices-
	ldy #6
	jmp SetControlRegisters
//==============================================================================
PlayMusic:
	ldx CurrentNumberofVoices	// Set the voice index for one big loop

//	lda #0
//	sta SIDFilterResCont
//	sta SIDFilterResCont+1
//	sta SIDFilterTypeVol
//	sta SIDFilterTypeVol+1

ProcessNextVoice:
	lda StackIndex,x		// Check to see if the voice is enabled
	bpl VoiceIsEnabled		// it may have been switched off
	jmp GetNextVoiceIndex		// at the end of the song

VoiceIsEnabled:
	lda TempoCounter		// Has the tempo count expired
	bne VoiceModulator		// No? Go directly to the Modulators

TempoExpired:
	dec NoteCounter,x		// Count down the note length
	bne VoiceModulator		// Not ready for new note so modulate

ReloadZeroPage:
	lda SeqPointer_L,x		// Set up the zero-page address
	sta ZPS_L			// so this voice can get the next
	lda SeqPointer_H,x		// Delay,Note,Command from its 
	sta ZPS_H			// sequence
	ldy #0				// reset the zero-page index
//..............................................................................
GetNextCommand:
	lda (ZPS_L),y			// Get the next byte from the sequence
	bpl ProcessNote			// If the high bit is set its a command
					// and not a note
ProcessCommands:
	sty ZPST			// Store the current sequnce ZP Y index
	tay				// Use it as the index to the
	lda CommandVectors_L-128,y	// COMMAND VECTORS
	sta CommandJump+1		// Since the MSB wasnt removed which
	lda CommandVectors_H-128,y	// saves two cycles, the command vectors
	sta CommandJump+2		// load is set -128 before the table
					// to compensate.
	ldy ZPST			// Index restored via self-modifying cod
CommandJump:
	jmp $0000			// Jump to commands filled in by
					// nasty self-modifying code
ProcessNote:	
	cmp #RST			// Is it a rest note
	beq FetchNoteLength		// Then add arse!

	clc				// Add any note transpose value
	adc NoteTranspose,x		// that has been set
	sta NoteNumber,x		// Store the value of the note

	sty ZPST			// Save 'Y' for later
	tay				// Transfer the note into the 'Y' index
	lda FreqTable_L,y		// Store the note frequency into
	sta SIDFreq_L,x			// the shadow SID
	lda FreqTable_H,y
	sta SIDFreq_H,x

	lda #0				// Reset the Portamento routine
	sta PortSpeed,x

InitModulators:
	lda #$fe
	sta ControlRegDelayCounter,x	// This will mark the modulators into
	sta PulseDelayCounter,x		// a state where they must initialise
	sta FreqNoteDelayCounter,x	// The Modulators can ignore this
	sta FreqRelDelayCounter,x	// if REMOD commands override
	sta ADSRDelayCounter,x		// the reload
	sta FilterDelayCounter,x

	ldy ZPST			// Get 'Y' back as the sequence pointer
FetchNoteLength:
	iny
	lda (ZPS_L),y			// Get the note duration
	sta NoteCounter,x		// and store it in the note counter
	iny				// Increase the index
//..............................................................................
StoreZP:
	tya				// Add 'Y' back
	clc				// to the zero-page low byte number
	adc ZPS_L
	sta SeqPointer_L,x		// Store is back in the pointer
	bcc VoiceModulator		// Fast increment of MSB if required
	inc SeqPointer_H,x
//	lda ZPS_H
//	adc #0
//	sta SeqPointer_H,x

//====== Modulate the sound ====================================================
VoiceModulator:
//------------------------------------------------------------------------------
ModulateController:
	lda RestartModulator,x		// Check if the modulator should
	and #MCT			// restart on a new note
	bne !NormalRuntime+

	lda ControlRegDelayCounter,x	// Fetch the delay counter
	cmp #$fe			// and check to see if the
					// sequencer has marked this module for
					// re-initialisation.

	bne !NormalRuntime+		// No, then continue processing
	ldy #0				// Reset the index pointer
	beq !FirstPass+			// and fetch the initial values

!NormalRuntime:
	lda ControlRegRepeat,x		// Check to see if we have reached
	cmp #RFS			// the end of this table
	beq !FinishedMod+		// if we have then do not modulate
	dec ControlRegDelayCounter,x	// Count down the delay
	bpl !FinishedMod+		// more delay to go to leave

	dec ControlRegRepeat,x		// Decrease the repeat counter
	bpl !DelayReload+		// More repeats to go for this command

	ldy ControlRegIndex,x		// Fetch the table index pointer

!FirstPass:
	lda ControlReg_L,x		// Build the Zero-page to the table
	sta ZPS_L
	lda ControlReg_H,x
	sta ZPS_H

!Reindexed:
	lda (ZPS_L),y			// Fetch the next byte from the table
	cmp #JIU			// Is it an index command?
	bne !NoReindex+			// Nope, so dont reindex
	iny				// Increase the index pointer
	lda (ZPS_L),y			// Fetch the 'Until' value
	bmi !Waiting+			// Negative values loops forever
	cmp NoteCounter,x		// Has the note reached this point?
	bcc !Waiting+			// No? Then fetch the index
	iny				// Yes? Then skip the JTP command
	iny				// and continue on in the sequence
	jmp !Reindexed-

!Waiting:
	iny
	lda (ZPS_L),y			// Fetch the value from Delay/Index
	tay				// Turn it into the new index pointer
	jmp !Reindexed-			// Now go fetch new data with the index

!NoReindex:
	sta ControlRegRepeat,x		// Store the value in the repeat counter

	cmp #RFS			// ****Just Added**** So nothing is
	beq !FinishedMod+		// read after a RFS

	iny				// Increase the pointer ZP+1
	lda (ZPS_L),y			// Fetch the Delay/Index value
	sta ControlRegDelay,x		// Store it also
	iny				// Increase the pointer ZP+2
	lda (ZPS_L),y			// Fetch the Control Reg Absolute Value
	sta ControlRegABS,x		// Store it out of ZP for faster access
	iny				// Increase to the last byte ZP+3
	lda (ZPS_L),y			// Fetch the Control Reg Relative Value
	sta ControlRegREL,x		// Store it out of ZP for faster access
	iny				// Increase the pointer ZP+4
	tya
	sta ControlRegIndex,x		// Store it ready for the next command
!DelayReload:

	lda ControlRegDelay,x
	sta ControlRegDelayCounter,x

	lda ControlRegABS,x		// Fetch the Absolute waveform value
	beq !NoAbsolute+		// If its null then dont apply it
	ora GateModify,x
	sta SIDControlReg,x		// Not null so update the shadow SID
	bne !FinishedMod+		// Pointless doing the relative
!NoAbsolute:
	lda SIDControlReg,x		
	eor ControlRegREL,x		// Xor it to the shadow SID
	ora GateModify,x
	sta SIDControlReg,x
!FinishedMod:
//..............................................................................
ModulatePulse:
	dec $d021
	lda RestartModulator,x	
	and #MPT
	bne !NormalRuntime+

	lda PulseDelayCounter,x		// Fetch the delay counter
	cmp #$fe			// and check to see if the sequencer
					// has marked this module for re-init
	bne !NormalRuntime+		// No, then continue processing
	lda #0				// Pulse width is additive so clear
	sta SIDPulse_L,x		// the initial pulse width
	sta SIDPulse_H,x
	tay				// Reset the table index pointer
	beq !FirstPass+			// and fetch the initial values

!NormalRuntime:
	lda PulseRepeat,x		// Check to see if we have reached
	cmp #RFS			// the end of this table
	beq !FinishedMod+		// if we have then do not modulate
	dec PulseDelayCounter,x		// Count down the delay
	bpl !FinishedMod+		// more delay to go to leave

	dec PulseRepeat,x		// Decrease the repeat counter
	bpl !DelayReload+		// More repeats to go for this command

	ldy PulseIndex,x		// Fetch the table index pointer

!FirstPass:
	lda Pulse_L,x			// Build the Zero-page to the table
	sta ZPS_L
	lda Pulse_H,x
	sta ZPS_H
!Reindexed:
	lda (ZPS_L),y			// Fetch the next byte from the table
	cmp #JIU			// Is it an index command?
	bne !NoReindex+			// Nope, so dont reindex
	iny				// Increase the index pointer
	lda (ZPS_L),y			// Fetch the 'Until' value
	bmi !Waiting+			// Negative values loops forever
	cmp NoteCounter,x		// Has the note reached this point?
	bcc !Waiting+			// No? Then fetch the index
	iny				// Yes? Then skip the JTP command
	iny				// and continue on in the sequence
	jmp !Reindexed-

!Waiting:
	iny
	lda (ZPS_L),y			// Fetch the value from Delay/Index
	tay				// Turn it into the new index pointer
	jmp !Reindexed-			// Now go fetch new data with the index

!NoReindex:
	sta PulseRepeat,x		// Store the value in the repeat counter
	cmp #RFS
	beq !FinishedMod+

	iny				// Increase the pointer ZP+1
	lda (ZPS_L),y			// Fetch the Delay/Index value
	sta PulseDelay,x		// Store it also
	iny				// Increase the pointer ZP+2
	lda (ZPS_L),y			// Fetch the Control Reg Absolute Value
	sta PulseVal_L,x		// Store it out of ZP for faster access
	iny				// Increase to the last byte ZP+3
	lda (ZPS_L),y			// Fetch the Control Reg Relative Value
	sta PulseVal_H,x		// Store it out of ZP for faster access
	iny				// Increase the pointer ZP+4
	tya
	sta PulseIndex,x		// Store it ready for the next command
!DelayReload:

	lda PulseDelay,x
	sta PulseDelayCounter,x

!GotDatatoProcess:
	lda PulseVal_L,x		// Pulse values are additive
	clc
	adc SIDPulse_L,x
	sta SIDPulse_L,x		// Add it to the current pulse value
	lda PulseVal_H,x		
	adc SIDPulse_H,x
	sta SIDPulse_H,x
!FinishedMod:
	inc $D021
//..............................................................................
ModulateFreqABS:
	lda RestartModulator,x
	and #MNT
	bne !NormalRuntime+

	lda FreqNoteDelayCounter,x	// Fetch the delay counter
	cmp #$fe			// and check to see if the sequencer
					// has marked this module for re-init
	bne !NormalRuntime+		// No, then continue processing
	ldy #0				// Reset the table index pointer
	beq !FirstPass+			// and fetch the initial values

!NormalRuntime:
	lda FreqNoteRepeat,x		// Check to see if we have reached
	cmp #RFS			// the end of this table
	beq !LargeJump+			// if we have then do not modulate
	dec FreqNoteDelayCounter,x	// Count down the delay
	bpl !LargeJump+			// more delay to go to leave

	dec FreqNoteRepeat,x		// Decrease the repeat counter
	bpl !DelayReload+		// More repeats to go for this command

	ldy FreqNoteIndex,x		// Fetch the table index pointer

!FirstPass:
	lda FreqNote_L,x		// Build the Zero-page to the table
	sta ZPS_L
	lda FreqNote_H,x
	sta ZPS_H

!Reindexed:
	lda (ZPS_L),y			// Fetch the next byte from the table
	cmp #JIU			// Is it an index command?
	bne !NoReindex+			// Nope, so dont reindex
	iny				// Increase the index pointer
	lda (ZPS_L),y			// Fetch the 'Until' value
	bmi !Waiting+			// Negative values loops forever
	cmp NoteCounter,x		// Has the note reached this point?
	bcc !Waiting+			// No? Then fetch the index
	iny				// Yes? Then skip the JTP command
	iny				// and continue on in the sequence
	jmp !Reindexed-
!LargeJump:
	jmp !FinishedMod+
!Waiting:
	iny
	lda (ZPS_L),y			// Fetch the value from Delay/Index
	tay				// Turn it into the new index pointer
	jmp !Reindexed-			// Now go fetch new data with the index

!NoReindex:
	sta FreqNoteRepeat,x		// Store the value in the repeat counter

	cmp #RFS
	beq !FinishedMod+

	iny				// Increase the pointer ZP+1
	lda (ZPS_L),y			// Fetch the Delay/Index value
	sta FreqNoteDelay,x		// Store it also
	iny				// Increase the pointer ZP+2
	lda (ZPS_L),y			// Fetch the Control Reg Absolute Value
	sta FreqNoteABS,x		// Store it out of ZP for faster access
	iny				// Increase to the last byte ZP+3
	lda (ZPS_L),y			// Fetch the Control Reg Relative Value
	sta FreqNoteRel,x		// Store it out of ZP for faster access
	iny				// Increase the pointer ZP+4
	tya
	sta FreqNoteIndex,x		// Store it ready for the next command
!DelayReload:

	lda FreqNoteDelay,x
	sta FreqNoteDelayCounter,x

	ldy FreqNoteABS,x		// Negative value forces it not
	bmi !NotQuantitive+		// to process this parameter
	lda FreqTable_L,y		// Get the note frequency
	sta SIDFreq_L,x
	lda FreqTable_H,y
	sta SIDFreq_H,x
!NotQuantitive:
	lda FreqNoteRel,x		// Negative value forces it not
	bmi !FinishedMod+		// to process this parameter
	clc
	adc NoteTranspose,x		// Add the transpose
	clc
	adc NoteNumber,x		// Add the parameter to the base note
	
	tay
	lda FreqTable_L,y		// Get the frequency for the note
	sta SIDFreq_L,x
	lda FreqTable_H,y
	sta SIDFreq_H,x
!FinishedMod:
//..............................................................................
ModulateFreqRel:
	lda RestartModulator,x		// Has the modulator been told
	and #MFT			// never to restart?
	bne !NormalRuntime+

	lda FreqRelDelayCounter,x	// Fetch the delay counter
	cmp #$fe			// and check to see if the sequencer
					// has marked this module for re-init
	bne !NormalRuntime+		// No, then contiinue processing
	ldy #0				// Reset the table index pointer
	beq !FirstPass+			// and fetch the initial values

!NormalRuntime:
	lda FreqRelRepeat,x		// Check to see if we have reached
	cmp #RFS			// the end of this table
	beq !FinishedMod+		// if we have then do not modulate
	dec FreqRelDelayCounter,x	// Count down the delay
	bpl !FinishedMod+		// more delay to go to leave

	dec FreqRelRepeat,x		// Decrease the repeat counter
	bpl !DelayReload+		// More repeats to go for this command

	ldy FreqRelIndex,x		// Load the table index pointer

!FirstPass:
	lda FreqRel_L,x			// Build the Zero-page to the table
	sta ZPS_L
	lda FreqRel_H,x
	sta ZPS_H

!Reindexed:
	lda (ZPS_L),y			// Fetch the next byte from the table
	cmp #JIU			// Is it an index command?
	bne !NoReindex+			// Nope, so dont reindex
	iny				// Increase the index pointer
	lda (ZPS_L),y			// Fetch the 'Until' value
	bmi !Waiting+			// Negative values loops forever
	cmp NoteCounter,x		// Has the note reached this point?
	bcc !Waiting+			// No? Then fetch the index
	iny				// Yes? Then skip the JTP command
	iny				// and continue on in the sequence
	jmp !Reindexed-

!Waiting:
	iny
	lda (ZPS_L),y			// Fetch the value from Delay/Index
	tay				// Turn it into the new index pointer
	jmp !Reindexed-			// Now go fetch new data with the index

!NoReindex:
	sta FreqRelRepeat,x		// Store the value in the repeat counter
	cmp #RFS
	beq !FinishedMod+

	iny				// Increase the pointer ZP+1
	lda (ZPS_L),y			// Fetch the Delay/Index value
	sta FreqRelDelay,x		// Store it also
	iny				// Increase the pointer ZP+2
	lda (ZPS_L),y			// Fetch the Control Reg Absolute Value
	sta FreqRelVal_L,x		// Store it out of ZP for faster access
	iny				// Increase to the last byte ZP+3
	lda (ZPS_L),y			// Fetch the Control Reg Relative Value
	sta FreqRelVal_H,x		// Store it out of ZP for faster access
	iny				// Increase the pointer ZP+4
	tya
	sta FreqRelIndex,x		// Store it ready for the next command
!DelayReload:

	lda FreqRelDelay,x
	sta FreqRelDelayCounter,x

!GotDatatoProcess:
	lda FreqRelVal_L,x		// Add the values as a signed number
	clc
	adc SIDFreq_L,x
	sta SIDFreq_L,x			// Low byte first then
	lda FreqRelVal_H,x		
	adc SIDFreq_H,x			// High byte
	sta SIDFreq_H,x
!FinishedMod:
//..............................................................................
ModulateADSR:
	lda RestartModulator,x		// Has the module been told never to
	and #MET			// restart
	bne !NormalRuntime+

	lda ADSRDelayCounter,x		// Fetch the delay counter
	cmp #$fe			// and check to see if the sequencer
					// has marked this module for re-init
	bne !NormalRuntime+		// No, so continue processing
	ldy #0				// Reset the table index pointer
	beq !FirstPass+			// and fetch the initial values

!NormalRuntime:
	lda ADSRRepeat,x		// Check to see if we have reached
	cmp #RFS			// the end of this table
	beq !FinishedMod+		// if we have then do not modulate
	dec ADSRDelayCounter,x		// Count down the delay
	bpl !FinishedMod+		// more delay to go to leave

	dec ADSRRepeat,x		// Decrease the repeat counter
	bpl !DelayReload+		// More repeats to go for this command

	ldy ADSRIndex,x			// Loads the table index

!FirstPass:
	lda ADSR_L,x			// Build the Zero-page to the table
	sta ZPS_L
	lda ADSR_H,x
	sta ZPS_H

!Reindexed:
	lda (ZPS_L),y			// Fetch the next byte from the table
	cmp #JIU			// Is it an index command?
	bne !NoReindex+			// Nope, so don't reindex
	iny				// Increase the index pointer
	lda (ZPS_L),y			// Fetch the 'Until' value
	bmi !Waiting+			// Negative values loops forever
	cmp NoteCounter,x		// Has the note reached this point?
	bcc !Waiting+			// No? Then fetch the index
	iny				// Yes? Then skip the JTP command
	iny				// and continue on in the sequence
	jmp !Reindexed-

!Waiting:
	iny
	lda (ZPS_L),y			// Fetch the value from Delay/Index
	tay				// Turn it into the new index pointer
	jmp !Reindexed-			// Now go fetch new data with the index

!NoReindex:
	sta ADSRRepeat,x		// Store the value in the repeat counter

	cmp #RFS			// ****Just Added**** So nothing is
	beq !FinishedMod+		// read after a RFS

	iny				// Increase the pointer ZP+1
	lda (ZPS_L),y			// Fetch the Delay/Index value
	sta ADSRDelay,x			// Store it also
	iny				// Increase the pointer ZP+2
	lda (ZPS_L),y			// Fetch the Control Reg Absolute Value
	sta ADSRVal_AD,x		// Store it out of ZP for faster access
	sta SIDAttackDecay,x
	iny				// Increase to the last byte ZP+3
	lda (ZPS_L),y			// Fetch the Control Reg Relative Value
	sta ADSRVal_SR,x		// Store it out of ZP for faster access
	sta SIDSustainRelease,x
	iny				// Increase the pointer ZP+4
	tya
	sta ADSRIndex,x			// Store it ready for the next command

!DelayReload:
	lda ADSRDelay,x
	sta ADSRDelayCounter,x
!FinishedMod:
//..............................................................................
ModulateFilter:
	ldy WhichSID,x			// Find which SID is assigned to voiceX
	lda Filter_H,x			// If there is no high byte 
	bne !GotTable+			// then we bypass this modulator
	jmp !FinishedMod+
!GotTable:
	lda RestartModulator,x
	and #MFL
	bne !NormalRuntime+

	lda FilterDelayCounter,x	// Fetch the delay counter
	cmp #$fe			// and check to see if the sequencer
					// has marked this module for re-init
	bne !NormalRuntime+		// No, then continue processing
	lda #0				// FIlter Cutoff is additive so clear
	sta SIDFilterCutoff_L,x		// the initial pulse width
	sta SIDFilterCutoff_H,x
	tay				// Reset the table index pointer
	beq !FirstPass+			// and fetch the initial values

!NormalRuntime:
	lda FilterRepeat,x		// Check to see if we have reached
	cmp #RFS			// the end of this table
	beq !FinishedMod+		// if we have then do not modulate
	dec FilterDelayCounter,x		// Count down the delay
	bpl !FinishedMod+		// more delay to go to leave

	dec FilterRepeat,x		// Decrease the repeat counter
	bpl !DelayReload+		// More repeats to go for this command

	ldy FilterIndex,x		// Fetch the table index pointer

!FirstPass:
	lda Filter_L,x			// Build the Zero-page to the table
	sta ZPS_L
	lda Filter_H,x
	sta ZPS_H
!Reindexed:
	lda (ZPS_L),y			// Fetch the next byte from the table
	cmp #JIU			// Is it an index command?
	bne !NoReindex+			// Nope, so don't reindex
	iny				// Increase the index pointer
	lda (ZPS_L),y			// Fetch the 'Until' value
	bmi !Waiting+			// Negative values loops forever
	cmp NoteCounter,x		// Has the note reached this point?
	bcc !Waiting+			// No? Then fetch the index
	iny				// Yes? Then skip the JTP command
	iny				// and continue on in the sequence
	jmp !Reindexed-

!Waiting:
	iny
	lda (ZPS_L),y			// Fetch the value from Delay/Index
	tay				// Turn it into the new index pointer
	jmp !Reindexed-			// Now go fetch new data with the index

!NoReindex:
	sta FilterRepeat,x		// Store the value in the repeat counter
	cmp #RFS
	beq !FinishedMod+

	iny				// Increase the pointer ZP+1
	lda (ZPS_L),y			// Fetch the Delay/Index value
	sta PulseDelay,x		// Store it also
	iny				// Increase the pointer ZP+2
	lda (ZPS_L),y			// Fetch the Control Reg Absolute Value
	sta FilterVal_L,x		// Store it out of ZP for faster access
	iny				// Increase to the last byte ZP+3
	lda (ZPS_L),y			// Fetch the Control Reg Relative Value
	sta FilterVal_H,x		// Store it out of ZP for faster access
	iny				// Increase the pointer ZP+4
	tya
	sta FilterIndex,x		// Store it ready for the next command
!DelayReload:

	lda FilterDelay,x
	sta FilterDelayCounter,x

!GotDatatoProcess:
	ldy WhichSID,x			// Find which SID is assigned to voiceX
	lda FilterVal_L,x		// Add the discreet values for each
	clc				// voice to the shadow sid
	adc SIDFilterCutoff_L,y
	sta SIDFilterCutoff_L,y		// Store the low byte
	lda FilterVal_H,x		
	adc SIDFilterCutoff_H,y		// Store the high byte
	sta SIDFilterCutoff_H,y
!FinishedMod:

	ldy WhichSID,x			// Fetch which SID this voice applies to
	lda FilterTypeVol,x		// Fetch the filter type (Low,Pass,High)
	ora SIDFilterTypeVol,y		// Bit push it to the shadow SID
	sta SIDFilterTypeVol,y
	lda FilterResCont,x
	ora SIDFilterResCont,y
	sta SIDFilterResCont,y
CheckFilterEnable:
	lda FilterTypeVol,x		// If there is a filter type then we
	beq NoFilterEnable		// need to apply is to the shadow 
	lda SIDFilterResCont,y		// SID
	ora FilterBits,x		// Note: This could possibly be 
	sta SIDFilterResCont,y		// optimised/merged in the above
NoFilterEnable:
//..............................................................................
Slider:
	lda SlideSpeed,x		// Is the slider waiting?
	beq NoSlider

	lda TempoCounter		// Only test on a tempo tick
	bne NoSlider
	
	lda SlideDelay,x		// Has the delay counted down?
	beq !Go+
	dec SlideDelay,x		// No? then decrment the delay
	jmp NoSlider
!Go:
	lda SlideSpeed,x		// What direction is the slide going?
	bmi SlideDown
	sta PortSpeed,x			// Set the speed in the portamento
	lda #$ff			// Aim for the highest note
	sta PortFreq_L,x
	sta PortFreq_H,x
	lda #0				// pretend that we are coming from 0
	sta PortFromNote,x
	beq SlideStore
SlideDown:
	eor #$ff			// convert negative value to positive
	adc #1
	sta PortSpeed,x			// store as the portamento speed
	lda #$5f
	sta PortFromNote,x		// Pretend we are porting from high
	lda #$00			// and to the lowest frequncy
	sta PortFreq_L,x
	sta PortFreq_H,x
SlideStore:
	sta SlideSpeed,x		// Slide watcher has done its job
					// by filling the portamento routine
					// with initial values so turn-off
					// the slider routine.
NoSlider:
//..............................................................................
Portamento:
	lda PortSpeed,x			// If the Portamento speed is 0
	beq !NoPortamento+		// then there is no point processing
	lda PortFromNote,x		// See which direction the portamento
	cmp NoteNumber,x		// is going by comparing the from and to
	beq !NoPortamento+		// note numbers
	bcs !PortamentoDown+
	lda SIDFreq_L,x			// ADD speed to the frequency for
	clc				// an upward slide
	adc PortSpeed,x
	sta SIDFreq_L,x
	bcc !NoMSBInc+
	inc SIDFreq_H,x
!NoMSBInc:
	lda PortFreq_H,x		// Has the high byte of the frequency
	cmp SIDFreq_H,x			// been reached?
	bne !NoPortamento+
	lda PortFreq_L,x		// If so, has the low byte also?
	cmp SIDFreq_L,x
	bcc !NoPortamento+
	bcs !StoreFinalValues+		// Low is >= so fix the final frequency
!PortamentoDown:
	lda SIDFreq_L,x			// SUB the speed from the frequency
	sec
	sbc PortSpeed,x
	sta SIDFreq_L,x
	bcs !NoMSBInc+
	dec SIDFreq_H,x
!NoMSBInc:
	lda PortFreq_H,x		// Has the high byte of the frequency
	cmp SIDFreq_H,x			// been reached?
	bne !NoPortamento+
	lda PortFreq_L,x
	cmp SIDFreq_L,x
	bcs !NoPortamento+
!StoreFinalValues:
	sta SIDFreq_L,x			// Adjust the low byte in case we
					// added or subbed beyond the to freq
	lda #0				// Switch off the portamento routine
	sta PortSpeed,x
!NoPortamento:

// ----- Once all voices are processed then apply it to the SID(s) -------------
GetNextVoiceIndex:
	dex				// Decrement the voice counter
	bmi TempoCountdown		// Has each voice been processed?
	jmp ProcessNextVoice

TempoCountdown:
	ldy TempoCounter
	dey
					// Has the counter passed #$FF and
	bpl CopyShadowSIDtoReal		// triggered a new note fetch/countdown?
	ldy CurrentTempo

CopyShadowSIDtoReal:
	sty TempoCounter		// Store the value back in the counter

	ldy CurrentNumberofVoices	// Get the number of voices
	cpy #3				// if there is enough for a normal SID
	bcs ProcessSecondSID		// then dont process the 2nd SID
					// because normally the 2nd SID address
					// is a repeat of 1st SID
	jmp ProcessFirstSID
ProcessSecondSID:
	lda SIDFreq_L+3
	sta SID2+$00
	lda SIDFreq_H+3
	sta SID2+$01
	lda SIDFreq_L+4
	sta SID2+$07
	lda SIDFreq_H+4
	sta SID2+$08
	lda SIDFreq_L+5
	sta SID2+$0e
	lda SIDFreq_H+5
	sta SID2+$0f
	lda SIDPulse_L+3
	sta SID2+$02
	lda SIDPulse_H+3
	sta SID2+$03
	lda SIDPulse_L+4
	sta SID2+$09
	lda SIDPulse_H+4
	sta SID2+$0a
	lda SIDPulse_L+5
	sta SID2+$10
	lda SIDPulse_H+5
	sta SID2+$11
	lda SIDAttackDecay+3
	sta SID2+$05
	lda SIDSustainRelease+3
	sta SID2+$06
	lda SIDAttackDecay+4
	sta SID2+$0c
	lda SIDSustainRelease+4
	sta SID2+$0d
	lda SIDAttackDecay+5
	sta SID2+$13
	lda SIDSustainRelease+5
	sta SID2+$14

	lda SIDFilterCutoff_L+1
	sta ZPST
	and #7
	sta SID2+$15
	lda SIDFilterCutoff_H+1
	lsr
	ror ZPST
	lsr
	ror ZPST
	lsr
	ror ZPST
	lda ZPST
	sta SID2+$16
	lda SIDFilterResCont+1
	sta SID2+$17
	lda SIDFilterTypeVol+1
	ora SongVolume+1
	sta SID2+$18

ProcessFirstSID:
	lda SIDFreq_L
	sta SID1+$00
	lda SIDFreq_H
	sta SID1+$01
	lda SIDFreq_L+1
	sta SID1+$07
	lda SIDFreq_H+1
	sta SID1+$08
	lda SIDFreq_L+2
	sta SID1+$0e
	lda SIDFreq_H+2
	sta SID1+$0f
	lda SIDPulse_L
	sta SID1+$02
	lda SIDPulse_H
	sta SID1+$03
	lda SIDPulse_L+1
	sta SID1+$09
	lda SIDPulse_H+1
	sta SID1+$0a
	lda SIDPulse_L+2
	sta SID1+$10
	lda SIDPulse_H+2
	sta SID1+$11
	lda SIDAttackDecay
	sta SID1+$05
	lda SIDSustainRelease
	sta SID1+$06
	lda SIDAttackDecay+1
	sta SID1+$0c
	lda SIDSustainRelease+1
	sta SID1+$0d
	lda SIDAttackDecay+2
	sta SID1+$13
	lda SIDSustainRelease+2
	sta SID1+$14

	lda SIDFilterCutoff_L
	sta ZPST
	and #7
	sta SID1+$15
	lda SIDFilterCutoff_H
	lsr
	ror ZPST
	lsr
	ror ZPST
	lsr
	ror ZPST
	lda ZPST
	sta SID1+$16
	lda SIDFilterResCont+0
	sta SID1+$17
	lda SIDFilterTypeVol+0
	ora SongVolume+0
	sta SID1+$18
SetControlRegisters:
	lda SIDControlReg		// Set the Control on a normal SID
	sta SID1+$04
	lda SIDControlReg+1
	sta SID1+$0b
	lda SIDControlReg+2
	sta SID1+$12

	cpy #3				// We still have Y so compare again
	bcc PlayerExit			// just a normal SID so leave now

	lda SIDControlReg+3
	sta SID2+$04
	lda SIDControlReg+4
	sta SID2+$0b
	lda SIDControlReg+5
	sta SID2+$12

PlayerExit:
	rts
//====== SEQUENCE COMMANDS =====================================================
//------ These are jumped to via a table of command vectors --------------------
INSTR_cmd:
	iny				// Increase the sequence index
	sty ZPST			// Put the sequence index away for
					// later use

	lda (ZPS_L),y			// Fetch the next byte which is the
					// instrument number
	iny				// Increment 'Y' so it is ready for the
					// next comannd/note
	sty ZPST

	asl				// Instrument table is split into
	asl				// 8 low bytes and 8 high bytes
	asl				// so multiply by 8
	tay				// Get it ready as an index

	lda Instruments_L+3,y		// Load the pointers for 
	sta ControlReg_L,x		// the control table (Waveform etc)
	lda Instruments_H+3,y
	sta ControlReg_H,x

	lda Instruments_L,y		// Load the pointers for
	sta FreqNote_L,x		// the frequency absolute table
	lda Instruments_H,y
	sta FreqNote_H,x

	lda Instruments_L+1,y		// Load the pointers to
	sta FreqRel_L,x			// the frequency relative table
	lda Instruments_H+1,y
	sta FreqRel_H,x

	lda Instruments_L+2,y		// Load the pointers to
	sta Pulse_L,x			// the pulse-width table
	lda Instruments_H+2,y
	sta Pulse_H,x

	lda Instruments_L+4,y		// Load the pointers to
	sta ADSR_L,x			// the envelope table
	lda Instruments_H+4,y
	sta ADSR_H,x

	lda Instruments_L+5,y		// Load the pointers to
	sta Filter_L,x			// the filter table
	lda Instruments_H+5,y
	sta Filter_H,x

	lda Instruments_L+6,y		// Set the filter resonance and type
	sta FilterResCont,x
	lda Instruments_L+7,y
	sta FilterTypeVol,x

	ldy ZPST			// Reload the Sequence index

	jmp GetNextCommand		// and then jump to go fetch it
//..............................................................................
EXECS_cmd:
	iny
	lda (ZPS_L),y			// Get the LSB of the code to exec
	sta EXECSJump+1			// Modify the LSB of the JMP address
	iny
	lda (ZPS_L),y			// Get the MSB of the code to exec
	sta EXECSJump+2			// Modify the MSB of the JMP address
	iny				// Move the index along
EXECSJump:
	jmp GetNextCommand		// Nasty self-modifying jump. Arf!
//..............................................................................
TIENOTE_cmd:
	iny
	lda (ZPS_L),y			// Fetch the note duration
	sta NoteCounter,x		// Store the duration
	iny				// Increment the pointer
	jmp StoreZP			// Thats all for now.
//..............................................................................
PORTA_cmd:
	iny
	lda (ZPS_L),y			// Fetch the note which we will
	clc
	adc NoteTranspose,x
	sta PortFromNote,x		// portamento from

	sty ZPST
	tay				// Transfer the note into the 'Y' index
	lda FreqTable_L,y		// Store the 'From' note frequency
	sta SIDFreq_L,x			// into the shadow SID
	lda FreqTable_H,y
	sta SIDFreq_H,x
	ldy ZPST
	iny

	lda (ZPS_L),y			// Fetch the portamento speed
	sta PortSpeed,x

	iny
	sty ZPST

	lda (ZPS_L),y
	clc
	adc NoteTranspose,x		// add any transpose that may be
	sta NoteNumber,x		// store as the destination note
	tay
	lda FreqTable_L,y		// Convert the from note into
	sta PortFreq_L,x		// a frequency that the portamento
	lda FreqTable_H,y		// routine will check against
	sta PortFreq_H,x
	ldy ZPST
	jmp FetchNoteLength		// Re-use part of the new note code
					// to get the note duration
//..............................................................................
JSUBS_cmd:
	iny				// Index++ for 1st param (LSB)
	lda (ZPS_L),y			// Get the LSB of the subroutine
	sta JumpSubSequence_L+1		// Store for fast retrieval
	iny				// Index++ for 2nd param (MSB)
	lda (ZPS_L),y			// Get the MSB of the subroutine
	sta JumpSubSequence_H+1		// Store it for fast retrieval
	
	iny				// Position after JSUBS
	sty JSUBSYIndexReload+1		// Self-modifying code to add the
					// current Y tp (ZP) for return address
	lda StackIndex,x		// Get the stack index
	clc
	adc StackVoiceOffset,x		// Add the offset for the voice
	tay				// Turn it into a register
	lda ZPS_L			// Get the LSB of current sequence
	clc
JSUBSYIndexReload:
	adc #0				// Add zero page index to return address
	sta StackPointer_L,y		// Store LSB return address in stack
	lda ZPS_H			// Do the same for MSB
	adc #0				// In case the returning address x-over
	sta StackPointer_H,y		// Store MSB return address in stack
	inc StackIndex,x		// Increase the stack index for the next
					// JSUBS/FORSQ that may ocurr
JumpSubSequence_L:
	lda #0				// Quick retrieval of subs LSB
	sta ZPS_L			// Store ready to get new notes/cmds
					// in the subroutine/sequence
JumpSubSequence_H:
	lda #0				// Quick retrieval of subs MSB
	sta ZPS_H			// Store ready to get new notes/cmds
	ldy #0				// Reset sequence index
	jmp GetNextCommand
//..............................................................................
RETFS_cmd:
	dec StackIndex,x
	bmi !EndVoice+
					// Use the Stack Index to see if the
					// voice/channel is enabled.
					// If the stack is negative (signed #)
					// then no more to do. Voice will stop

	lda StackIndex,x		// Get the stack index
	clc
	adc StackVoiceOffset,x		// Add the offset for the voice
	tay				// Turn it into a register
	lda StackPointer_L,y		// Get the LSB of the return address
	sta ZPS_L			// Store ready to get new notes/cmds
	lda StackPointer_H,y		// Get the MSB of the return address
	sta ZPS_H			// Store ready to get new notes/cmds
	ldy #0				// Reset the sequence index
	jmp GetNextCommand
!EndVoice:
	jmp StoreZP			// Stack empty. Last RETFS reached
					// Voice now exits as Stack index
					// is negative
//..............................................................................
JTPOS_cmd:
	iny				// Increase index to fetch next byte
	lda (ZPS_L),y			// Fetch the new low byte of the pointer
	sta ZPST			// Temp store it as we are still using
					// the current pointer to fetch the new
	iny				// Increase the index
	lda (ZPS_L),y			// Fetch the High byte of the pointer
	sta ZPS_H			// We are done with the current pointer
					// so load the new one
	lda ZPST			// And load the low byte
	sta ZPS_L
	ldy #0				// Reset the index
	jmp GetNextCommand		// Fetch data from the new pointer addr.
//..............................................................................
FORSQ_cmd:
	iny				// Get the index ready
	lda (ZPS_L),y			// to store the number of For iterations
	sta ForNextCounter,x		// Put it where the NXT can use it
	iny				// Get the index past this command
	sty ForIndexReload+1		// Store it for creating the sequence
					// pointer in the stack
	lda StackIndex,x		// Get the stack index
	clc
	adc StackVoiceOffset,x		// Add the offset for the voice
	tay				// Prepare it to index into stack values
	lda ZPS_L			// Get the current sequence pointer
	clc
ForIndexReload:
	adc #0				// Nasty self-modifying code
					// Add the zero page index
	sta StackPointer_L,y		// Store it so the NEXTS knows where
	lda ZPS_H			// to loop back to in the sequence
	adc #0
	sta StackPointer_H,y
	inc StackIndex,x		// Increase the stack index for any
					// future JSUBS/FORSQ nesting
	ldy ForIndexReload+1		// Sneaky reload of Y index for current
					// sequence work to continue
	jmp GetNextCommand
//..............................................................................	
NEXTS_cmd:
	dec ForNextCounter,x		// Decrement the for counter
	bne !Iterate+			// Is there another iteration?
	iny				// No? Well then push the index past
					// the NEXTS command
	dec StackIndex,x		// Effectively pulls the loop from
					// the stack. Pointer overwritten
					// on future stack push
	jmp GetNextCommand		// Retrieve next note/commands
!Iterate:
	lda StackIndex,x		// Get the stack index
	clc
	adc StackVoiceOffset,x		// Add the offset for the voice
	tay				// Prepare it to index into stack values
	dey				// Stack index is advanced on every push
					// so we wind back to the last pointer
					// so we know where to loop back to
					// in the sequence
	lda StackPointer_L,y		// Restore the Zero-Page address
	sta ZPS_L			// ready for looping back
	lda StackPointer_H,y
	sta ZPS_H
	ldy #0				// Reset the pointer 
	jmp GetNextCommand
//..............................................................................
TRANS_cmd:
	iny
	lda (ZPS_L),y			// Fetch the signed semitone value that
	sta NoteTranspose,x		// each note will add
	iny
	jmp GetNextCommand
//..............................................................................
GATEM_cmd:
	iny
	lda (ZPS_L),y			// Fetch the value that will be or'd
	sta GateModify,x		// into the control byte
	iny
	jmp GetNextCommand
//..............................................................................
REMOD_cmd:
	iny
	lda (ZPS_L),y			// Toggle the restart modulator
	eor RestartModulator,x		// byte by exlusive or'ing the value
	sta RestartModulator,x
	iny
	jmp GetNextCommand
//..............................................................................
TEMPO_cmd:
	iny
	lda (ZPS_L),y			// Fetch the new song tempo
	sta CurrentTempo		// and store it for use
	iny
	jmp GetNextCommand
!EndInstruction:
//..............................................................................
SETAD_cmd:
	iny
	lda (ZPS_L),y			// Fetch the Attack/Decay value
	sta SIDAttackDecay,x		// place it in the shadow SID
	iny
	jmp GetNextCommand
//..............................................................................
SETSR_cmd:
	iny
	lda (ZPS_L),y			// Fetch the Sustain/Release value
	sta SIDSustainRelease,x		// store it in the shadow SID
	iny
	jmp GetNextCommand
//..............................................................................
CHNOTE_cmd:
	iny
	lda (ZPS_L),y			// Fetch the low byte of the pointer
	sta FreqNote_L,x		// store it 
	iny
	lda (ZPS_L),y			// Fetch the high byte of the pointer
	sta FreqNote_H,x
	iny
	jmp GetNextCommand
//..............................................................................
CHFREQ_cmd:
	iny
	lda (ZPS_L),y			// Fetch the low byte of the pointer
	sta FreqRel_L,x			// store it 
	iny
	lda (ZPS_L),y			// Fetch the high byte of the pointer
	sta FreqRel_H,x
	iny
	jmp GetNextCommand
//..............................................................................
CHPULS_cmd:
	iny
	lda (ZPS_L),y			// Fetch the low byte of the pointer
	sta Pulse_L,x			// store it 
	iny
	lda (ZPS_L),y			// Fetch the high byte of the pointer
	sta Pulse_H,x
	iny
	jmp GetNextCommand
//..............................................................................
CHCTRL_cmd:
	iny
	lda (ZPS_L),y			// Fetch the low byte of the pointer
	sta ControlReg_L,x		// store it 
	iny
	lda (ZPS_L),y			// Fetch the high byte of the pointer
	sta ControlReg_H,x
	iny
	jmp GetNextCommand
//..............................................................................
CHENVL_cmd:
	iny
	lda (ZPS_L),y			// Fetch the low byte of the pointer
	sta ADSR_L,x			// store it 
	iny
	lda (ZPS_L),y			// Fetch the high byte of the pointer
	sta ADSR_H,x
	iny
	jmp GetNextCommand
//..............................................................................
CHFILT_cmd:
	iny
	lda (ZPS_L),y			// Fetch the low byte of the pointer
	sta Filter_L,x			// store it 
	iny
	lda (ZPS_L),y			// Fetch the high byte of the pointer
	sta Filter_H,x
	iny
	jmp GetNextCommand
//..............................................................................
SLIDE_cmd:
	iny
	lda (ZPS_L),y			// Fetch the slide delay
	sta SlideDelay,x		// store it 
	iny
	lda (ZPS_L),y			// Fetch the slide speed
	sta SlideSpeed,x
	iny
	jmp GetNextCommand
//..............................................................................

//====== Storage of variables used for position tracking in songs ==============
//------ and intruments and whatever else that needs storage -------------------
.pc	= * "zBlex V6 Data"
PlayerEnabled:			// Whether the player is active
	.byte	0
TempoCounter:			// This is the countdown byte for the tempo
	.byte	0	

//----- These variables mimic the Music Index table so we can fast-fill them ---
//----- at the point of song initialisation ------------------------------------

SeqPointer_L:
	.byte	0,0,0,0,0,0	// Low byte of pointer address for voice 1 - 6
SeqPointer_H:
	.byte	0,0,0,0,0,0	// High byte of pointer address for voice 1 - 6
CurrentTempo:
	.byte	0
CurrentNumberofVoices:
	.byte	0
	.byte	0,0		// This matches the two reserved bytes in MIndex
SongVolume:
	.byte	$0f,$0f		// Song Volume
WhichSID:
	.byte	0,0,0,1,1,1	// Matches voice number in 'X' to a SID.
//..............................................................................
ForNextStack_L:
	.byte	0,0,0,0,0,0
ForNextStack_H:
	.byte	0,0,0,0,0,0
ForNextCounter:
	.byte	0,0,0,0,0,0
//----- More various variables -------------------------------------------------
NoteCounter:			// Counts down the length of the note 
	.byte	0,0,0,0,0,0	// for up to 6 voices
NoteNumber:			// Store the current note number
	.byte	0,0,0,0,0,0
//..............................................................................
SlideDelay:			// Delay before note slides
	.byte	0,0,0,0,0,0
SlideSpeed:
	.byte	0,0,0,0,0,0
//..............................................................................
PortFreq_L:			// The frequency we want the portamento to reach
	.byte	0,0,0,0,0,0
PortFreq_H:
	.byte	0,0,0,0,0,0
PortFromNote:			// The note we are coming from. Calcs direction
	.byte	0,0,0,0,0,0
PortSpeed:			// The value added to the frequency
	.byte	0,0,0,0,0,0
//..............................................................................
NoteTranspose:			// Transpose / Key changer
	.byte	0,0,0,0,0,0
//..............................................................................
RestartModulator:		// Controls if the modulators are restarted
	.byte	0,0,0,0,0,0	// on each new note
//..............................................................................
GateModify:
	.byte	0,0,0,0,0,0	// Or'd to Control byte to stop note off 
//..............................................................................
ControlReg_L:			// **** Need to add comments here ****
	.byte	0,0,0,0,0,0
ControlReg_H:
	.byte	0,0,0,0,0,0
ControlRegIndex:
	.byte	0,0,0,0,0,0
ControlRegRepeat:
	.byte	0,0,0,0,0,0
ControlRegDelay:
	.byte	0,0,0,0,0,0
ControlRegABS:
	.byte	0,0,0,0,0,0
ControlRegREL:
	.byte	0,0,0,0,0,0
ControlRegDelayCounter:
	.byte	0,0,0,0,0,0
//..............................................................................
FreqRel_L:
	.byte	0,0,0,0,0,0
FreqRel_H:
	.byte	0,0,0,0,0,0
FreqRelIndex:
	.byte	0,0,0,0,0,0
FreqRelRepeat:
	.byte	0,0,0,0,0,0
FreqRelDelay:
	.byte	0,0,0,0,0,0
FreqRelVal_L:
	.byte	0,0,0,0,0,0
FreqRelVal_H:
	.byte	0,0,0,0,0,0
FreqRelDelayCounter:
	.byte	0,0,0,0,0,0
//..............................................................................
FreqNote_L:
	.byte	0,0,0,0,0,0
FreqNote_H:
	.byte	0,0,0,0,0,0
FreqNoteIndex:
	.byte	0,0,0,0,0,0
FreqNoteRepeat:
	.byte	0,0,0,0,0,0
FreqNoteDelay:
	.byte	0,0,0,0,0,0
FreqNoteABS:
	.byte	0,0,0,0,0,0
FreqNoteRel:
	.byte	0,0,0,0,0,0
FreqNoteDelayCounter:
	.byte	0,0,0,0,0,0
//..............................................................................
Pulse_L:
	.byte	0,0,0,0,0,0
Pulse_H:
	.byte	0,0,0,0,0,0
PulseIndex:
	.byte	0,0,0,0,0,0
PulseRepeat:
	.byte	0,0,0,0,0,0
PulseDelay:
	.byte	0,0,0,0,0,0
PulseVal_L:
	.byte	0,0,0,0,0,0
PulseVal_H:
	.byte	0,0,0,0,0,0
PulseDelayCounter:
	.byte	0,0,0,0,0,0
//..............................................................................
ADSR_L:
	.byte	0,0,0,0,0,0
ADSR_H:
	.byte	0,0,0,0,0,0
ADSRIndex:
	.byte	0,0,0,0,0,0
ADSRRepeat:
	.byte	0,0,0,0,0,0
ADSRDelay:
	.byte	0,0,0,0,0,0
ADSRVal_AD:
	.byte	0,0,0,0,0,0
ADSRVal_SR:
	.byte	0,0,0,0,0,0
ADSRDelayCounter:
	.byte	0,0,0,0,0,0
//..............................................................................
Filter_L:
	.byte	0,0,0,0,0,0
Filter_H:
	.byte	0,0,0,0,0,0
FilterIndex:
	.byte	0,0,0,0,0,0
FilterRepeat:
	.byte	0,0,0,0,0,0
FilterDelay:
	.byte	0,0,0,0,0,0
FilterVal_L:
	.byte	0,0,0,0,0,0
FilterVal_H:
	.byte	0,0,0,0,0,0
FilterDelayCounter:
	.byte	0,0,0,0,0,0
FilterBits:
	.byte	1,2,4,1,2,4
FilterResCont:
	.byte	0,0,0,0,0,0
FilterTypeVol:
	.byte	0,0,0,0,0,0
//------ The Shadow SID chip(s) arranged for fast access by the voice loop -----
SIDFreq_L:
	.byte	0,0,0,0,0,0
SIDFreq_H:
	.byte	0,0,0,0,0,0
SIDPulse_L:
	.byte	0,0,0,0,0,0
SIDPulse_H:
	.byte	0,0,0,0,0,0
SIDControlReg:
	.fill	6,8
SIDAttackDecay:
	.byte	0,0,0,0,0,0
SIDSustainRelease:
	.byte	0,0,0,0,0,0
SIDFilterCutoff_L:
	.byte	0,0
SIDFilterCutoff_H:
	.byte	0,0
SIDFilterResCont:
	.byte	0,0				// Resonance + Filter Enable
SIDFilterTypeVol:
	.byte	$0f,$0f				// Filter Type + Volume
// ---- Stack ccontrol for JSUBS & FORSQ - Also disables Voices ----------------
StackIndex:
	.fill	6,$ff				// Position in the Stack
						// Also used to disable voices
						// by containing a negative
						// number
StackVoiceOffset:
	.byte	0*STACKDEPTH,1*STACKDEPTH,2*STACKDEPTH
	.byte	3*STACKDEPTH,4*STACKDEPTH,5*STACKDEPTH
StackPointer_L:
	.fill	STACKDEPTH*6,0			// LSB of Stacked addresses
StackPointer_H:
	.fill	STACKDEPTH*6,0			// MSB of Stacked addresses
// ---- These are the vectors to the commands ----------------------------------
CommandVectors_L:
	.byte	<RETFS_cmd,<JTPOS_cmd,<JSUBS_cmd,<FORSQ_cmd,<NEXTS_cmd
	.byte	<REMOD_cmd,<CHFILT_cmd,<CHENVL_cmd,<CHCTRL_cmd,<CHPULS_cmd
	.byte	<CHFREQ_cmd,<CHNOTE_cmd,<SETSR_cmd,<SETAD_cmd
	.byte	<INSTR_cmd,<EXECS_cmd,<TIENOTE_cmd,<PORTA_cmd
	.byte	<TRANS_cmd,<GATEM_cmd,<SLIDE_cmd,<TEMPO_cmd
CommandVectors_H:
	.byte	>RETFS_cmd,>JTPOS_cmd,>JSUBS_cmd,>FORSQ_cmd,>NEXTS_cmd
	.byte	>REMOD_cmd,>CHFILT_cmd,>CHENVL_cmd,>CHCTRL_cmd,>CHPULS_cmd
	.byte	>CHFREQ_cmd,>CHNOTE_cmd,>SETSR_cmd,>SETAD_cmd
	.byte	>INSTR_cmd,>EXECS_cmd,>TIENOTE_cmd,>PORTA_cmd
	.byte	>TRANS_cmd,>GATEM_cmd,>SLIDE_cmd,>TEMPO_cmd
// ----- Note frequency split into two tables for speed ------------------------
FreqTable_L:
	.byte 	$0c,$1c,$2d,$3e,$47,$66,$7b,$91
	.byte	$a9,$c3,$dd,$fa,$18,$38,$5a,$7d
	.byte	$a3,$cc,$f6,$23,$53,$86,$bb,$f4
	.byte	$30,$70,$b4,$fb,$47,$98,$ed,$47
	.byte	$a7,$0c,$77,$e9,$61,$e1,$68,$f7
	.byte	$8f,$30,$da,$8f,$4e,$18,$ef,$d2
	.byte	$c3,$c3,$d1,$ef,$1f,$60,$b5,$1e
	.byte	$9c,$31,$df,$a5,$87,$86,$a2,$df
	.byte	$3e,$c1,$6b,$3c,$39,$63,$be,$4b
	.byte	$0f,$0c,$45,$bf,$7d,$83,$d6,$79
	.byte	$73,$c7,$7c,$97,$1e,$18,$8b,$7e
	.byte	$fa,$06,$ac,$f3,$e6,$8f,$f8,$fc
FreqTable_H:
	.byte	$01,$01,$01,$01,$01,$01,$01,$01
	.byte	$01,$01,$01,$01,$02,$02,$02,$02
	.byte	$02,$02,$02,$03,$03,$03,$03,$03
	.byte	$04,$04,$04,$04,$05,$05,$05,$06
	.byte	$06,$07,$07,$07,$08,$08,$09,$09
	.byte	$0a,$0b,$0b,$0c,$0d,$0e,$0e,$0f
	.byte	$10,$11,$12,$13,$15,$16,$17,$19
	.byte	$1a,$1c,$1d,$1f,$21,$23,$25,$27
	.byte	$2a,$2c,$2f,$32,$35,$38,$3b,$3f
	.byte	$43,$47,$4b,$4f,$54,$59,$5e,$64
	.byte	$6a,$70,$77,$7e,$86,$8e,$96,$9f
	.byte	$a8,$b3,$bd,$c8,$d4,$e1,$ee,$fd
//------------------------------------------------------------------------------
DriverMessage:
	.text	"ZBLEXV6.0 - GAVIN GRAHAM "
//------------------------------------------------------------------------------
.pc	= * "Music Data"
//All instrument low pointers are grouped together
Instruments_L:

INS1_L:
	.byte	<FREQA1		// Frequency Absolute Low Byte
	.byte	<FREQR1		// Frequency Relative Low Byte
	.byte	<PULSE1		// Pulse Low Byte
	.byte	<CTRL1		// Control Register Low Byte
	.byte	<ADSR1		// A/D/S/R Low Byte
	.byte	<FILT1		// Filter Low Byte
	.byte	240		// Filter Resonance
	.byte	FILTLOW		// Filter Type
INS2_L:
	.byte	0,0,0,0,0,0,0,0
//..............................................................................
//All instrument high pointers are grouped together
Instruments_H:

INS1_H:
	.byte	>FREQA1		// Frequency Absolute High Byte
	.byte	>FREQR1		// Frequency Relative High Byte
	.byte	>PULSE1		// Pulse High Byte
	.byte	>CTRL1		// Control Register High Byte
	.byte	>ADSR1		// A/D/S/R High Byte
	.byte	>FILT1		// Filter High Byte
	.byte	0			// RESERVED
	.byte	0			// RESERVED

INS2_H:
	.byte	0,0,0,0,0,0,0,0


//----- Repeat/Command, Delay/Index, Waveform Absolute, Waveform Relative ------
//Format:     |Repeat/Command	|Delay		|Waveform ABS	|Waveform Rel
CTRL1:
//	.byte	0		,2		,$81		,$00
	.byte	0		,0		,$41		,$00
	.byte	JIU		,2		,0
	.byte	0		,0		,$40		,0
	.byte	RETFS

//Format:     |Repeat/Command	|Delay		|Freq Replace	|Freq Add
FREQA1:
//	.byte	0		,2		,B7		,$80
//	.byte	0		,0		,$80		,0
	.byte	RETFS

FREQR1:
	.byte	0		,32
	.word					0
	.byte	2		,0
	.word					-10
FREQR1P1:
	.byte	4		,0
	.word					10
	.byte	4		,0
	.word	-10
	.byte	JIU		,255		,8

PULSE1:
	.byte	0		,0
	.word					$0700
	.byte	32		,0
	.word					16
PULSE1P1:
	.byte	16		,0
	.word					64
	.byte	16		,0
	.word					-64
	.byte 	JIU		,255		,8

ADSR1:
	.byte	0		,0		,$09		,$4a
	.byte	JIU		,2		,0
	.byte	0		,0		,$09		,$49
	.byte	RFS

FILT1:
	.byte	0		,8
	.word					1500
	.byte	32		,0
	.word					-20
	.byte	10		,0
	.word					-10
	.byte	10		,0
	.word					10
	.byte	JIU		,255		,8

//====== The address table to the sequences for each voice =====================

MusicIndex:		// This is the pointer table to each voice in the tune
			// It is low/high bytes of each sequence
			// followed by the tempo and voices used in this piece

Song1:
	.byte	<S1V1,<S1V1,<S1V1,<S1V1,<S1V1,<S1V1		// Low byte pointers
	.byte	>S1V1,>S1V1,>S1V1,>S1V1,>S1V1,>S1V1		// High byte pointers
	.byte	4		// Tempo
	.byte	1-1		// Number of voices in this song
	.byte	0,0		// Two reserved bytes for future expansion

//======= SONG SEQUENCES =======================================================
//------ Labels are in the format of SnVn[Pn]
//------ for 'S'ong number & 'V'oice number [and optionally 'P'art number]

S1V1:
	.byte	INSTR,0
	.byte	FORSQ,4
//	.byte	G4,4
	.byte	JSUBS,<S1V1P1,>S1V1P1
	.byte	NEXTS
	.byte	GATEM,1,C4,16
	.byte	GATEM,0,REMOD,MODPULS|MODFILT,PORTA,C4,64,G3,16
	.byte	REMOD,MODPULS|MODFILT,A3,16
	.byte	JTPOS,<S1V1,>S1V1		// Jump to Start of Sequence
S1V1P1:
	.byte	G4,4,FS4,8,RETFS
S1V2:
	.byte	RETFS
S1V3:
	.byte	RETFS
S1V4:
	.byte	RETFS
S1V5:
	.byte	RETFS
S1V6:
	.byte	RETFS

