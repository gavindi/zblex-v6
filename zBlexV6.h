//====== Note names for easy entry into the sequences ==========================
.const BLW	= $80			// 'B'reak 'L'oop 'W'hen
					// a note has less that nn counter left
					// before it finishes. If it has more
					// than nn to go, then loop to index
					//
					// (Intruments Only)
					// .byte BLW+[0-127],#nn
					// 	|CMD+Remainder of Note|Index to
//..............................................................................
.const RFS	= $80			// 'R'eturn 'F'rom 'S'equence
.const RETFS	= RFS			// (Sequences Only)
//..............................................................................
.const JTP	= $81			// 'J'ump 'T'o 'P'osition
.const JTPOS	= JTP			// (Sequences Only)
					//
					// .byte JTP,<SnVn[Pn]_L,>SnVn[Pn]_H
					//      |CMD|Low Pointer|Hi Pointer|
//..............................................................................
.const JIU	= $fc			// 'J'ump to 'I'ndex 'U'ntil 
.const JINDX	= JIU			// (Intruments Only)
					//
					// .byte JIU,#nn,#nn
					//      |CMD|Note Counter|Index|
//..............................................................................
.const JSS	= $82			// 'J'ump 'T'o 'S'ubsequence @ Pointer
.const JSUBS	= JSS			// (Sequences only)
					//
					// .byte JSS,<SnVn[Pn]_L,>SnVn[Pn]_H
					//      |CMD|Low Pointer|Hi Pointer|
//..............................................................................
.const FOR	= $83			// Start a pattern loop (Sequences only)
.const FORSQ	= FOR			//
					// .byte FOR,nn,tt
					//      |CMD|# of repeats|Rel transpose
//..............................................................................
.const NXT	= $84			// Next command (Sequences only)
.const NEXTS	= NXT			// End marker for a FOR pattern loop
//..............................................................................
.const RMT	= $85			// 'R'estart 'M'odulator 'T'oggle
.const REMOD	= RMT			// .byte RMT,%xxxxxxxx
					//      |CMD|MFA:MFR:MPW:MCR:MEV:MFT
					// (See Modulator Control Bits Below)
//..............................................................................
.const SFL	= $86			// 'S'et 'F'i'L'ter Table
.const CHFILT	= SFL			// .byte SFN,Table_Lo,Table_Hi
					//      |CMD|Low Pointer|High Pointer
//..............................................................................
.const SET	= $87			// 'S'et 'E'n'V'elope Table
.const CHENVL	= SET			// .byte SFN,Table_Lo,Table_Hi
					//      |CMD|Low Pointer|High Pointer
//..............................................................................
.const SCT	= $88			// 'S'et 'C'ontrol 'R'egister Table
.const CHCTRL	= SCT			// .byte SFN,Table_Lo,Table_Hi
					//      |CMD|Low Pointer|High Pointer
//..............................................................................
.const SPT	= $89			// 'S'et 'P'ulse 'W'idth Table
.const CHPULS	= SPT			// .byte SFN,Table_Lo,Table_Hi
					//      |CMD|Low Pointer|High Pointer
//..............................................................................
.const SFT	= $8a			// 'S'et 'F'ine 'F'requency Table
.const CHFREQ	= SFT			// .byte SFN,Table_Lo,Table_Hi
					//      |CMD|Low Pointer|High Pointer
//..............................................................................
.const SNT	= $8b			// 'S'et 'F'reqency 'N'ote Table
.const CHNOTE	= SNT			// .byte SFN,Table_Lo,Table_Hi
					//      |CMD|Low Pointer|High Pointer
//..............................................................................
.const SSR	= $8c			// 'S'et 'S'ustain/'R'elease
.const SETSR	= SSR			// .byte SAD,#$NN
					//      |CMD|New Sustain/Release Value
//..............................................................................
.const SAD	= $8d			// 'S'et 'A'ttack/'D'ecay
.const SETAD	= SAD			// .byte SAD,#$NN
					//      |CMD|New Attack/Decay Value
//..............................................................................
.const INS	= $8e			// Instrument Change to Pointer
.const INSTR	= INS			// (Sequences only)
					//
					// .byte INS,<INSnn_L,>INSnn_H
					//      |CMD|Lo Pointer|Hi Pointer|
//..............................................................................
.const EXE	= $8f			// Command instruction
.const EXECS	= EXE			// Reserved for future expansion
//..............................................................................
.const TIE	= $90			// Tie this note to the previous
.const TIENOTE	= TIE			// .byte TIE,nn
					//      |CMD|Note Length|
//..............................................................................
.const POR	= $91			// Tone Portamento + TIE
.const PORTA	= POR			// Applied to the note before portamento
					// so modulation & gate isn't reset
					// .byte POR,nn,nn,nn
					//      |CMD|From Note|To Note|Speed|
//..............................................................................
.const TRN	= $92			// Note Transpose
.const TRANS	= TRN			// .byte TRN,nn
					//      |CMD|Signed number of semitones
//..............................................................................
.const GAT	= $93			// Gate Modify
.const GATEM	= GAT			// .byte GAT,nn
					//      |CMD|Number Or'd to Control Reg
//..............................................................................
.const SLD	= $94			// Slide note
.const SLIDE	= SLD			// 
//..............................................................................
.const TEM	= $95			// Set Tempo
.const TEMPO	= TEM			// .byte TEM,nn
					//      |CMD|Tempo
//..............................................................................
.const HARDR	= $96			// Hard Restart
					// .byte HARDR.nn
					//	|CMD|Flag 0=off $80=on
//------ Note Names ------------------------------------------------------------
.const RST	= 96			// Rest Note #$60
.const REST	= RST			//
//..............................................................................
.const C0	= 0			// All the note/octave combo's
.const CS0	= 1
.const D0	= 2
.const DS0	= 3
.const E0	= 4
.const F0	= 5
.const FS0	= 6
.const G0	= 7
.const GS0	= 8
.const A0	= 9
.const AS0	= 10
.const B0	= 11
.const C1	= 12
.const CS1	= 13
.const D1	= 14
.const DS1	= 15
.const E1	= 16
.const F1	= 17
.const FS1	= 18
.const G1	= 19
.const GS1	= 20
.const A1	= 21
.const AS1	= 22
.const B1	= 23
.const C2	= 24
.const CS2	= 25
.const D2	= 26
.const DS2	= 27
.const E2	= 28
.const F2	= 29
.const FS2	= 30
.const G2	= 31
.const GS2	= 32
.const A2	= 33
.const AS2	= 34
.const B2	= 35
.const C3	= 36
.const CS3	= 37
.const D3	= 38
.const DS3	= 39
.const E3	= 40
.const F3	= 41
.const FS3	= 42
.const G3	= 43
.const GS3	= 44
.const A3	= 45
.const AS3	= 46
.const B3	= 47
.const C4	= 48
.const CS4	= 49
.const D4	= 50
.const DS4	= 51
.const E4	= 52
.const F4	= 53
.const FS4	= 54
.const G4	= 55
.const GS4	= 56
.const A4	= 57
.const AS4	= 58
.const B4	= 59
.const C5	= 60
.const CS5	= 61
.const D5	= 62
.const DS5	= 63
.const E5	= 64
.const F5	= 65
.const FS5	= 66
.const G5	= 67
.const GS5	= 68
.const A5	= 69
.const AS5	= 70
.const B5	= 71
.const C6	= 72
.const CS6	= 73
.const D6	= 74
.const DS6	= 75
.const E6	= 76
.const F6	= 77
.const FS6	= 78
.const G6	= 79
.const GS6	= 80
.const A6	= 81
.const AS6	= 82
.const B6	= 83
.const C7	= 84
.const CS7	= 85
.const D7	= 86
.const DS7	= 87
.const E7	= 88
.const F7	= 89
.const FS7	= 90
.const G7	= 91
.const GS7	= 92
.const A7	= 93
.const AS7	= 94
.const B7	= 95				// #$5F

//---- Filter Control Bits -----------------------------------------------------
.const FILTNO		= $00
.const FILTLOW		= $10
.const FILTBAND		= $20
.const FILTHIGH		= $40
.const FILTNOTCH	= $50
//---- Modulator Restart Control Bits ------------------------------------------
.const MNT		= $80			// Modulate Note Table
.const MODNOTE		= MNT
.const MFT		= $40			// Modulate Frequency Table
.const MODFREQ		= MFT
.const MPT		= $20			// Modulate Pulse Width Table
.const MODPULS		= MPT
.const MCT		= $10			// Modulate Control Register
.const MODCTRL		= MCT
.const MET		= $08			// Modulate Envelope Table
.const MODADSR		= MET
.const MFL		= $04			// Modulate Filter Table
.const MODFILT		= MFL
//---- Zero Page Addresses -----------------------------------------------------
				// Use Zero Page for Indexing
.const ZPS_L	= $b0		// Zero-page low byte of sequence pointer
.const ZPS_H	= $b1		// Zero-page high byte of sequence pointer
.const ZPST	= $b2		// Zero-page temp store of sequence index 'Y'
.const SID1	= $d400		// First SID chip
.const SID2	= $d420		// Second SID Chip
//--- Boolean Operators --------------------------------------------------------
.const ON	= 1
.const OFF	= 0
//------------------------------------------------------------------------------
.const STACKDEPTH = 8
//------------------------------------------------------------------------------
//SID-ADR-Table:
//
//     VALUE    ATTACK    DECAY/RELEASE
//   +-------+----------+---------------+
//   |   0   |    2 ms  |      6 ms     |
//   |   1   |    8 ms  |     24 ms     |
//   |   2   |   16 ms  |     48 ms     |
//   |   3   |   24 ms  |     72 ms     |
//   |   4   |   38 ms  |    114 ms     |
//   |   5   |   56 ms  |    168 ms     |
//   |   6   |   68 ms  |    204 ms     |
//   |   7   |   80 ms  |    240 ms     |
//   |   8   |  100 ms  |    300 ms     |
//   |   9   |  240 ms  |    720 ms     |
//   |   A   |  500 ms  |    1.5 s      |
//   |   B   |  800 ms  |    2.4 s      |
//   |   C   |    1 s   |      3 s      |
//   |   D   |    3 s   |      9 s      |
//   |   E   |    5 s   |     15 s      |
//   |   F   |    8 s   |     24 s      |
//   +-------+----------+---------------+					
//										
// ATTACK / DECAY CYCLE CONTROL
// Bits 7-4 Select ATTACK Cycle Duration: 0-15             
// Bits 3-0 Select DECAY Cycle Duration: 0-15  
//        			
// SUSTAIN / RELEASE CYCLE CONTROL
// Bits 7-4 Select Sustain Cycle Duration: 0-15             
// Bits 3-0 Select Release Cycle Duration: 0-15             	
//       								
// WAVEFORM/GATE BIT SET		
// BIT 7 SELECT RANDOM NOISE WAVEFORM, 		1 = ON 
// BIT 6 SELECT PULSE WAVEFORM, 			1 = ON
// BIT 5 SELECT SAWTOOTH WAVEFORM, 		1 = ON
// BIT 4 SELECT TRIANGLE WAVEFORM, 		1 = ON 
// BIT 3 TEST BIT: 1 = DISABLE OSCILLATOR 
// BIT 2 RING MODULATE OSC. 1 WITH OSC. 3 OUTPUT, 1 = ON 
// BIT 1 SYNCHRONIZE OSC. 1 WITH OSC. 3 FREQUENCY, 1 = ON 
// BIT 0 GATE BIT: 1 = START ATT/DEC/SUS, 0 = START RELEASE
// TRIANGLE 				ON 17 	OFF	16
// SAWTOOTH 		 		ON 33 	OFF	32
// PULSE 				ON 65 	OFF 64
// NOISE WAVEFORM 			ON 129 	OFF 128
//
//$D415	FILTER CUTOFF FREQUENCY: LOW-NYBBLE	(BITS 2-0) (0 to 7)
//						
//$D416	FILTER CUTOFF FREQUENCY: HIGH-BYTE 					
//					
// FILTERCON 7-4
// $D417	FILTER RESONANCE CONTROL / VOICE INPUT CONTROL
// 7-4	SELECT FILTER RESONANCE: 0-15
// 3		FILTER EXTERNAL INPUT: 1 = YES, 0 = NO
// 2		FILTER VOICE 3 OUTPUT: 1 = YES, 0 = NO	
// 1		FILTER VOICE 2 OUTPUT: 1 = YES, 0 = NO	
// 0		FILTER VOICE 1 OUTPUT: 1 = YES, 0 = NO				
//					
// FILTERMODE		
// $D418	SELECT FILTER MODE AND VOLUME
// 7		CUT-OFF VOICE 3 OUTPUT: 1 = OFF, 0 = ON  	
// 6		SELECT FILTER HIGH-PASS MODE: 1 = ON				
// 5		SELECT FILTER BAND-PASS MODE: 1 = ON		
// 4		SELECT FILTER LOW-PASS MODE: 1 = ON			
// 3-0	SELECT OUTPUT VOLUME: 0-15
