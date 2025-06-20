; Musik
; 2025 Michael Cassera
; Program used to generate random Mozart waltzes using 
; Musikalisches Wurfelspiel (musical dice game)
; and the dream midi chip in the K2/JR2

.cpu "w65c02"				                ; set the cpu to Western Digital 65C02
.include "setup.asm"		                ; all of our initial settings

;some local Zero Page vars
bar     =	$a9								; word for bar address
barPos	=	$ab 							; byte for position in bar
bcount	=	$ac								; count of 1/16 notes in bar
playCounter = $ad
midiNote	=	$af							; temp storage for midi note
chartLO		=	$b0							; chart pointer
chartHI		=	$b1
phrase		=	$ae							; how many phrases to make a song

*=$a0										; Set up buffer for Kernel communication
.dsection zp						        ; Define position for zp (zero page)
.cerror * > $af, "Too many Zero page variables"

*=$1ffd

start:										; ***TEMP CODE FOR PROGRAMMING***
		jmp SC								; Start of the program - We put this jump here so you can load the PGZ into the computer.
											; With the self running code below, you can boot into it in RAM. Without this jump, loading a PGZ will
											; hit the self running code the kernel needs at the start of the slot at $2000 and look like a freeze.
											; hitting the reset on the back of the F256k will start the program.



.include "api.asm"							; This is the Kernel API for communication

;SetupKernel:								; Set up the API to work with

.section zp									; Zero page section $a0 to $a8
event:	.dstruct	kernel.event.event_t
.send


; ************************************************************************************************************************************
*=$2000										; ***TEMP CODE FOR PROGRAMMING***
											; Self running code to send via USB port the F256. Allows me to quickly load into emulator
											; dipswitch 1 should be set to on.
		.byte $f2,$56						; Required bytes for the Kernel to identify
		.byte $04,$01						; how big is the program in 8K sections, What slot to map to
		.byte $0b,$20						; the starting address of your program
		.byte $00,$00,$00,$00				; reserved
		.byte $00							; terminating byte
; ************************************************************************************************************************************
											; *****My program starts here!*****
SC:


		stz MMU_IO_CTRL						; should do this on every program
		stz phrase							; set phrses to zero


; ************************************************************************************************************************************

;init_events:
        lda #<event
        sta kernel.args.events
        lda #>event
        sta kernel.args.events+1

;Set up TinyVicky to display tiles
		lda #%00010111						; Graphic, Sprites Engine enabled  			|xx|GM|SP|TL|BM|GR|OV|TX|
		sta VKY_MSTR_CTRL_0					; Text overlay enabled						| 0| 0| 0| 1| 0| 1| 1| 1|

		lda #%00000110						; Text mode options for the overlay 		|xx|xx|FS|FO|MS|2Y|2X|70|
		sta VKY_MSTR_CTRL_1					; 320 x 240, 60 Hz, dbl X & Y				| 0| 0| 0| 0| 0| 1| 1| 0|
		stz VKY_BRDR_CTRL					; No Border

		lda #$00							; Set the background color
		sta VKY_BKG_COL_R
		lda #$00
		sta VKY_BKG_COL_G
		lda #$00
		sta VKY_BKG_COL_B

		jsr clrScreen

		

;Set up TinyVicky to display tiles
		lda #%00000000						;no tiles for layer 0 and 1	               |xx|LA YE R1|xx|LA YE R0|
		sta VKY_LAYER_CTRL_0				;						 				   | 0| 0| 0| 0| 0| 0| 0| 0|
		lda #%00000100						;Layer 2 = TileMap 2					   |xx|xx|xx|xx|xx|LA YE R2|	
		sta VKY_LAYER_CTRL_1				;										   | 0| 0| 0| 0| 0| 1| 0| 0|

		jsr setTileMap1

; ************************************************************************************************************************************

; turn on random number generator
		lda #$01
		sta Random_Reg

; ************************************************************************************************************************************

;Load the CLUT into memory
		lda #$01							; Change I/O control to page 1
		sta MMU_IO_CTRL
		lda #<CLUT0							; Set source pointer to CLUT for color information
		sta ptr_src
		lda #>CLUT0
		sta ptr_src+1

		lda #<VKY_GR_CLUT_0					; Set destination pointer to Graphics CLUT 0
		sta ptr_dst
		lda #>VKY_GR_CLUT_0
		sta ptr_dst+1

		ldx #$00							; Y is the number of colors to copy, check for 154
		ldy #154
; ************************************************************************************************************************************
makeClut:
		sty totalColors
color_loop:
		ldy #$00							; Y points to the color component (Blue Red Green Alpha)
comp_loop:
		lda (ptr_src),y						; Read byte from our color table 
		sta (ptr_dst),y						; write byte to the Graphic CLUT
		iny
		cpy #$04							; Do 4 bytes for one color + Alpha
		bne comp_loop

		inx
		cpx totalColors						; Loop for all colors of the CLUT
		beq done_lut

		clc									; Move the source pointer to the next Color
		lda ptr_src
		adc #$04
		sta ptr_src
		lda ptr_src+1
		adc #$00
		sta ptr_src+1

		clc									; Move the destination pointer to the next Color
		lda ptr_dst
		adc #$04
		sta ptr_dst
		lda ptr_dst+1
		adc #$00
		sta ptr_dst+1

		jmp color_loop						; and start copying the next color
done_lut:
		stz MMU_IO_CTRL
; ************************************************************************************************************************************
setFont:									; let's change the font
		lda #<font
		sta $80
		lda #>font
		sta $81
		lda #$c1
		stz $82
		sta $83
		ldy #$00
		ldx #$03
		lda #$01
		sta MMU_IO_CTRL
_sfLoop:
		lda ($80),y
		sta ($82),y 
		iny
		bne _sfLoop
		inc $81
		inc $83
		dex
		bne _sfLoop
		stz MMU_IO_CTRL

; set timer for SOF

		lda #kernel.args.timer.FRAMES		; set the Timer to Frames
		ora #kernel.args.timer.QUERY		; and query what frame we're on
		sta kernel.args.timer.units			; store in units parameter
		jsr kernel.Clock.SetTimer			; jsr to Kernel Routine
		bcs skipSet							; If Carry set, ignore
		adc #$01							; if not add 1 to Accumulator for next frame
		sta $d0
skipSet:
		jsr SetTimer						; Let's get the kernel set up for the timer

		jsr setMidiInstrument

; ************************************************************************************************************************************		
; demo starts doing stuff here

		lda #$02
		sta MMU_IO_CTRL
		ldx #$00
eTextLoop:
		lda exitText,x 
		sta $c488,x 
		inx
		cpx #40
		bcc eTextLoop

loop:
		
		jsr handle_events					; This is my game loop
		jmp loop

handle_events:
		lda kernel.args.events.pending		; Peek at the queue to see if anything is pending
		bpl done_handle_events				; Nothing to do
		jsr kernel.NextEvent				; Get the next event.
		bcs done_handle_events				; If Carry is set, skip the handler
		jsr dispatch						; Handle the event
		jmp handle_events					; go and check for another event
done_handle_events:
		rts 								

dispatch:
		lda event.type						; get the event type from Kernel
		cmp #kernel.event.timer.EXPIRED		; is the event timer.EXPIRED?
		beq UpdateScreen					; run the screen update
		cmp #kernel.event.key.PRESSED		                     
        beq keypress	
		rts

keypress:
		lda event.key.flags               	; Once a key is pressed, we can get the ascii value by loading the byte from the
        lda event.key.ascii                 ; event.key.ascii location assigned by the kernel. We then check to see if it's a
		cmp #135							; f7
		beq endprog
		cmp #129							; f1
		beq changeDisplay
		rts
endprog:
		lda #%00000001						; Graphic, Sprites Engine enabled  			|xx|GM|SP|TL|BM|GR|OV|TX|
		sta VKY_MSTR_CTRL_0					; Text overlay enabled						| 0| 0| 0| 1| 0| 1| 1| 1|

		lda #%00000000						; Text mode options for the overlay 		|xx|xx|FS|FO|MS|2Y|2X|70|
		sta VKY_MSTR_CTRL_1					; 320 x 240, 60 Hz, dbl X & Y				| 0| 0| 0| 0| 0| 1| 1| 0|
		stz VKY_BRDR_CTRL					; No Border
		jmp $e020							; reset f256

changeDisplay:
		lda disp
		beq lines
		cmp #$01
		beq diags
squares:	
		stz disp
		jmp setTileMap1
		
lines:
		inc disp
		jmp setTileMap2

diags:
		inc disp
		jmp setTileMap3
		

UpdateScreen:
		jsr SetTimer						; set timer for next TOF
		jsr dropColor
		inc playCounter						; inc the play counter - notes are not every frame
		lda playSong
		bne silence
		lda playCounter
		cmp #$0a							; have we hit 4 cycles?
		beq playNotes						; yes, let's play the next notes
        rts

silence:
		dec playSong
		lda playSong
		beq startSong
		rts
startSong:									; setup for a new song
		lda #<RandomChart
		sta chartLO
		lda #>RandomChart
		sta chartHI
		stz playCounter
		stz bc
		;stz phrase
		jsr NewMeasure
		rts
		
		

playNotes:
		stz playCounter
		ldy barpos							; get the current position in the measure (cariable based on complexity)
loopNotes:
		lda (bar),y							; get the current note information
		beq endNotes						; if 0, no note, end of 1/16 beat

		jsr makeMusic						; strike the note
		;jsr showWork	;temp display
		iny									; set to the next psotion
		sty barpos							; and save
		bra loopNotes

endNotes:
		inc bcount							; incrmemnt what beat we're on
		lda bcount 							; load and check if we've played 6 1/16 notes of the measure
		cmp #$06		
		bcs NewMeasure						; if yes, time for a new measure
		iny									; if not, increment position in the measure
		sty barpos							; and store
		rts									; return out of music loop

makeMusic:									; make midi note
			
		ldx #$90							; strike a note on channel 0
		stx MIDI_COM
		;lda (bar),y							; send note value based on what ball hit the edge
		sta MIDI_COM
		sta midiNote
		lda bcount
		beq accent
		ldx #$34 
		bra hitNote
accent:
		ldx #$40							; set strike velocity ($40 is the default value)
hitNote:
		stx MIDI_COM
		lda midiNote
		sec
		sbc #36
		tax
		lda Random_L
		ora #%10000000
		sta boxB,x
		lda Random_L
		ora #%10000000
		sta boxG,x
		lda Random_L
		ora #%10000000
		sta boxR,x
		rts

NewMeasure:									
		lda Random_L						; get a random number for the next measure
		sta MULU_A_L						; and multiply it by 11 to get one of the premade bars of music
		lda #$0b
		sta MULU_B_L
		stz MULU_A_H
		stz MULU_B_H
		lda MULU_LH							; get the LH byte from the multiplier which will be between 0 and A
		asl									; double it because the table is built from byte pairs
		tay									; and transfer to y for an indirect index
		lda (chartLO),y						; Get the bar from the chart and transfer it to the bar memory location
		sta bar
		iny
		lda (chartLO),y
		sta bar+1
		stz barPos							; reset all the bar counter to the start of the measure
		stz bcount
		clc
		lda chartLO							; Move to the next row on the chart table for the next time around
		adc #22								; since there are 11 locations per row and they are doubled, add 22 to drop
		sta chartLO							; to the next row.
		lda chartHI
		adc #$00
		sta chartHI
		inc bc 								; there are 16 columns on the table for 16 measures of music
		lda bc								; this counts measures so we know whena phrase is done
		cmp #$11
		beq endSong							; if we hit the end of a phase, do an end song check
		rts
endSong:	
		jsr startSong						; we've hit the end of a phase to reset the counters to the start of the chart
		inc phrase
		lda phrase
		cmp #$03							; 3 phrases make up a "Song" so we'll count to three before initaing silence
		bcc NotDone
		lda #$ff							; if we're done, we'll stop playing music for 99 sof cycles
		sta playSong
		stz phrase
NotDone:
		rts


dropColor:
		ldx #$00
colorLoop:
		dec boxG,X							; each cycle reduce the color value by one
		dec boxR,x 
		dec boxB,x 
		lda boxG,X							; check if we've gone too far
		cmp #$ff							; and reset to zero
		bne checkRed 
		lda #$00
		sta boxG,x 
checkRed:
		lda boxR,x 
		cmp #$ff
		bne checkBlue 
		lda #$00
		sta boxR,x 
checkBlue:
		lda boxB,x 
		cmp #$ff
		bne colorDone
		lda #$00
		sta boxB,x 
colorDone:
		lda #$01
		sta MMU_IO_CTRL						; set IO to 1

		txa									
		clc				
		adc #$08							; and add eight for the first color to first ball
		asl									; then multiply by 4 to get to the right
		asl									; location for the RGBA bytes for each color
		tay									; and transfer to Y for indirect indexing
		lda #<VKY_GR_CLUT_0					; Set destination pointer to Graphics CLUT 0
		sta ptr_dst
		lda #>VKY_GR_CLUT_0
		sta ptr_dst+1

		lda boxB,x 							; get the box color
		sta (ptr_dst),y						; and store it in the CLUT
		iny									; incrment y for next color component
		lda boxR,x 							; and repeat
		sta (ptr_dst),y 
		iny
		lda boxG,x 
		sta (ptr_dst),y 
		stz MMU_IO_CTRL						; reset mmu IO to zero		

		inx
		cpx #48
		bcc colorLoop
		rts

; ************************************************************************************************************************************
setTileMap1:
;Set TileSet 0 for our background
		lda #<tileset1
		sta VKY_TS0_AD_L
		lda #>tileset1
		sta VKY_TS0_AD_M
		lda #$01							
		sta VKY_TS0_AD_H

;Set Tile Map 0
		lda #%00000001						; 16 x 16 tiles, enable					   |xx|xx|xx|TS|xx|xx|xx|EN|
		sta VKY_TM0_CTRL					;										   | 0| 0| 0| 0| 0| 0| 0| 1|
		lda #20								; Tile Map Size 20 X 
		sta VKY_TM0_SZ_X
		lda #15								; Tile Map Size 15 Y
		sta VKY_TM0_SZ_Y

		lda #<tilemap1						; Point to the Tile Map Data, LOW BYTE
		sta VKY_TM0_AD_L
		lda #>tilemap1						; Point to the Tile Map Data, MEDIUM BYTE
		sta VKY_TM0_AD_M
		lda #$01							; Point to the Tile Map Data, HIGH BYTE
		sta VKY_TM0_AD_H
		rts
; ************************************************************************************************************************************
setTileMap2:
;Set TileSet 0 for our background
		lda #<tileset2
		sta VKY_TS0_AD_L
		lda #>tileset2
		sta VKY_TS0_AD_M
		lda #$02						
		sta VKY_TS0_AD_H

;Set Tile Map 0
		lda #%00000001						; 16 x 16 tiles, enable					   |xx|xx|xx|TS|xx|xx|xx|EN|
		sta VKY_TM0_CTRL					;										   | 0| 0| 0| 0| 0| 0| 0| 1|
		lda #20								; Tile Map Size 20 X 
		sta VKY_TM0_SZ_X
		lda #15								; Tile Map Size 15 Y
		sta VKY_TM0_SZ_Y

		lda #<tilemap2						; Point to the Tile Map Data, LOW BYTE
		sta VKY_TM0_AD_L
		lda #>tilemap2						; Point to the Tile Map Data, MEDIUM BYTE
		sta VKY_TM0_AD_M
		lda #$02							; Point to the Tile Map Data, HIGH BYTE
		sta VKY_TM0_AD_H
		rts
; ************************************************************************************************************************************

setTileMap3:
;Set TileSet 0 for our background
		lda #<tileset3
		sta VKY_TS0_AD_L
		lda #>tileset3
		sta VKY_TS0_AD_M
		lda #$03						
		sta VKY_TS0_AD_H

;Set Tile Map 0
		lda #%00000001						; 16 x 16 tiles, enable					   |xx|xx|xx|TS|xx|xx|xx|EN|
		sta VKY_TM0_CTRL					;										   | 0| 0| 0| 0| 0| 0| 0| 1|
		lda #20								; Tile Map Size 20 X 
		sta VKY_TM0_SZ_X
		lda #15								; Tile Map Size 15 Y
		sta VKY_TM0_SZ_Y

		lda #<tilemap3						; Point to the Tile Map Data, LOW BYTE
		sta VKY_TM0_AD_L
		lda #>tilemap3						; Point to the Tile Map Data, MEDIUM BYTE
		sta VKY_TM0_AD_M
		lda #$03							; Point to the Tile Map Data, HIGH BYTE
		sta VKY_TM0_AD_H
		rts
; ************************************************************************************************************************************

; set midi device
setMidiInstrument:
		lda #$c0							; set the instrument for channel 0
		sta MIDI_COM
		ldx midiInst								; this is the instrument number
		lda midiTable,x
		sta MIDI_COM
		rts

; ************************************************************************************************************************************
SetTimer:	
		inc $d0
		lda $d0
		sta kernel.args.timer.absolute		; store in timer.absolute paramter
		sta kernel.args.timer.cookie		; saved as a cookie to the kernel (same as frame number)
		lda #kernel.args.timer.FRAMES		; set the Timer to Frames
		sta kernel.args.timer.units			; store in units parameter
		jsr kernel.Clock.SetTimer			; jsr to Kernel routine to set timer
		rts

; ************************************************************************************************************************************
clrScreen:
		ldx #$00							; set x for indexing
csLoop:
		lda #$02							; set the output to character matrix
		sta MMU_IO_CTRL
		lda #$20							; set a to a blank character
		sta $c000+$000,x 					; and save every 240 memory locations
		sta $c000+$0f0,x 					;
		sta $c000+$1e0,x 					; We're only going to loop once instead of
		sta $c000+$2d0,x 					; nesting loops
		sta $c000+$3c0,x 
		lda #$03							; set the output to the color matrix
		sta MMU_IO_CTRL
		lda #$f0							; pick white
		sta $c000+$000,x 					; do the same save groups
		sta $c000+$0f0,x 
		sta $c000+$1e0,x 
		sta $c000+$2d0,x 
		sta $c000+$3c0,x 
		inx									; inc x
		cpx #$f1 							; and check if we've hit 241
		bcc csLoop							; if less, continue looping

		stz MMU_IO_CTRL						; reset IO to 0
		rts

; ************************************************************************************************************************************
; Working Memory

totalColors:    .byte $00
midiInst:		.byte 6
playSong:		.byte $10
bc:				.byte $00
disp:			.byte $00
;					  "0123456789012345678901234567890123456789"	
exitText:		.text "     F1 - Display      F7 - Quit        "
				.byte $00,$00,$00,$00,$00,$00
boxR:			.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00
boxG:			.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00
boxB:			.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00

midiTable:	.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,24,25,26,27,28,32,33,34,35,36,37,38,39,45,46,47     ;31
			.byte 88,96,98,99,100,103,104,105,106,107,108,112,113,114,115,116,117,118,123,127				;51

; ************************************************************************************************************************************	

musicCharts:
.include "music_charts.s"
font:
.binary "atari.bin"
instruments:
.include "midi_instruments.s"
CLUT0:
.include "clut.s"

*=$10000
tileset1:
.include "Musik_tileset.s"
tilemap1:
.binary "Musik_tilemap.tlm"

*=$20000
tileset2:
.include "Musik2_tileset.s"
tilemap2:
.binary "Musik2_tilemap.tlm"

*=$30000
tileset3:
.include "Musik3_tileset.s"
tilemap3:
.binary "Musik3_tilemap.tlm"