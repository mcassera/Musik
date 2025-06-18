; Master Memory Unit
MMU_IO_CTRL 	= $01						;MMU I/O Control

; Vicky control
VKY_MSTR_CTRL_0 = $d000						; Vicky Master Control Register 0
VKY_MSTR_CTRL_1 = $d001						; Vicky Master Control Register 1
VKY_BRDR_CTRL   = $d004						; Vicky Border Control Register

; Vicky Background Color
VKY_BKG_COL_B   = $d00d						; Vicky Graphics Background Color Blue
VKY_BKG_COL_G   = $d00e						; Vicky Graphics Background Color Green
VKY_BKG_COL_R   = $d00f						; Vicky Graphics Background Color Red

; Vicky layer control
VKY_LAYER_CTRL_0= $d002
VKY_LAYER_CTRL_1= $d003

; Tile Set 0 Registers
VKY_TS0_AD_L	= $d280						; Vicky Tile 0 Image Start Address LOW BYTE
VKY_TS0_AD_M	= $d281						; Vicky Tile 0 Image Start Address MEDIUM BYTE
VKY_TS0_AD_H	= $d282						; Vicky Tile 0 Image Start Address HIGH BYTE

; Tile Set 1 Registers
VKY_TS1_AD_L	= $d284						; Vicky Tile 1 Image Start Address LOW BYTE
VKY_TS1_AD_M	= $d285						; Vicky Tile 1 Image Start Address MEDIUM BYTE
VKY_TS1_AD_H	= $d286						; Vicky Tile 1 Image Start Address HIGH BYTE

; Tile Set 2 Registers
VKY_TS2_AD_L	= $d288						; Vicky Tile 2 Image Start Address LOW BYTE
VKY_TS2_AD_M	= $d289						; Vicky Tile 2 Image Start Address MEDIUM BYTE
VKY_TS2_AD_H	= $d28a						; Vicky Tile 2 Image Start Address HIGH BYTE


; Tile Map 0 Registers
VKY_TM0_CTRL	= $d200						; Tile Map 0 Control
VKY_TM0_AD_L	= $d201						; Tile Map 0 Start Address LOW BYTE
VKY_TM0_AD_M	= $d202						; Tile Map 0 Start Address MEDIUM BYTE
VKY_TM0_AD_H	= $d203						; Tile Map 0 Start Address HIGH BYTE
VKY_TM0_SZ_X	= $d204						; Tile Map 0 Size X
VKY_TM0_SZ_Y	= $d206						; Tile Map 0 Size Y
VKY_TM0_POS_X_L = $d208						; Tile Map 0 X Position & Scroll LOW BYTE
VKY_TM0_POS_X_H = $d209						; Tile Map 0 X Position & Scroll HIGH BYTE
VKY_TM0_POS_Y_L = $d20a						; Tile Map 0 Y Position & Scroll LOW BYTE
VKY_TM0_POS_Y_H = $d20b						; Tile Map 0 Y Position & Scroll HIGH BYTE

; Tile Map 1 Registers
VKY_TM1_CTRL	= $d20c						; Tile Map 1 Control
VKY_TM1_AD_L	= $d20d						; Tile Map 1 Start Address LOW BYTE
VKY_TM1_AD_M	= $d20e						; Tile Map 1 Start Address MEDIUM BYTE
VKY_TM1_AD_H	= $d20f						; Tile Map 1 Start Address HIGH BYTE
VKY_TM1_SZ_X	= $d210						; Tile Map 1 Size X
VKY_TM1_SZ_Y	= $d212						; Tile Map 1 Size Y
VKY_TM1_POS_X_L = $d214						; Tile Map 1 X Position & Scroll LOW BYTE
VKY_TM1_POS_X_H = $d215						; Tile Map 1 X Position & Scroll HIGH BYTE
VKY_TM1_POS_Y_L = $d216						; Tile Map 1 Y Position & Scroll LOW BYTE
VKY_TM1_POS_Y_H = $d217						; Tile Map 1 Y Position & Scroll HIGH BYTE

; Tile Map 2 Registers
VKY_TM2_CTRL	= $d218						; Tile Map 2 Control
VKY_TM2_AD_L	= $d219						; Tile Map 2 Start Address LOW BYTE
VKY_TM2_AD_M	= $d21a						; Tile Map 2 Start Address MEDIUM BYTE
VKY_TM2_AD_H	= $d21b						; Tile Map 2 Start Address HIGH BYTE
VKY_TM2_SZ_X	= $d21c						; Tile Map 2 Size X
VKY_TM2_SZ_Y	= $d21e						; Tile Map 2 Size Y
VKY_TM2_POS_X_L = $d220						; Tile Map 2 X Position & Scroll LOW BYTE
VKY_TM2_POS_X_H = $d221						; Tile Map 2 X Position & Scroll HIGH BYTE
VKY_TM2_POS_Y_L = $d222						; Tile Map 2 Y Position & Scroll LOW BYTE
VKY_TM2_POS_Y_H = $d223						; Tile Map 2 Y Position & Scroll HIGH BYTE

; Sprite registers		                    ; we're starting a $0a for cars in case we want something in front of them (explosions or ???)

VKY_SP0         = $d900                     ; start of sprite register locations / each new aprite is a multiple of 8
SP_CTRL         = $00                       ; control register              7-x, 6/5-size, 4/3-layer, 2/1-lut, 0-enable
SP_AD_L         = $01                       ; image address location
SP_AD_M         = $02
SP_AD_H         = $03
SP_POS_X_L      = $04                       ; x position
SP_POS_X_H      = $05 
SP_POS_Y_L      = $06                       ; y position
SP_POS_Y_H      = $07

playerSP        = $d9a0                     ; the sprite registers for the player

; Vicky Color Look Up Table Regsiters
VKY_GR_CLUT_0  	= $d000						; Graphics LUT #0 in I/O page 1
VKY_GR_CLUT_1  	= $d400						; Graphics LUT #1 in I/O page 1

; SID Registers
SID_L1_FREQ_L   = $d400                     ; Left Sid Registers
SID_L1_FREQ_H   = $d401
SID_L1_PULS_L   = $d402
SID_L1_PULS_H   = $d403
SID_L1_GATE     = $d404
SID_L1_ATDL     = $d405
SID_L1_STRL     = $d406

SID_L2_FREQ_L   = $d407
SID_L2_FREQ_H   = $d408
SID_L2_PULS_L   = $d409
SID_L2_PULS_H   = $d40a
SID_L2_GATE     = $d40b
SID_L2_ATDL     = $d40c
SID_L2_STRL     = $d40d

SID_L3_FREQ_L   = $d40e
SID_L3_FREQ_H   = $d40f
SID_L3_PULS_L   = $d410
SID_L3_PULS_H   = $d411
SID_L3_GATE     = $d412
SID_L3_ATDL     = $d413
SID_L3_STRL     = $d414

SID_L_FLT_L     = $d415
SID_L_FLT_H     = $d416
SID_L_RES       = $d417
SID_L_VOL       = $d418

SID_R1_FREQ_L   = $d500                     ; Right Sid Registers
SID_R1_FREQ_H   = $d501
SID_R1_PULS_L   = $d502
SID_R1_PULS_H   = $d503
SID_R1_GATE     = $d504
SID_R1_ATDL     = $d505
SID_R1_STRL     = $d506

SID_R2_FREQ_L   = $d507
SID_R2_FREQ_H   = $d508
SID_R2_PULS_L   = $d509
SID_R2_PULS_H   = $d50a
SID_R2_GATE     = $d50b
SID_R2_ATDL     = $d50c
SID_R2_STRL     = $d50d

SID_R3_FREQ_L   = $d50e
SID_R3_FREQ_H   = $d50f
SID_R3_PULS_L   = $d510
SID_R3_PULS_H   = $d511
SID_R3_GATE     = $d512
SID_R3_ATDL     = $d513
SID_R3_STRL     = $d514

SID_R_FLT_L     = $d515
SID_R_FLT_H     = $d516
SID_R_RES       = $d517
SID_R_VOL       = $d518

;PSG Registers
PSG_L           = $d600
PSG_R           = $d610
PSG_LR          = $d608

PSG1_FREQ_LO    = %10000000                 ; OR with low 4 bits
PSG1_FREQ_HI    = %00000000                 ; OR with low 6 bits
PSG1_VOLUME     = %10010000                 ; OR with low 4 bits / default is full  - 0 = full, f = silent

PSG2_FREQ_LO    = %10100000                 ; OR with low 4 bits
PSG2_FREQ_HI    = %00000000                 ; OR with low 6 bits
PSG2_VOLUME     = %10110000                 ; OR with low 4 bits / default is full  - 0 = full, f = silent

PSG3_FREQ_LO    = %11000000                 ; OR with low 4 bits
PSG3_FREQ_HI    = %00000000                 ; OR with low 6 bits
PSG3_VOLUME     = %11010000                 ; OR with low 4 bits / default is full  - 0 = full, f = silent


;Midi
MIDI_COM        = $dda1                     ; midi command

; Interrupt Registers
VIRQ			= $fffe						; Pointer to IRQ routine (LOW Byte)
INT_PEND_0		= $d660						; Pending register for interrupts 0-7
INT_PEND_1		= $d661						; Pending register for interrupts 8-15
INT_MASK_0		= $d66c						; Mask register for interrupts 0-7
INT_MASK_1		= $d66d						; Mask register for interrupts 8-15

; Math Coprocessor
MULU_A_L		= $de00						; unsigned A LOW byte
MULU_A_H		= $de01						; unsigned A HIGH Byte
MULU_B_L		= $de02						; unsigned B LOW byte
MULU_B_H		= $de03						; unsigned B HIGH byte
MULU_LL			= $de10						; A x B byte 0
MULU_LH			= $de11						; A x B byte 1
MULU_HL			= $de12						; A x B byte 3
MULU_HH			= $de13						; A x B byte 4

ADD_A_LL		= $de08
ADD_A_LH		= $de09
ADD_A_HL		= $de0a
ADD_A_HH		= $de0b
ADD_B_LL		= $de0c
ADD_B_LH		= $de0d
ADD_B_HL		= $de0e
ADD_B_HH		= $de0f
ADD_R_LL		= $de18
ADD_R_LH		= $de19
ADD_R_HL		= $de1a
ADD_R_HH		= $de1b

;Random NUmber Generator
Random_Reg		= $d6a6
Random_L		= $d6a4

; Misc Variables for Indirect Indexing
ptr_src			= $80						; A pointer to read data
ptr_dst			= $82						; A pointer to write data



