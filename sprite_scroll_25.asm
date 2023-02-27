; ==========================================================================
; =                                                                        =
; = Three independent scroller using double-sized multiplexed sprites      =
; = Centre sinus scroll (sine on each sprite)                              =
; = Added raster cycling                                                   =
; = Optimised for speed                                                    =
; = Animated chess-board background                                        =
; = Wobbly text on the top and bottom rasters                              = 
; = Updated for text strings >255 characters (centre scrolly only)         =
; = Opened up bottom border!                                               =
; = Added extra sprites to bottom border                                   =
; =                                                                        =
; = Paul Alan Freshney										               =
; =                                                                        =
; -   GITHUB.COM/MAXIMUMOCTOPUS
; =                                                                        =
; = February 26th 2023 (original version October 4th 2016)                 =
; =                                                                        =
; ==========================================================================

				;.cpu 6502

				; === constants =============================================================================================
				
				.include "include\colours.inc"
				
				; ==========================================================================================================

				VICBASE   			= $8000
				SCREENRAM 			= VICBASE   + $2000
				CHARRAM   			= VICBASE   + $3000
				SPRITEPTR 			= SCREENRAM + $03f8
				
				SCROLL1SPEED     	= $03	
				SCROLL2SPEED     	= $02
				SCROLL3SPEED     	= $01
				SCROLL4SPEED     	= $04

				RASTERBARSPEED   	= $06
				
				SCROLL1COLOUR1  	= $0f
				SCROLL1COLOUR2	   	= $0c
				
				SCROLL2COLOUR1  	= $0a
				SCROLL2COLOUR2   	= $02

				SCROLL3COLOUR1   	= $0e
				SCROLL3COLOUR2   	= $06
				
				SCROLL4COLOUR1   	= $0f
				SCROLL4COLOUR2   	= $0c
				
				HORIZBOUNCESPEED 	= $03
				
				MUSICLOAD		 	= $1000
				MUSICINIT		 	= $1000
				MUSICPLAY        	= $1003
				
				ANIMCOUNT			= 0 ; 256 (rollsover from 255 :)
				
				; ===========================================================================================================

				zpSCROLL1YOFFSET	= $02
				zpSCROLL1TXTOFFSET	= $03
				zpSCROLL1SPEED		= $04
				
				zpSCROLL2YOFFSET	= $05
				zpSCROLL2TXTOFFSET	= $06
				zpSCROLL2SPEED		= $07
				
				zpSCROLL3YOFFSET	= $08
				zpSCROLL3SPEED		= $09

				zpRASTERBARTIMER	= $0a
				
				zpANIMDELAY			= $0b
				zpANIMCHAROFFSET	= $0c
				
				zpSCROLL4YOFFSET	= $0d
				zpSCROLL4TXTOFFSET	= $0e
				zpSCROLL4SPEED		= $0f

				zpSCROLL4COL1		= $10
				zpSCROLL4COL2		= $11
				
				zpSCROLL4SINTIMER	= $12
				
				zpRASTERTEXTSTATUS	= $13 ; fade status ($00 new chars, $ff wait)
				zpRASTERTEXTOFFSET	= $14
				zpRASTERSCREENOFFSET= $15
				zpRASTERTEXTTIMERC	= $16
				zpRASTERTEXTTIMERT	= $17
				
				zpHORIZSCROLLOFFSET	= $18
				zpHORIZSCROLLTIMER	= $19
				
				zpSPRITE1SINOFFSET	= $1a
				zpSPRITE2SINOFFSET	= $1b
				zpSPRITE3SINOFFSET	= $1c
				zpSPRITE4SINOFFSET	= $1d
				zpSPRITE5SINOFFSET	= $1e
				zpSPRITE6SINOFFSET	= $1f
				zpSPRITE7SINOFFSET	= $20
				zpSPRITE8SINOFFSET	= $21
				
				zpCOLOURBARS		= $22
				
				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================

				*=$0801
				
				.word (+), 2005  		;pointer, line number
				.null $9e, format("%d", start)
+				.word 0          		;basic line end

				; ===========================================================================================================

				*=$4000

start			sei           			;disable interrupts
		 
				lda #$00
				tax
				tay
				jsr MUSICINIT			; initialise music
				
				; -----------------------------------------------------------------------------------------------------------
				
				ldx #$77				; lowest y value
				ldy #$00
sinustable		txa
				clc
				adc	offsets,y			; add offset to base value
				
				sta spriteysinus1,y
				
				iny

				cpy #30
				bne sinustable
				
				; -----------------------------------------------------------------------------------------------------------
				
initscroll3		lda #>scrollytext3+7	; update the address of the next sprite image
				sta sprite7xx+2			; in code
				lda #<scrollytext3+7	;
				sta sprite7xx+1
				
				; -----------------------------------------------------------------------------------------------------------

				lda #cBLACK
				sta $d020      			; make border black.
				sta $d021      			; make background colour 0 black.
		
				lda #$8c				; character data at $3000, screen at $2000 (+ base VIC address)
				sta $d018				; 
				
				lda #$1b
				sta $d011
			
				lda $dd00
				and #%11111100
				ora #%00000001 			; using bank2
				sta $dd00
		
				lda #58
				sta $11
				lda #59
				sta $12
				lda #60
				sta $13
				lda #61
				sta $14
				
				; ===========================================================================================================
		
				ldx #$00
cls				lda $11					; clears the screen with alternate pattern per line (abab..., baba..., etc.)
				sta $a000,x				; top 0
				sta $a050,x				; top 2
				sta $a0a0,x				; top 4
				sta $a0f0,x				; top 6
				sta $a140,x				; middle 0
				sta $a190,x				; middle 2
				sta $a1e0,x				; middle 4
				sta $a230,x				; middle 6
				sta $a280,x				; middle 8
				sta $a2d0,x				; bottom 0
				sta $a320,x				; bottom 2
				sta $a370,x				; bottom 4
				sta $a3c0,x				; bottom 6
				
				lda $13
				sta $a028,x				; top 1
				sta $a078,x				; top 3
				sta $a0c8,x				; top 5
				sta $a168,x				; middle 1
				sta $a1b8,x				; middle 3
				sta $a208,x				; middle 5
				sta $a258,x				; middle 7
				sta $a2f8,x				; bottom 1
				sta $a348,x				; bottom 3
				sta $a398,x				; bottom 5
				sta $a3e8,x				; bottom 7
				
				
				lda #32					; space (blank)
				sta SCREENRAM+$118,x	; place it in to screen RAM
				sta SCREENRAM+$2a8,x	; place it in to screen RAM
				
				inx
				
				ldy $12					; swap char codes, gives tiling effect
				
				lda $11					;
				sta $12					;
				
				sty $11					;
				
				ldy $14					; swap char codes, gives tiling effect
				
				lda $13					;
				sta $14					;
				
				sty $13					;				
				
				cpx #40					; 40 chars per line
				bne cls
				
				; ===========================================================================================================
				
				ldx #$00
				lda #$0b				; dark grey
cram			sta $d800,x				; whole screen
				sta $d900,x				;
				sta $da00,x				;
				sta $db00,x				;
				
				inx
				bne cram
				
				ldx #$00
				lda #cWHITE				; white
cram2			sta $d918,x				; top and bottow wobbly text (over rasters)
				sta $daa8,x				;
				
				inx
				cpx #40					; 40 chars per line
				bne cram2
				
				; ===========================================================================================================
			
				lda #$00
				sta zpSCROLL1YOFFSET	; scroll #1 y-coord index
				sta zpSCROLL1TXTOFFSET	; index to scroll text
				lda #SCROLL1SPEED				
				sta zpSCROLL1SPEED		; scroll speed
				
				lda #$00
				sta zpSCROLL2YOFFSET	; scroll #2 y-coord index
				sta zpSCROLL2TXTOFFSET	; index to scroll text
				lda #SCROLL2SPEED				
				sta $07					; scroll speed		

				lda #$00
				sta zpSCROLL3YOFFSET	; scroll #3 y-coord index
				lda #SCROLL3SPEED				
				sta zpSCROLL3SPEED		; scroll speed	
				
				lda #$00
				sta zpSCROLL4YOFFSET	; scroll #4 y-coord index
				sta zpSCROLL4TXTOFFSET	; which sprite shape is showing, from lookup table
				lda #SCROLL4SPEED				
				sta zpSCROLL4SPEED		; scroll speed
				
				lda #$1d
sinvals			sta zpSPRITE1SINOFFSET	; sine table offset
				lda #$19
				sta zpSPRITE2SINOFFSET	; sine table offset
				lda #$15
				sta zpSPRITE3SINOFFSET	; sine table offset
				lda #$11
				sta zpSPRITE4SINOFFSET	; sine table offset
				lda #$0d
				sta zpSPRITE5SINOFFSET	; sine table offset
				lda #$09
				sta zpSPRITE6SINOFFSET	; sine table offset
				lda #$05
				sta zpSPRITE7SINOFFSET	; sine table offset
				lda #$01
				sta zpSPRITE8SINOFFSET	; sine table offset
				
				lda #04
				sta zpSCROLL4SINTIMER	; y-coord sinus timer
				
				lda #RASTERBARSPEED
				sta zpRASTERBARTIMER	; raster colour cycle timer
				
				lda #$02				; anim delay
				sta zpANIMDELAY
				
				lda #$00				; character data offset
				sta zpANIMCHAROFFSET
				lda #$06
				sta	zpCOLOURBARS
				
				lda scrollytext4c1
				sta	zpSCROLL4COL1
				lda scrollytext4c2
				sta	zpSCROLL4COL2
				
				lda	#$ff
				sta	zpRASTERTEXTSTATUS
				lda	#$00
				sta	zpRASTERTEXTOFFSET	; offset to character data
				sta	zpRASTERSCREENOFFSET
				lda	#$04
				sta	zpRASTERTEXTTIMERC	; time for next character
				lda #$20
				sta	zpRASTERTEXTTIMERT	; time before starting next string
							
				; ===========================================================================================================
			
sprite_init		lda	#$ff				; 
				sta	$d015				; turn on all sprites
				sta	$d01c				; turn all sprites to hi colour
				sta $d017 				; stretch all vertically
				sta $d01d 				; stretch all horizontally
			
				lda #$00
				ldx #$00
				ldy #cWHITE				; shared sprite colour
shapeloop		lda scrollytext,x
				sta	SPRITEPTR,x			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				sty $d027				; sprite colour
			
				inc	shapeloop+7			; modifies the address above!!
			
				inx
				txa
			
				cpx	#8
				bne	shapeloop
				
				; ===========================================================================================================

				lda #$00
				ldx #$00
				ldy #$00

				lda #$00
				sta zpHORIZSCROLLOFFSET
				lda #$04
				sta zpHORIZSCROLLTIMER
				
				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================
				
main	    	jsr MUSICPLAY			; play music (should run every 1/50th of a second)
				
				ldy #$20				; raster line 32
				cpy	$d012
				bne *-3
				
				lda	#$ff				; scroll 4 alters these, so need to set them for scrollers 1-3
				sta $d017 				; stretch all vertically
				sta $d01d 				; stretch all horizontally
				
				lda #SCROLL1COLOUR1
				sta $d025				; sprite colour 1
				lda #SCROLL1COLOUR2				
				sta $d026				; sprite colour 2
				
				; ===========================================================================================================
			
				ldx	zpSCROLL1TXTOFFSET	; load offset to scroll
scrolltext		lda	scrollytext,x
				sta	SPRITEPTR
				inx
				lda	scrollytext,x
				sta	SPRITEPTR+1
				inx
				lda	scrollytext,x
				sta	SPRITEPTR+2
				inx
				lda	scrollytext,x
				sta	SPRITEPTR+3
				inx
				lda	scrollytext,x
				sta	SPRITEPTR+4
				inx
				lda	scrollytext,x
				sta	SPRITEPTR+5
				inx
				lda	scrollytext,x
				sta	SPRITEPTR+6
				inx
				lda	scrollytext,x
				sta	SPRITEPTR+7
				
				lda #$38
				sta $d001				; y-coords
				sta $d003
				sta $d005
				sta $d007
				sta $d009
				sta $d00b
				sta $d00d
				sta $d00f
				
				ldy zpSCROLL1YOFFSET	; offset to x-coord table
				
movesprites		lda spritemove0,y
				sta	$d000
				lda spritemove1,y
				sta	$d002
				lda spritemove2,y
				sta	$d004
				lda spritemove3,y
				sta	$d006
				lda spritemove4,y
				sta	$d008
				lda spritemove5l,y
				sta	$d00a
				lda spritemove5h,y
				sta $d010
				lda spritemove6,y
				sta	$d00c
				lda spritemove7,y
				sta	$d00e

				dec zpSCROLL1SPEED		; scroll speed
				bne wibblywoo
				
				; ===========================================================================================================
				
				lda #SCROLL1SPEED		; effective scroll speed = x frames per scroll
				sta zpSCROLL1SPEED	
				
				iny						; move to next sprite co-ordinates				
				cpy	#25					
				bne continue
				
				ldy #$00				; go to beginning of y-coords
				
				ldx	zpSCROLL1TXTOFFSET	; text offset index
				inx						; next character
				cpx	#size(scrollytext)  ; size of scrolly text
				bne	keepscrollin
			
				ldx	#$00				; move to beginning of text
			
keepscrollin	stx zpSCROLL1TXTOFFSET
				
continue		sty zpSCROLL1YOFFSET

wibblywoo		;dec $d020

				; ===========================================================================================================
				; ===========================================================================================================

rasterbar1		ldy #$6a    			; start raster line

				cpy $d012   			; wait for raster to hit correct place
				bne *-3
				
				nop						; at the raster above where we want
				nop						; now wait a bit until we're in a good place to change the
				nop						; horiz scroll register
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop						; not there yet
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop						; READY!

horizoffset1	ldx #$c0				; code modifies this
				stx $d016
				
				ldy #$6c    			; start raster line
				ldx #$02      			; offset to colour table
rasterbar1loop	lda rasterbarcols,x   	; 

				cpy $d012   			; wait for correct raster line
				bne *-3	    			; 

				sta $d020				; background colour
				sta $d021      			; border colour

				cpx #8        			; 8 colours
				beq centrescrolly		; 

				inx						; next colour
				iny            			; next raster line

				jmp rasterbar1loop		; jump to loop.		
				
				; ===========================================================================================================
				; ===========================================================================================================

centrescrolly	ldx #$c0				; reset horiz scroll
				stx $d016				;
				
				ldy #$72
				cpy	$d012
				bne *-3
				
				lda #SCROLL3COLOUR1
				sta $d025				; sprite colour 1
				lda #SCROLL3COLOUR2				
				sta $d026				; sprite colour 2
				
				; ===========================================================================================================
				; ===========================================================================================================
				
scrolltext3
scroll3sprite0	lda	#$00				; code modifies this value
				sta	SPRITEPTR
scroll3sprite1	lda	#$00				; code modifies this value
				sta	SPRITEPTR+1
scroll3sprite2	lda	#$00				; code modifies this value
				sta	SPRITEPTR+2
scroll3sprite3	lda	#$00				; code modifies this value
				sta	SPRITEPTR+3
scroll3sprite4	lda	#$00				; code modifies this value
				sta	SPRITEPTR+4
scroll3sprite5	lda	#$00				; code modifies this value
				sta	SPRITEPTR+5
scroll3sprite6	lda	#$00				; code modifies this value
				sta	SPRITEPTR+6
scroll3sprite7	ldx	#$00				; code modifies this value
				stx	SPRITEPTR+7
				
				cpx #$00				; have we reached the end?
				bne sprite3y			; no
				
				lda #>scrollytext3+7	; update the address of the next sprite image in code
				sta sprite7xx+2			; high byte
				lda #<scrollytext3+7	;
				sta sprite7xx+1			; low byte
				
sprite3y		ldx zpSPRITE1SINOFFSET	; load y-coord from sinus table
				lda spriteysinus1,x		;
				sta $d001
				ldx zpSPRITE2SINOFFSET	; load y-coord from sinus table
				lda spriteysinus1,x		;
				sta $d003
				ldx zpSPRITE3SINOFFSET	; load y-coord from sinus table
				lda spriteysinus1,x		;
				sta $d005
				ldx zpSPRITE4SINOFFSET	; load y-coord from sinus table
				lda spriteysinus1,x		;
				sta $d007
				ldx zpSPRITE5SINOFFSET	; load y-coord from sinus table
				lda spriteysinus1,x		;
				sta $d009
				ldx zpSPRITE6SINOFFSET	; load y-coord from sinus table
				lda spriteysinus1,x		;
				sta $d00b
				ldx zpSPRITE7SINOFFSET	; load y-coord from sinus table
				lda spriteysinus1,x		;
				sta $d00d
				ldx zpSPRITE8SINOFFSET	; load y-coord from sinus table
				lda spriteysinus1,x		;
				sta $d00f
				
				dec zpSCROLL4SINTIMER	; sinus scroll timer
				bne	continuecentre
				
				lda #$02				; reset the timer
				sta zpSCROLL4SINTIMER	; 
				
				ldx zpSPRITE1SINOFFSET
				dex						; move to next y-coord
				bpl nextsinus1
				
				ldx #$1d				; yes, back to zero
				
nextsinus1		stx zpSPRITE1SINOFFSET	; no

				ldx zpSPRITE2SINOFFSET
				dex						; move to next y-coord
				bpl nextsinus2
				
				ldx #$1d				; yes, back to zero
				
nextsinus2		stx zpSPRITE2SINOFFSET	; no

				ldx zpSPRITE3SINOFFSET
				dex						; move to next y-coord
				bpl nextsinus3
				
				ldx #$1d				; yes, back to zero
				
nextsinus3		stx zpSPRITE3SINOFFSET	; no

				ldx zpSPRITE4SINOFFSET
				dex						; move to next y-coord
				bpl nextsinus4
				
				ldx #$1d				; yes, back to zero
				
nextsinus4		stx zpSPRITE4SINOFFSET	; no

				ldx zpSPRITE5SINOFFSET
				dex						; move to next y-coord
				bpl nextsinus5
				
				ldx #$1d				; yes, back to zero
				
nextsinus5		stx zpSPRITE5SINOFFSET	; no

				ldx zpSPRITE6SINOFFSET
				dex						; move to next y-coord
				bpl nextsinus6
				
				ldx #$1d				; yes, back to zero
				
nextsinus6		stx zpSPRITE6SINOFFSET	; no

				ldx zpSPRITE7SINOFFSET
				dex						; move to next y-coord
				bpl nextsinus7
				
				ldx #$1d				; yes, back to zero
				
nextsinus7		stx zpSPRITE7SINOFFSET	; no

				ldx zpSPRITE8SINOFFSET
				dex						; move to next y-coord
				bpl nextsinus8
				
				ldx #$1d				; yes, back to zero
				
nextsinus8		stx zpSPRITE8SINOFFSET	; no
			
continuecentre	ldy zpSCROLL3YOFFSET	; offset to x-coord table
				
movesprites3	lda spritemove0,y
				sta	$d000
				lda spritemove1,y
				sta	$d002
				lda spritemove2,y
				sta	$d004
				lda spritemove3,y
				sta	$d006
				lda spritemove4,y
				sta	$d008
				lda spritemove5l,y
				sta	$d00a
				lda spritemove5h,y
				sta $d010
				lda spritemove6,y
				sta	$d00c
				lda spritemove7,y
				sta	$d00e

				dec zpSCROLL3SPEED		; scroll speed
				bne rasterbar2
				
				; ===========================================================================================================
				
				lda #SCROLL3SPEED		; effective scroll speed = x frames per scroll
				sta zpSCROLL3SPEED		;
				
				iny						; move to next sprite co-ordinate in lookup table			
				cpy	#25					; reached end?
				bne continue3			; no
				
				ldy #$00				; yes, go to beginning of y-coord lookup table
				
				lda scroll3sprite1+1	; copy value for sprite 1 to 0
				sta scroll3sprite0+1	;
				lda scroll3sprite2+1	; copy value for sprite 2 to 1
				sta scroll3sprite1+1	;
				lda scroll3sprite3+1	; copy value for sprite 3 to 2
				sta scroll3sprite2+1	;
				lda scroll3sprite4+1	; copy value for sprite 4 to 3
				sta scroll3sprite3+1	;
				lda scroll3sprite5+1	; copy value for sprite 5 to 4
				sta scroll3sprite4+1	;
				lda scroll3sprite6+1	; copy value for sprite 6 to 5
				sta scroll3sprite5+1	;
				lda scroll3sprite7+1	; copy value for sprite 7 to 6
				sta scroll3sprite6+1	;
				
				lda zpSPRITE2SINOFFSET	; copy sine offset from 2 to 1
				sta	zpSPRITE1SINOFFSET	;
				lda zpSPRITE3SINOFFSET	; copy sine offset from 3 to 2
				sta	zpSPRITE2SINOFFSET	;
				lda zpSPRITE4SINOFFSET	; copy sine offset from 4 to 3
				sta	zpSPRITE3SINOFFSET	;
				lda zpSPRITE5SINOFFSET	; copy sine offset from 5 to 4
				sta	zpSPRITE4SINOFFSET	;
				lda zpSPRITE6SINOFFSET	; copy sine offset from 6 to 5
				sta	zpSPRITE5SINOFFSET	;
				lda zpSPRITE7SINOFFSET	; copy sine offset from 7 to 6
				sta	zpSPRITE6SINOFFSET	;
				lda zpSPRITE8SINOFFSET	; copy sine offset from 8 to 7
				sta	zpSPRITE7SINOFFSET	;
				
				sbc #4					; next coordinate
				bpl	spritecont			; rolled past zero?
				
				clc						; yes
				adc #30					; add 30 (max number of coordinates)
				
spritecont		sta zpSPRITE8SINOFFSET	; store in sprite 8

				inc sprite7xx+1			; move to next position in scroll text
				bne	sprite7xx			; crossed a page boundary?
				inc sprite7xx+2			; yes
				
sprite7xx		lda $ca75				; no, read the next character
				sta scroll3sprite7+1	; place it in sprite 7
				
continue3		sty zpSCROLL3YOFFSET
	
				; ===========================================================================================================
				; ===========================================================================================================

rasterbar2		ldy #$ba    			; start raster line

				cpy $d012   			; wait for raster to hit correct place
				bne *-3
				
				nop						; at the raster above where we want
				nop						; now wait a bit until we're in a good place to change the
				nop						; horiz scroll register
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop						; not there yet
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop						; READY!

horizoffset2	ldx #$c0				; code modifies this
				stx $d016				;
				
				ldy #$bc

				ldx #$07      			; offset to colour table
rasterbar2loop	lda rasterbarcols,x   	;

				cpy $d012   			; wait for raster to hit correct place
				bne *-3	    			; 

				sta $d020				; background colour
				sta $d021      			; border colour

				cpx #1        			; 8 colours
				beq bottomscrolly		; 

				dex						; next colour in colour table
				iny            			; next raster

				jmp rasterbar2loop		;
							
				; ===========================================================================================================
				; ===========================================================================================================
				
bottomscrolly	ldx #$c0				; reset horiz scroll
				stx $d016				;

				ldy #$c5
				cpy	$d012
				bne *-3
				
				lda #SCROLL2COLOUR1
				sta $d025				; sprite colour 1
				lda #SCROLL2COLOUR2				
				sta $d026				; sprite colour 1

				; ===========================================================================================================
				; ===========================================================================================================
				
				ldx	zpSCROLL2TXTOFFSET
scrolltext2		lda	scrollytext2,x
				sta	SPRITEPTR
				inx
				lda	scrollytext2,x
				sta	SPRITEPTR+1
				inx
				lda	scrollytext2,x
				sta	SPRITEPTR+2
				inx
				lda	scrollytext2,x
				sta	SPRITEPTR+3
				inx
				lda	scrollytext2,x
				sta	SPRITEPTR+4
				inx
				lda	scrollytext2,x
				sta	SPRITEPTR+5
				inx
				lda	scrollytext2,x
				sta	SPRITEPTR+6
				inx
				lda	scrollytext2,x
				sta	SPRITEPTR+7
				
				lda #$ca				;
				sta $d001				; y-coords
				sta $d003				;
				sta $d005				;
				sta $d007				;
				sta $d009				;
				sta $d00b				;
				sta $d00d				;
				sta $d00f				;
				
				ldy zpSCROLL2YOFFSET	; offset to x-coord table
movesprites2	lda spritemove0,y
				sta	$d000
				lda spritemove1,y
				sta	$d002
				lda spritemove2,y
				sta	$d004
				lda spritemove3,y
				sta	$d006
				lda spritemove4,y
				sta	$d008
				lda spritemove5l,y
				sta	$d00a
				lda spritemove5h,y
				sta $d010
				lda spritemove6,y
				sta	$d00c
				lda spritemove7,y
				sta	$d00e

				dec $07					; scroll speed
				bne cycletimer
				
				; ===========================================================================================================
				
				lda #SCROLL2SPEED		; effective scroll speed = x frames per scroll
				sta $07	
				
				iny						; move to next sprite co-ordinates				
				cpy	#25					
				bne continue2
				
				ldy #$00				; go to beginning of y-coords
				
				ldx	zpSCROLL2TXTOFFSET	; text offset index
				inx						; next character
				cpx	#size(scrollytext2) ; size of scrolly text, reached end?
				bne	keepscrollin2		; no
			
				ldx	#$00				; yes, move to beginning of text
			
keepscrollin2	stx zpSCROLL2TXTOFFSET
				
continue2		sty zpSCROLL2YOFFSET

cycletimer		dec zpRASTERBARTIMER

				bne backtomain
				
				; ===========================================================================================================
				
				
rastercycle		lda rasterbarcols+3
				sta rasterbarcols+2
				lda rasterbarcols+4
				sta rasterbarcols+3
				lda rasterbarcols+5
				sta rasterbarcols+4
				lda rasterbarcols+6
				sta rasterbarcols+5
				lda rasterbarcols+7
				sta rasterbarcols+6
				
				ldx zpCOLOURBARS
				lda colorsw,x
				sta rasterbarcols+7
				
				inx
				cpx #48
				bne rastercyclecont

				ldx #$00
				
rastercyclecont	stx zpCOLOURBARS
				
				lda #RASTERBARSPEED		; reset the cycle timer
				sta zpRASTERBARTIMER

				; ===========================================================================================================

backtomain		lda $dc00				; load joystick A status
				and #16					; fire pressed?
				bne	contscroll4			; no, continue

				lda #SCROLL1SPEED+1				
				sta zpSCROLL1SPEED		; reset scroll speed, freezes scroll
				
				lda #SCROLL2SPEED				
				sta zpSCROLL2SPEED		; reset scroll speed, freezes scroll	

				lda #SCROLL3SPEED				
				sta zpSCROLL3SPEED		; reset scroll speed, freezes scroll
				
				lda #SCROLL4SPEED				
				sta zpSCROLL4SPEED		; reset scroll speed, freezes scroll
				
				; ===========================================================================================================
				
contscroll4		dec zpSCROLL4SPEED		; scroll #4 speed
				bne jumpgap
				
				; ===========================================================================================================
				
				lda #SCROLL4SPEED		; effective scroll speed = x frames per scroll
				sta zpSCROLL4SPEED	
				
				ldy zpSCROLL4YOFFSET
				
				dey						; move to next sprite co-ordinates				
				bpl continue4
				
				ldy #25					; go to beginning of y-coords
				
				ldx	zpSCROLL4TXTOFFSET	; character offset index
				inx						; next character
				cpx	#7					; maximum number of characters
				bne	keepscrollin4		; no
				
				ldx #$00
				
keepscrollin4	stx zpSCROLL4TXTOFFSET	; update sprite data
				lda scrollytext4c1,x
				sta	zpSCROLL4COL1
				lda scrollytext4c2,x
				sta	zpSCROLL4COL2
				
continue4		sty zpSCROLL4YOFFSET	; update y-coord offset
				
jumpgap			jmp borderz

				; ===========================================================================================================
				
				.align $100
				
borderz			ldx #$e1
				jsr rastersync
				
				lda #$f8
rl1   			cmp $d012
				bne rl1

				lda #$18&$f7			; open the lower border
				sta $d011				;
				
				lda	#$00				; 
				sta $d017 				; no stretching horiz
				sta $d01d 				; no stretching vert
				
				lda zpSCROLL4COL1
				sta $d025				; sprite colour 1
				lda zpSCROLL4COL2
				sta $d026				; sprite colour 2

				ldy zpSCROLL4TXTOFFSET	; offset to sprite shape data
				lda	scrollytext4,y		; all sprites the same
				sta	SPRITEPTR			;
				sta	SPRITEPTR+1			;
				sta	SPRITEPTR+2			;
				sta	SPRITEPTR+3			;
				sta	SPRITEPTR+4			;
				sta	SPRITEPTR+5			;
				sta	SPRITEPTR+6			;
				sta	SPRITEPTR+7			;
				
				ldx zpSCROLL4YOFFSET	; offset to x-coord table				
movesprites4	lda spritemove0,x
				sta	$d000
				lda spritemove1,x
				sta	$d002
				lda spritemove2,x
				sta	$d004
				lda spritemove3,x
				sta	$d006
				lda spritemove4,x
				sta	$d008
				lda spritemove5l,x
				sta	$d00a
				lda spritemove5h,x
				sta $d010
				lda spritemove6,x
				sta	$d00c
				lda spritemove7,x
				sta	$d00e
				
				lda #$ff				;
				sta $d001				; y-coords
				sta $d003				;
				sta $d005				;
				sta $d007				;
				sta $d009				;
				sta $d00b				;
				sta $d00d				;
				sta $d00f				;
				
animateback		ldy #$ff				; wait until raster 255
				cpy	$d012				;
				bne *-3					;
				
				lda #$1b				; reset horiz offset
				sta $d011				;
		
				; ===========================================================================================================
				
				dec zpANIMDELAY			; char anim timer
				
				beq	contanim			; reached zero yet?
				
				jmp hscroller

contanim		lda zpANIMCHAROFFSET	; yes, move to next char
				adc	#$07				;
				
				tax						;
				cpx #ANIMCOUNT			; reached end?
				bne nextchar			;
				
fisrtchar		lda #$00				; yes, back to frame 0

nextchar		sta zpANIMCHAROFFSET	; no
				
				tax
writecram		lda chardata1,x
				sta CHARRAM+(58*8)		; character #0
				lda chardata2,x
				sta CHARRAM+(58*8)+8	; character #1
				lda chardata3,x
				sta CHARRAM+(60*8)		; character #0
				lda chardata4,x
				sta CHARRAM+(60*8)+8	; character #1
				inx
				lda chardata1,x
				sta CHARRAM+(58*8)+1	; character #0
				lda chardata2,x
				sta CHARRAM+(58*8)+9	; character #1
				lda chardata3,x
				sta CHARRAM+(60*8)+1	; character #0
				lda chardata4,x
				sta CHARRAM+(60*8)+9	; character #1
				inx
				lda chardata1,x
				sta CHARRAM+(58*8)+2	; character #0
				lda chardata2,x
				sta CHARRAM+(58*8)+10	; character #1
				lda chardata3,x
				sta CHARRAM+(60*8)+2	; character #0
				lda chardata4,x
				sta CHARRAM+(60*8)+10	; character #1
				inx
				lda chardata1,x
				sta CHARRAM+(58*8)+3	; character #0
				lda chardata2,x
				sta CHARRAM+(58*8)+11	; character #1
				lda chardata3,x
				sta CHARRAM+(60*8)+3	; character #0
				lda chardata4,x
				sta CHARRAM+(60*8)+11	; character #1
				inx
				lda chardata1,x
				sta CHARRAM+(58*8)+4	; character #0
				lda chardata2,x
				sta CHARRAM+(58*8)+12	; character #1
				lda chardata3,x
				sta CHARRAM+(60*8)+4	; character #0
				lda chardata4,x
				sta CHARRAM+(60*8)+12	; character #1
				inx
				lda chardata1,x
				sta CHARRAM+(58*8)+5	; character #0
				lda chardata2,x
				sta CHARRAM+(58*8)+13	; character #1
				lda chardata3,x
				sta CHARRAM+(60*8)+5	; character #0
				lda chardata4,x
				sta CHARRAM+(60*8)+13	; character #1
				inx
				lda chardata1,x
				sta CHARRAM+(58*8)+6	; character #0
				lda chardata2,x
				sta CHARRAM+(58*8)+14	; character #1
				lda chardata3,x
				sta CHARRAM+(60*8)+6	; character #0
				lda chardata4,x
				sta CHARRAM+(60*8)+14	; character #1
				inx
				lda chardata1,x
				sta CHARRAM+(58*8)+7	; character #0
				lda chardata2,x
				sta CHARRAM+(58*8)+15	; character #1
				lda chardata3,x
				sta CHARRAM+(60*8)+7	; character #0
				lda chardata4,x
				sta CHARRAM+(60*8)+15	; character #1
				
				lda #$04				; reset anim timer
				sta zpANIMDELAY
				
				; ===========================================================================================================
				
hscroller		dec zpHORIZSCROLLTIMER	; timer delay
				bne	rasterbartext		; ready for new value?
			
				lda #HORIZBOUNCESPEED	; yes, reset delay
				sta zpHORIZSCROLLTIMER

				ldx zpHORIZSCROLLOFFSET	; load table offset
				inx						; move to next value
				cpx #20					; last one?
				bne savehoriz			; no
				
				ldx #$00				; yes, reset to first item
				
savehoriz		stx zpHORIZSCROLLOFFSET	; save offset to table

				lda horizscroll,x		; update top text horiz scroll value
				sta horizoffset1+1		;
				lda horizscrolli,x		; update bottom text horiz scroll value
				sta horizoffset2+1		;

				; ===========================================================================================================

rasterbartext	ldy zpRASTERTEXTSTATUS	; 
				cpy #$ff				; am i waiting for the message to change
				beq waitmessage			; yes
				
				dec	zpRASTERTEXTTIMERC	; next character delay
				bne	loopy				; 
				
				lda	#$04				; reset char wait delay timer
				sta zpRASTERTEXTTIMERC	;
				
				ldx	zpRASTERTEXTOFFSET	; next character
				ldy	zpRASTERSCREENOFFSET; next place on screen
				
				lda	copyright,x			; read next character (top raster)
				sta	SCREENRAM+$118,y	; write next character
				lda	website,x			; read next character (bottom raster)
				sta	SCREENRAM+$2a8,y	; write next character
				
				inc	zpRASTERTEXTOFFSET	; move to next character
				inc zpRASTERSCREENOFFSET; move to next screen location
				
				cpy	#39					; last location?
				beq	resetstatus			;
				
				iny						; * sweeps text across (loooks cool)
				lda	#$2a				; *
				sta SCREENRAM+$118,y	;
				sta SCREENRAM+$2a8,y	;

				jmp	main				; loop
				
resetstatus		lda	#$ff				; yes
				sta	zpRASTERTEXTSTATUS	; set status to wait for next text string
				
				jmp main				; loop

waitmessage		dec	zpRASTERTEXTTIMERT	; 
				bne	loopy				; have we run down the next text string timer
				
				lda #$50				; yes, reset delay
				sta	zpRASTERTEXTTIMERT	;
				lda #$00				;
				sta	zpRASTERTEXTSTATUS	;
				sta	zpRASTERSCREENOFFSET
				
				ldy	zpRASTERTEXTOFFSET	; current position with text
				cpy	#$f0				; (6 lines) * 40 chars = 240 = f0
				bne	loopy				;
				
				sta zpRASTERTEXTOFFSET	; reset position, first text

				; ===========================================================================================================
				
loopy			jmp	main				; no
		
				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================				
				
				.align $100
rastersync

lp1			    cpx $d012
				bne lp1
				jsr cycles
				bit $ea
				nop
				cpx $d012
				beq skip1
				nop
				nop
skip1	   		jsr cycles
				bit $ea
				nop
				cpx $d012
				beq skip2
				bit $ea
skip2			jsr cycles
				nop
				nop
				nop
				cpx $d012
				bne onecycle

onecycle 		rts

cycles			ldy #$06
lp2				dey
				bne lp2
				inx
				nop
				nop
         
				rts
				
				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================

horizscroll		.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
horizscrolli	.byte $c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1,$c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6
				
spriteysinus1	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				
offsets			.byte $0a,$0c,$0e,$10,$11,$13,$14,$14,$14,$14,$13,$11,$10,$0e,$0c
				.byte $0a,$08,$06,$04,$03,$01,$00,$00,$00,$00,$01,$03,$04,$06,$08
				
				; requires $00 at begin and end
rasterbarcols	.byte $00,$00,$0b,$0c,$0f,$0d,$0d,$0f,$00

colorsw			.byte $0b,$0c,$0f,$0d,$0d,$0f,$0c,$0b 		; grey/green
				.byte $0b,$0c,$0f,$01,$01,$0f,$0c,$0b 		; grey/white
				.byte $02,$04,$0a,$01,$01,$0a,$04,$02 		; red/purple
				.byte $06,$0e,$03,$01,$01,$03,$0e,$06		; blue/white
				.byte $09,$08,$07,$01,$01,$07,$08,$09		; yellow/white
				.byte $0b,$0c,$0f,$0d,$0d,$0f,$0c,$0b		; copy of first set, for nice cycling
				
spritex			.byte $1A,$00,$4C,$00,$7E,$00,$B0,$00,$E2,$00,$14,$20,$46,$40,$78,$80

spritemove0		.byte $1A,$18,$16,$14,$12,$10,$0E,$0C,$0A,$08,$06,$04,$02
				.byte $00,$f6,$f4,$f2,$f0,$ee,$ec,$ea,$e8,$e6,$e4,$e2,$e0
spritemove1		.byte $4C,$4A,$48,$46,$44,$42,$40,$3E,$3C,$3A,$38,$36,$34
				.byte $32,$30,$2E,$2C,$2A,$28,$26,$24,$22,$20,$1E,$1C,$1A
spritemove2		.byte $7E,$7C,$7A,$78,$76,$74,$72,$70,$6E,$6C,$6A,$68,$66
				.byte $64,$62,$60,$5E,$5C,$5A,$58,$56,$54,$52,$50,$4E,$4C
spritemove3		.byte $B0,$AE,$AC,$AA,$A8,$A6,$A4,$A2,$A0,$9E,$9C,$9A,$98
				.byte $96,$94,$92,$90,$8E,$8C,$8A,$88,$86,$84,$82,$80,$7E
spritemove4		.byte $E2,$E0,$DE,$DC,$DA,$D8,$D6,$D4,$D2,$D0,$CE,$CC,$CA
				.byte $C8,$C6,$C4,$C2,$C0,$BE,$BC,$BA,$B8,$B6,$B4,$B2,$B0
spritemove5l	.byte $14,$12,$10,$0E,$0C,$0A,$08,$06,$04,$02,$00,$FE,$FC
				.byte $FA,$F8,$F6,$F4,$F2,$F0,$EE,$EC,$EA,$E8,$E6,$E4,$E2
spritemove5h	.byte $E0,$E0,$E0,$E0,$E0,$E0,$E0,$E0,$E0,$E0,$E0,$C0,$C0
				.byte $C0,$C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1
spritemove6		.byte $46,$44,$42,$40,$3E,$3C,$3A,$38,$36,$34,$32,$30,$2E
				.byte $2C,$2A,$28,$26,$24,$22,$20,$1E,$1C,$1A,$18,$16,$14
spritemove7		.byte $78,$76,$74,$72,$70,$6E,$6C,$6A,$68,$66,$64,$62,$60
				.byte $5E,$5C,$5A,$58,$56,$54,$52,$50,$4E,$4C,$4A,$48,$46
		
				.enc "screen"
scrollytext		.text '        TRIPLE SPRITE SCROLLER VERSION XXV - FEBRUARY 26th 2023 - WITH SOME SPEED OPTIMISATIONS        '
scrollytext2	.text '        % GITHUB.COM/MAXIMUMOCTOPUS % WWW.MAXIMUMOCTOPUS.COM % PAUL!FRESHNEY.ORG % ARTSTATION.COM/MRFRESH3141        '
scrollytext3	.text '        HELLO AND WELCOME TO MY LATEST C64 INTRO (AND THE FIRST TO APPEAR ON YOUTUBE!). 1982 LINES OF PURE ASSEMBLER CODE. THREE LOVELY '
				.text 'MULTIPLEXED SPRITE SCROLLERS (INCLUDING THIS LOVELY SINUS SCROLLER#), SPRITE SCROLLER IN THE BOTTOM BORDER, '
				.text 'ANIMATED BACKGROUND, WOBBLY TEXT, RASTER BARS AND MUSIC. ALL CODE AND GRAPHICS BY PAF, MUSIC BY IRON CAT "ENIGMA PHA THEME". '
				.text 'HELLOS, AND MEOWS, TO THE DEVELOPMENT CATS, RUTHERFORD FREEMAN AND MAXWELL, ADAM, STEVE, STEVE, EVERYONE I KNOW AND COMMODORE FANS EVERWHERE... '
				.text 'IF YOU LIKE MY C64 INTROS, ARE INTERESTED IN CONTRIBUTING OR WORKING TOGETHER ON A C64 PROJECT, OR WOULD JUST LIKE THE SOURCE CODE '
				.text 'THEN PLEASE SEND ME AN EMAIL... '
				.byte $00	; scroller3 needs a terminator
copyright		.text '    PAUL ALAN FRESHNEY  FEB 26TH 2023   '
				.text '    TRIPLE SPRITE SCROLLER VERSION 25   '
				.text '  HELLOS TO... SARAH ADAM STEVE STEVE   '
				.text '         DAN AMY TARTY AND JAY          '
				.text '    THANKS TO THE DEVELOPMENT CATS...   '
				.text '    RUTHERFORD, FREEMAN, AND MAXWELL    '
website			.text '       GITHUB.COM/MAXIMUMOCTOPUS        '
				.text '         WWW.MAXIMUMOCTOPUS.COM         '
				.text '       ARTSTATION.COM/MRFRESH3141       '
				.text '              FRESHNEY.ORG              '
				.text '        PAUL@MAXIMUMOCTOPUS.COM         '
				.text '    SOURCEFORGE.NET/U/MAXIMUMOCTOPUS    '
				.enc "none"
				
				;     PAF,7de,cat,PAF,7de,cat,oct
scrollytext4	.byte $1c,$2b,$1f,$1c,$2b,$25,$3a
scrollytext4c1	.byte $0a,$0e,$0c,$0a,$0e,$0c,$08				; sprite colour 1 for this sprite
scrollytext4c2	.byte $02,$06,$0b,$02,$06,$0b,$08				; sprite colour 2 for this sprite
				
				
				;     char   12
				;            34
				
chardata1		.byte  255, 255, 255, 255, 255, 255, 255, 255 ;1
				.byte  127, 127, 127, 127, 127, 127, 127, 127
				.byte   63,  63,  63,  63,  63,  63,  63,  63
				.byte   31,  31,  31,  31,  31,  31,  31,  31
				.byte   15,  15,  15,  15,  15,  15,  15,  15
				.byte    7,   7,   7,   7,   7,   7,   7,   7
				.byte    3,   3,   3,   3,   3,   3,   3,   3
				.byte    1,   1,   1,   1,   1,   1,   1,   1
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;2
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0 
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;3
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;4
				.byte    0,   0,   0,   0,   0,   0,   0, 255 
				.byte    0,   0,   0,   0,   0,   0, 255, 255 
				.byte    0,   0,   0,   0,   0, 255, 255, 255 
				.byte    0,   0,   0,   0, 255, 255, 255, 255 
				.byte    0,   0,   0, 255, 255, 255, 255, 255 
				.byte    0,   0, 255, 255, 255, 255, 255, 255 
				.byte    0, 255, 255, 255, 255, 255, 255, 255 
				
chardata2		.byte    0,   0,   0,   0,   0,   0,   0,   0 ;1
				.byte  128, 128, 128, 128, 128, 128, 128, 128
				.byte  192, 192, 192, 192, 192, 192, 192, 192
				.byte  224, 224, 224, 224, 224, 224, 224, 224
				.byte  240, 240, 240, 240, 240, 240, 240, 240
				.byte  248, 248, 248, 248, 248, 248, 248, 248
				.byte  252, 252, 252, 252, 252, 252, 252, 252
				.byte  254, 254, 254, 254, 254, 254, 254, 254
				.byte  255, 255, 255, 255, 255, 255, 255, 255 ;2
				.byte    0, 255, 255, 255, 255, 255, 255, 255
				.byte    0,   0, 255, 255, 255, 255, 255, 255
				.byte    0,   0,   0, 255, 255, 255, 255, 255
				.byte    0,   0,   0,   0, 255, 255, 255, 255
				.byte    0,   0,   0,   0,   0, 255, 255, 255
				.byte    0,   0,   0,   0,   0,   0, 255, 255
				.byte    0,   0,   0,   0,   0,   0,   0, 255 
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;3
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0 
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;4
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				
				
chardata4		.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0 
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;2
				.byte  255,   0,   0,   0,   0,   0,   0,   0
				.byte  255, 255,   0,   0,   0,   0,   0,   0
				.byte  255, 255, 255,   0,   0,   0,   0,   0
				.byte  255, 255, 255, 255,   0,   0,   0,   0
				.byte  255, 255, 255, 255, 255,   0,   0,   0
				.byte  255, 255, 255, 255, 255, 255,   0,   0
				.byte  255, 255, 255, 255, 255, 255, 255,   0 
				.byte  255, 255, 255, 255, 255, 255, 255, 255 ;3
				.byte  254, 254, 254, 254, 254, 254, 254, 254
				.byte  252, 252, 252, 252, 252, 252, 252, 252
				.byte  248, 248, 248, 248, 248, 248, 248, 248
				.byte  240, 240, 240, 240, 240, 240, 240, 240
				.byte  224, 224, 224, 224, 224, 224, 224, 224
				.byte  192, 192, 192, 192, 192, 192, 192, 192
				.byte  128, 128, 128, 128, 128, 128, 128, 128 
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;4
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				
chardata3		.byte    0,   0,   0,   0,   0,   0,   0,   0 ;1
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0 
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;2
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0
				.byte    0,   0,   0,   0,   0,   0,   0,   0 ;3
				.byte    1,   1,   1,   1,   1,   1,   1,   1 
				.byte    3,   3,   3,   3,   3,   3,   3,   3
				.byte    7,   7,   7,   7,   7,   7,   7,   7
				.byte   15,  15,  15,  15,  15,  15,  15,  15
				.byte   31,  31,  31,  31,  31,  31,  31,  31
				.byte   63,  63,  63,  63,  63,  63,  63,  63
				.byte  127, 127, 127, 127, 127, 127, 127, 127
				.byte  255, 255, 255, 255, 255, 255, 255, 255 ;4
				.byte  255, 255, 255, 255, 255, 255, 255,   0 
				.byte  255, 255, 255, 255, 255, 255,   0,   0
				.byte  255, 255, 255, 255, 255,   0,   0,   0
				.byte  255, 255, 255, 255,   0,   0,   0,   0
				.byte  255, 255, 255,   0,   0,   0,   0,   0
				.byte  255, 255,   0,   0,   0,   0,   0,   0
				.byte  255,   0,   0,   0,   0,   0,   0,   0
				
				; ===========================================================================================================
		 
				*=MUSICLOAD
		 
				.binary "include\music\$1000\Enigma_PHA_theme.dat",2
				
				; ===========================================================================================================
		
				*=CHARRAM
		
				.binary "fonts\chimera.64c",2					; 58 chars (0-57)
		
				.byte $00,$00,$00,$00,$00,$00,$00,$00			; char 58
				.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff			; char 59
				.byte $00,$00,$00,$00,$00,$00,$00,$00			; char 60
				.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff			; char 61
			
				; ===========================================================================================================
			
				*=VICBASE
				
				.include "include\spritefont_bas_relief_2.inc"
			
				.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	; sprite 57	
				.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0

				; ===========================================================================================================
