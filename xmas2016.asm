; ==========================================================================
; =                                                                        =
; = Xmas intro 2014                                                        =
; =                                                                        =
; = Paul Alan Freshney										               =
; =                                                                        =
; =     https://github.com/MaximumOctopus/C64Intros                        =
; =                                                                        =
; = February 27th 2023 (original from December 13th 2014)                  =
; =                                                                        =
; ==========================================================================

				;.cpu 6502

				; === constants =============================================================================================
				
				.include "include\colours.inc"
				
				; ==========================================================================================================

				VICBASE   				= $4000
				SCREENRAM 				= VICBASE   + $0400
				CHARRAM   				= VICBASE   + $2000
				BITMAP					= VICBASE   + $2000
				SPRITEPTR 				= SCREENRAM + $03f8
				
				SPRITEDATA				= VICBASE	+ $1000 	; sprites 64 to 127
				
				BITMAPDATA				= $7f40
				COLOURDATA				= $8328
				
				COLOURRAM   			= $d800
				
				MUSICLOAD				= $1000
				
				; ==========================================================================================================
				
				SCROLL1COLOUR1  		= cWHITE
				SCROLL1COLOUR2	   		= cWHITE
				SPRITE1COLOUR0			= cLIGHTBLUE
				
				SCROLL3SPEED     		= $01			; frames per scroll

				zpSPRITE1SINOFFSET		= $02
				zpSPRITE2SINOFFSET		= $03
				zpSPRITE3SINOFFSET		= $04
				zpSPRITE4SINOFFSET		= $05
				zpSPRITE5SINOFFSET		= $06
				zpSPRITE6SINOFFSET		= $07
				zpSPRITE7SINOFFSET		= $08
				zpSPRITE8SINOFFSET		= $09

				zpSCROLL3YOFFSET		= $0a
				zpSCROLL3SPEED			= $0b		

				zpSCROLL1SINTIMER		= $0c
				
				zpHORIZTIMER			= $0d
				zpHORIZOFFSET			= $0e

				; ==========================================================================================================

				*=$0801

				.word (+), 2005  		; pointer, line number
				.null $9e, format("%d", start)
+				.word 0          		; basic line end

				; ==========================================================================================================

				*=$2000
				
start			sei

				; ==========================================================================================================

				lda $dd00
				and #%11111100
				ora #%00000010 			; VIC using bank2
				sta $dd00					
				
				lda #$3b
				sta $d011
				lda #$18
				sta $d016
				lda #$18
				sta $d018

	
				lda #cBLACK
				sta $d020
				sta $d021
				
				lda #$00				
				jsr	MUSICLOAD			; init music
				
				; ==========================================================================================================
				
				; ==========================================================================================================
				
				ldx #$00

loadimage		lda #$00
				sta BITMAP,x

				lda BITMAPDATA+$0000,x				; copy colour/data to VIC
				sta SCREENRAM+$0000,x
				lda BITMAPDATA+$0100,x
				sta SCREENRAM+$0100,x
				lda BITMAPDATA+$0200,x
				sta SCREENRAM+$0200,x
				lda BITMAPDATA+$0300,x
				sta SCREENRAM+$0300,x
				
				lda COLOURDATA+$0000,x
				sta $d800,x
				lda COLOURDATA+$0100,x
				sta $d900,x
				lda COLOURDATA+$0200,x
				sta $da00,x
				lda COLOURDATA+$0300,x
				sta $db00,x
				
				inx
				bne loadimage
							
				; ===========================================================================================================
				; == Initialise Variables ===================================================================================
				; ===========================================================================================================
				
				lda #$00
				sta zpHORIZTIMER
				sta zpHORIZOFFSET
				
				
				ldx #$00
				ldy #$00				; centre of sine movement
sinvals			sty zpSPRITE1SINOFFSET	; sine table offset
				
				iny
				
				inc sinvals+1
				
				inx
				
				cpx #8
				bne	sinvals

				lda #02
				sta zpSCROLL1SINTIMER	; y-coord sinus timer
				
				ldx #$a4				; lowest y value
				ldy #$00
sinustable		txa
				clc
				adc	offsets,y			; add offset to base value
				
				sta spriteysinus1,y
				
				iny

				cpy #00					; 60 values in the table
				bne sinustable

				; -----------------------------------------------------------------------------------------------------------
				
				lda #$00
				sta zpSCROLL3YOFFSET	; scroll #3 y-coord index
				lda #SCROLL3SPEED				
				sta zpSCROLL3SPEED		; scroll speed	
				
				; -----------------------------------------------------------------------------------------------------------
				
sprite_init		lda	#$ff				; 
				sta	$d015				; turn on all sprites
				sta	$d01c				; turn all sprites to hi colour
				sta $d017 				; stretch all vertically
				sta $d01d 				; stretch all horizontally
				
				lda #$00
				ldx #$00
				ldy #SPRITE1COLOUR0
shapeloop		lda scrolly,x
				sta	SPRITEPTR,x			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				sty $d027				; sprite colour
			
				inc	shapeloop+7			; modifies the address above!!
				inx
				txa
			
				cpx	#8
				bne	shapeloop
				
				lda #SCROLL1COLOUR1
				sta $d025				; sprite colour 1
				lda #SCROLL1COLOUR2				
				sta $d026				; sprite colour 2	
				
				lda #>scrolly+7			; update the address of the next sprite image in code
				sta sprite7xx+2			; high byte
				lda #<scrolly+7			;
				sta sprite7xx+1

				; ===========================================================================================================
				; == fixes the scrolly text sprite mapping, because sprite data is at 64-127 ================================
				; ===========================================================================================================
							
fixtext			lda scrolly				; scrolly character
				
				cmp	#$f0				; control codes are $f0 - $ff
				bcs	nextchar			;
				
				cmp	#$00				; $00 represents last byte of scroll text
				beq	endfix
				
				clc						
				adc	#$40				; add 64 (moves from sprite 0-63 to 64-127)
storetext		sta scrolly
				
nextchar		inc fixtext+1			; move to next position in scroll text
				bne	c1					; crossed a page boundary?
				inc fixtext+2			; yes
				
c1				inc storetext+1			; move to next position in scroll text
				bne	fixtext				; crossed a page boundary?
				inc storetext+2			; yes

				jmp	fixtext
endfix			
				
				; ==========================================================================================================
				; ==========================================================================================================
				; ==========================================================================================================

				ldx #$00
				lda #$00
main	    	bit $d011 				; Wait for new frame
				bpl *-3
				bit $d011
				bmi *-3
				
				lda #cBLACK
				sta $d020     			; border colour
				
				ldy zpHORIZOFFSET
				ldx horizscroll,y
				stx $d016
				ldx	vertiscroll,y
				stx $d011
				
				; ==========================================================================================================
				; == Stars =================================================================================================
				; ==========================================================================================================
				
				lda	#$ff				; 
				sta	$d015				; turn on all sprites
				lda	#$00
				sta $d017 				; stretch all vertically
				sta $d01d 				; stretch all horizontally
				lda	#$c0
				sta $d010				; clear the bit-8 flags of x-coords
				
				ldx	#$7a
				stx	SPRITEPTR			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				stx	SPRITEPTR+1			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				stx	SPRITEPTR+2			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				stx	SPRITEPTR+3			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				stx	SPRITEPTR+4			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				stx	SPRITEPTR+5			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				stx	SPRITEPTR+6			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				stx	SPRITEPTR+7			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				
				lda	starsx				; put stars on screen
				sta	$d000				;
				lda	starsy				;
				sta	$d001				;
				lda	starsx+1			;
				sta	$d002				;
				lda	starsy+1			;
				sta	$d003				;
				lda	starsx+2			;
				sta	$d004				;
				lda	starsy+2			;
				sta	$d005				;
				lda	starsx+3			;
				sta	$d006				;
				lda	starsy+3			;
				sta	$d007				;
				lda	starsx+4			;
				sta	$d008				;
				lda	starsy+4			;
				sta	$d009				;
				lda	starsx+5			;
				sta	$d00a				;
				lda	starsy+5			;
				sta	$d00b				;
				lda	starsx+6			;
				sta	$d00c				;
				lda	starsy+6			;
				sta	$d00d				;
				lda	starsx+7			;
				sta	$d00e				;
				lda	starsy+7			;
				sta	$d00f				;
				
				; ==========================================================================================================
				
				jsr	MUSICLOAD+3
				
				; ==========================================================================================================				
				
				dec zpHORIZTIMER
				bne	resethoriz
				
				ldx #$20
			
				cpx $d012
				bne *-3
				
			
				ldy zpHORIZOFFSET
horioffset		ldx horizscroll,y		; offset to scroll table
				stx $d016
				ldx	vertiscroll,y
				stx $d011
				
				iny
				cpy #$14
				bne	resethtimer
				
				ldy #$00				; reset horiz scroll table offset
				
resethtimer		sty zpHORIZOFFSET
				
				lda #$03
				sta zpHORIZTIMER
				
resethoriz		ldx #$9a				; y raster after Merry Christmas text
			
			
				cpx $d012				; wait for this y
				bne *-3
				
				lda #$18				; reset horiz scroll
				sta $d016				;
				ldx	#$3b
				stx $d011

				
				ldx #$a0
				ldy #cDARKGREY

				cpx $d012				; wait for this y
				bne *-3
				
				sty $d020
				
				; ===========================================================================================================
				; == Sprite Scrolly =========================================================================================
				; ===========================================================================================================
				
scrolltext3		lda	#$ff				; 
				sta	$d015				; turn on all sprites
				sta $d017 				; stretch all vertically
				sta $d01d 				; stretch all horizontally

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
				
				bne sprite3y			; no
				
				lda #>scrolly			; update the address of the next sprite image in code
				sta sprite7xx+2			; high byte
				lda #<scrolly			;
				sta sprite7xx+1			; low byte
				
				ldx	#$6e				; if char is $00 then change it to .
				stx scroll3sprite7+1	
				
sprite3y		stx	SPRITEPTR+7

				ldx zpSPRITE1SINOFFSET	; load y-coord from sinus table
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
				
				dec zpSCROLL1SINTIMER	; sinus scroll timer
				bne	continuecentre
				
				lda #$01				; reset the timer
				sta zpSCROLL1SINTIMER	; 
				
				inc zpSPRITE1SINOFFSET
				inc zpSPRITE2SINOFFSET
				inc zpSPRITE3SINOFFSET
				inc zpSPRITE4SINOFFSET
				inc zpSPRITE5SINOFFSET
				inc zpSPRITE6SINOFFSET
				inc zpSPRITE7SINOFFSET
				inc zpSPRITE8SINOFFSET

continuecentre	ldy zpSCROLL3YOFFSET	; offset to x-coord table
				
movesprites3	lda spritemove0,y		;
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
				beq topscroller
				
				jmp bottomraster				
				
				; ===========================================================================================================

topscroller		lda #SCROLL3SPEED		; effective scroll speed = x frames per scroll
				sta zpSCROLL3SPEED		;
				
				iny						; move to next sprite co-ordinate in lookup table			
				cpy	#25					; reached end?
				beq reachend
				
				jmp	continue3			; no

reachend		ldy #$00				; yes, go to beginning of y-coord lookup table

				lda $d028				; transfer sprite colours
				sta	$d027				;
				lda $d029				; 
				sta	$d028				;
				lda $d02a				; 
				sta	$d029				;
				lda $d02b				; 
				sta	$d02a				;
				lda $d02c				; 
				sta	$d02b				;
				lda $d02d				; 
				sta	$d02c				;
				lda $d02e				; 
				sta	$d02d				;
				
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
				
				clc						;
				adc #6					;

spritecont		sta zpSPRITE8SINOFFSET	; store in sprite 8

movenext		inc sprite7xx+1			; move to next position in scroll text
				bne	sprite7xx			; crossed a page boundary?
				inc sprite7xx+2			; yes
				
sprite7xx		lda $1000				; no, read the next character
				
				cmp	#$f0				; control code?
				bcc	normalchar			;
			
controlchar		and #$0f				; yes
				sta spritecolour+1		;
				
				jmp	spritecont			; goto next non-control char
			
normalchar		sta scroll3sprite7+1	; no, place it in sprite 7
				
spritecolour	lda #SPRITE1COLOUR0		;
				sta $d02e				;
				
continue3		sty zpSCROLL3YOFFSET	;
				
				; ==========================================================================================================
				
bottomraster	ldx #$f2				; raster y
				lda	#cWHITE				;

				cpx $d012				;
				bne *-3					;
				
				sta $d020      			; border colour
				
				jmp main				; next frame!

				; ==========================================================================================================
				; ==========================================================================================================
				; ==========================================================================================================
			
				.enc "screen"
scrolly		 	.text '........ ',$f7,'WELCOME ',$fe,'TO MY FIRST EVER ',$f4,'XMAS ',$fe,'C64 INTRO# DECEMBER 13TH 2016. CODE AND GRAPHICS BY P.A.FRESHNEY, MUSIC BY METAL, CALLED '
				.text '"XMAS BEAT". ',$fa,'GREETZ ',$fe,'TO SARAH, DAN, AMY, DAVE, TARTY, ',$ff,'THE DEVELOPMENT CATS ',$fe,'RUTHERFORD, FREEMAN AND MAXWELL, STEVE, STEVE, '
				.text 'AND COMMODORE FANS EVERYWHERE# DEDICATED TO JULIE, DYANNE, AND ADAM. '
				.text 'CHECK OUT ',$f3,'GITHUB.COM/MAXIMUMOCTOPUS ',$fe,'FOR MORE C64 STUFF... ',$f3,'ARTSTATION.COM/MRFRESH3141 ',$fe,'AND ',$f3,'MAXIMUMOCTOPUS.COM ',$fe,'FOR UPDATES, '
				.text 'ELECTRONICS, FREE SOFTWARE AND OTHER COOL STUFF. THIS INTRO IS 1253 LINES OF ASSEMBLER.... HAVE A WOBBLY MERRY CHRISTMAS $) ......'
				.byte $00	; always end with a zero
				
				.align $100
horizscroll		.byte $18,$18,$19,$19,$1a,$1b,$1c,$1d,$1e,$1e,$1f,$1f,$1e,$1e,$1d,$1c,$1b,$1a,$1a,$19
vertiscroll		.byte $3b,$3a,$39,$3a,$3b,$3a,$39,$3a,$3b,$3a,$39,$3a,$3b,$3a,$39,$3a,$3b,$3a,$39,$3a

starsx			.byte $22,$44,$58,$97,$c0,$f0,$1a,$38
starsy			.byte $66,$84,$32,$39,$5d,$52,$32,$62

offsets			.byte 18, 19, 21, 22, 24, 25, 26, 27, 29, 30, 30, 31, 32, 32, 32, 32
				.byte 32, 32, 32, 31, 31, 30, 29, 29, 28, 27, 26, 25, 24, 23, 22, 21
				.byte 20, 19, 19, 18, 18, 17, 17, 17, 17, 17, 17, 18, 18, 19, 19, 20
				.byte 21, 21, 22, 23, 24, 24, 25, 26, 26, 27, 27, 27, 27, 27, 27, 27
				.byte 27, 26, 25, 25, 24, 23, 22, 20, 19, 18, 16, 15, 13, 12, 10,  9
				.byte  8,  6,  5,  4,  3,  2,  1,  1,  0,  0,  0,  0,  0,  1,  1,  2
				.byte  2,  3,  4,  5,  6,  7,  9, 10, 11, 12, 14, 15, 16, 17, 18, 19
				.byte 20, 20, 21, 21, 22, 22, 22, 22, 22, 21, 21, 21, 20, 19, 19, 18
				.byte 18, 17, 16, 16, 15, 15, 14, 14, 14, 13, 13, 13, 14, 14, 14, 15
				.byte 16, 17, 17, 18, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 32
				.byte 33, 34, 34, 35, 35, 35, 35, 35, 35, 34, 34, 33, 32, 31, 30, 28
				.byte 27, 26, 24, 23, 21, 20, 19, 17, 16, 15, 13, 12, 11, 10, 10,  9
				.byte  9,  8,  8,  8,  8,  8,  8,  9,  9, 10, 10, 11, 12, 13, 13, 14
				.byte 15, 16, 16, 17, 17, 18, 18, 18, 18, 18, 18, 18, 17, 17, 16, 16
				.byte 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  6,  5,  4,  4,  3,  3
				.byte  3,  3,  3,  3,  4,  4,  5,  6,  7,  8,  9, 11, 12, 13, 15, 17
				
				.align $100
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
				
				.align $100
spriteysinus1	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
								
				; ==========================================================================================================
				
				*=SPRITEDATA					; loads sprites to positions 64-127
				
				.include "include\spritefont_flat_snow.inc" ; 59 sprites
				
				; ==========================================================================================================				
								
				*=BITMAP ; saved in format Koala Painter
		
				.binary "screens\xmas3.prg"

				; ==========================================================================================================
				
				*=MUSICLOAD
				
				.binary "include\music\$1000\Xmas_Beat.dat",2
				
				; ==========================================================================================================
