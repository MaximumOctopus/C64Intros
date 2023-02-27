; ==============================================================
; =                                                            =
; = Raster Demo 20 (scroller/text/cls/music/bars+cycling)      =
; =                                                            =
; = raster bars + sprites + music + text (with fade in (v2.0)) =
; = + better sinus effect                                      =
; = + colour effect on top scroller                            =
; = + horizontal scroll of bottom text                         =
; = + gentle start                                             =
; = + open bottom border                                       =
; = + scroller in bottom border                                =
; =                                                            =
; = Paul Alan Freshney									       =
; =                                                            =
; =   github.com/MaximumOctopus                                =
; =                                                            =
; = February 27th 2023 (original version July 12th 2014)       =
; =                                                            =
; ==============================================================

				;.cpu 6502
				
				; === constants =============================================================================================
				
				.include "include\colours.inc"
				
				; ==========================================================================================================

				VICBASE      = $8000
				SCREENRAM    = VICBASE   + $2000
				CHARRAM      = VICBASE   + $2800
				SPRITEPTR    = SCREENRAM + $03f8
				
				line      	 = SCREENRAM+$140		; screen RAM offset to beginning of scroll text
				
				SCROLLSPEED  = $01					; scroll speed of the bottom scroller
				
				; ==========================================================================================================
				
				MUSICLOAD    		= $1000
				MUSICINIT   		= $1000
				MUSICPLAY    		= $1012
				
				; ==========================================================================================================
				
				zpSPRITEOCT_YDATA   = $03 ; (03 - 10) (odd bytes)
				zpSPRITEOCT_XDATA   = $04 ; (03 - 10) (even bytes)
				
				zpSPRITEMAX_YDATA   = $40 ; (40 - 4d) (even bytes)
				zpSPRITEMAX_XDATA   = $41 ; (40 - 4d) (odd bytes)
				
				zpCYCLEDELAY     	= $14
				zpCURRENTMESSAGE 	= $15
				zpMESSAGETIMER   	= $16
				
				zpFADESTATUS     	= $17
				zpFADECOLOUR	 	= $18
				zpFADECHAR       	= $19
				
				zpSCROLLCYCLEOFFSET = $1a
				zpSCROLLCYCLEDELAY  = $1b
				
				zpWOBBLYTXTHORIZOFF	= $1c
				zpWOBBLYTXTTIMER    = $1d
				
				zpSCROLLHORIZOFF	= $1e
				
				zpWOBBLYRASTEROFF   = $1f
				
				zpRANDOMOFFSET		= $20
				
				zpBOTTOMSCROLLYOFF	= $21
				zpBOTTOMSCROLLYSPD  = $22
				
				zpSPRITEYTIMER		= $23
				
				zpINTROFADE		 	= $3e
				zpINTROCOUNT     	= $3f
				
				; ==========================================================================================================

				*=$0801

				.word (+), 2005  		;pointer, line number
				.null $9e, format("%d", start)
+				.word 0          		;basic line end

				; ==========================================================================================================

				*=$4000
				
start			sei           			; disable interrupts

				lda #$1b
				sta $d011

				jsr scrinit				; initialise scroll routine
				
initbottoms		lda #>bottomscrolly+7	; update the address of the next sprite image
				sta sprite7xx+2			; in code
				lda #<bottomscrolly+7	;
				sta sprite7xx+1
				
				; ==========================================================================================================
				
				ldy #$00
sinustable		lda #$32				; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus1,y		; store in table
				
				lda #$a7				; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus2,y		; store in table
				
				iny

				cpy #30					; 30 values
				bne sinustable				

				; ==========================================================================================================
				
				lda #$00
				tax
				tay
				jsr MUSICINIT			; initialise music
		
				lda #$00       			; black
				sta $d020      			; border
				sta $d021      			; background
		
				; %10001010				; sprites        at $0000  (+ base VIC address)
				lda #$8a				; screen         at $2000 
				sta $d018				; character data at $2800
				
				lda #$36				;
				sta $01					;
			
				lda $dd00
				and #%11111100
				ora #%00000001 			; VIC using bank2
				sta $dd00			
		
				ldx #$00
				lda #$20			
cls				sta SCREENRAM,x			; clear the screen		
				sta SCREENRAM+$100,x
				sta SCREENRAM+$200,x
				sta SCREENRAM+$300,x
				inx
				bne cls
				
textrasterinit	lda #$fa				; reset delay
				sta zpCYCLEDELAY
				
				lda #$00				; current message being display
				sta zpCURRENTMESSAGE
				
				lda #$ff				; countdown until next message
				sta zpMESSAGETIMER
				
fadeininit		lda #$00
				sta zpFADESTATUS		; fade status ($00 fade in chars, $01 fade out, $02 write new message, $ff wait)
				sta zpFADECOLOUR		; fade in enabled ($00 = yes)
				sta zpFADECHAR			; character to fade in
				
				sta zpSCROLLCYCLEOFFSET	; offset to colour table
				lda #$05				; the initial value is 1 more than the actual loop delay (*perfect* timing)
				sta zpSCROLLCYCLEDELAY	; delay
				
				lda #$00				; text horiz scroll table offset
				sta zpWOBBLYTXTHORIZOFF
				lda #$04				; text horiz scroll timer
				sta zpWOBBLYTXTTIMER
				
				lda #$07				; initscroll
				sta zpSCROLLHORIZOFF	;
				
				lda #$00				; offset to bottom raster bar cycle colour table
				sta zpWOBBLYRASTEROFF
				
				lda #$00
				sta zpBOTTOMSCROLLYOFF	; scroll x-coord index
				lda #SCROLLSPEED				
				sta zpBOTTOMSCROLLYSPD	; which sprite shape is showing, from lookup table
				
				lda #$60				; slight delay before the action starts
				sta zpSPRITEYTIMER		;
		
				; ===========================================================================================================
			
sprite_init		lda	#$00				;
				sta	$d015				; turn on first 7 sprites
			
				lda #$00
				ldx #$00
				ldy #$01				; colour white
shapeloop		sta	SPRITEPTR,x			; sprite shape (base VIC bank address + $07F8 + sprite # (0-6))
				sty $d027				; sprite colour
			
				inc	shapeloop+4			; modifies the address above!!
			
				inx
				txa
			
				cpx	#7
				bne	shapeloop

				lda #$0f
				sta $d025				; sprite colour 1
				lda #$0c
				sta $d026				; sprite colour 2
				lda #$0c
				sta $d026				; sprite colour 2
				
				lda #$00
				sta $12					; stores the offset to the sprite colour table
				lda #$07
				sta $13
				
				ldx #$00
				ldy #$00
xyloop			lda spritex,x	
				sta $d000,x				; sprite x coord
			
				lda #$40
				sta $d001,x				; sprite y coord

				stx zpSPRITEOCT_XDATA,y
				stx zpSPRITEOCT_YDATA,y
				
				stx zpSPRITEMAX_XDATA,y
				stx zpSPRITEMAX_YDATA,y

				iny
				iny
				
				inx					
				lda spritex,x			; read the upper bit of the x-coord
				ora	$d010				; or it with current value
				sta $d010				; write it out
				
				inx
			
				cpx #14
				bne xyloop
							
				; ===========================================================================================================
			
				ldx #$00
				ldy #$00
text			lda #$a0				; inverse-video "space"
				sta SCREENRAM+$348,x
				sta SCREENRAM+$3c0,x
				sta SCREENRAM+$398,x
				sta SCREENRAM+$370,x
				
				lda #$00				; black
				sta $dbc0,x
				sta $db98,x
				sta $db70,x
				sta $db48,x
				
				lda #$01				; white
				sta	$d940,x				; the location of the scrolly text
				
				inx		
				cpx	#40					; 40 chars in message
				bne text
				
				lda #$3f
				sta zpRANDOMOFFSET
				
				lda #$00				;
				sta $30					; raster bar colours
				sta $31					;
				sta $32					;
				sta $33					;
				sta $34					;
				sta $35					;
				sta $36					;
				sta $37					;
				sta $38					;
				
				bit $d011 				; Wait for new frame
				bpl *-3					;
				bit $d011				;
				bmi *-3					;
				
				; ===========================================================================================================
				; == nice intro start =======================================================================================
				; ===========================================================================================================
				
				lda #$10				; slight delay before fading in (#frames)
				sta zpINTROFADE
				lda #$00				; frame count, exit to main loop once max reached
				sta zpINTROCOUNT
				
prepare			bit $d011 				; Wait for new frame
				bpl *-3
				bit $d011
				bmi *-3
				
b1y				ldy #$64   				; start raster line
				ldx #$00      			; 
sb1  			lda cat+1,x   			; load colour from table + offset

				cpy $d012   			; reached next raster yet?
				bne *-3	    			; 

				sta $d020      			; border
				sta $d021      			; background

				cpx #6        			; 6 raster lines
				beq b2y     			; 

				inx						; next colour
				iny            			; next raster line

				jmp sb1     			; jump to loop.	
							
b2y				ldy #$94   				; start raster line
				ldx #$06      			; 6 raster lines
sb2  			lda cat,x  				; load colour from table + offset

				cpy $d012   			; reached next raster yet?
				bne *-3	    			; 

				sta $d020      			; border				
				sta $d021      			; background

				cpx #$00        		; reached last one?
				beq keep     			; 

				dex						; next colour (backwards)
				iny            			; next raster line
				
				jmp sb2      			; jump to loop.	
				
keep			dec zpINTROFADE
				bne	prepare
				
				lda #$05
				sta zpINTROFADE
				
				clc
				lda sb1+1
				adc	#8
				sta sb1+1
				
				clc
				lda sb2+1
				adc	#8
				sta sb2+1
				
				inc zpINTROCOUNT
				ldx zpINTROCOUNT
				cpx #7
				beq maininit
				
				jmp prepare
				
				; ===========================================================================================================				
				
maininit				
				ldx #$00
initmessage1	lda message1,x
				sta SCREENRAM+$1be,x	; place it in to screen RAM
				lda #$06				; make the text invisible
				sta $d9be,x				; using dark blue
				
				inx
				cpx #28
				bne initmessage1
				
				lda	#$7f				; 
				sta	$d015				; activate sprites 0-6
				lda	#$ff				; 
				sta	$d01c				; multi-col for sprites 0-7

				; ===========================================================================================================
				; == Main Demo Loop Starts Here =============================================================================
				; ===========================================================================================================

				ldx #$00
				lda #$00
main	    	bit $d011 ; Wait for new frame
				bpl *-3
				bit $d011
				bmi *-3

				jsr MUSICPLAY			; play music (should run every 1/50th of a second)

				; ===========================================================================================================
			
				lda #$0f
				sta $d025				; sprite colour 1
				lda #$0c
				sta $d026				; sprite colour 2
			
				lda	#$7f
				sta $d015 				; activate sprites 0-6
				lda #$00
				sta $d01d 				; no stretching horiz
			
				ldx #$00
				stx $d010				; clear the x-coord 9th bit
xyloop2			lda spritex,x	
				sta $d000,x				; sprite x coord
					
				inx					
				lda spritex,x			; read the upper bit of the x-coord
				ora	$d010				; or it with current value
				sta $d010				; write it out
				
				inx
			
				cpx #14
				bne xyloop2

				; ===========================================================================================================				
				
				dec zpBOTTOMSCROLLYSPD	; scroll speed
				bne scrollcolcycle
				
				lda #SCROLLSPEED		; effective scroll speed = x frames per scroll
				sta zpBOTTOMSCROLLYSPD
				
				ldy zpBOTTOMSCROLLYOFF
				
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
				
				inc sprite7xx+1			; move to next position in scroll text
				bne	sprite7xx			; crossed a page boundary?
				inc sprite7xx+2			; yes
				
sprite7xx		lda $ca75				; no, read the next character
				sta scroll3sprite7+1	; place it in sprite 7
				
continue3		sty zpBOTTOMSCROLLYOFF	; update sprite x-coord lookup
			
				; ===========================================================================================================
			
scrollcolcycle	dec zpSCROLLCYCLEDELAY	; cycle delay counter
				bne spr_maximum
				
				lda #$04				; reset delay
				sta zpSCROLLCYCLEDELAY	;
			
				ldx zpSCROLLCYCLEOFFSET	; colour table offset
				inx
				cpx #$0c				; reached max?
				bne nextcolour			; no
				
				ldx #$00				; yes, clear
				
nextcolour		stx zpSCROLLCYCLEOFFSET	; remember
				
				ldy #$00				; char 0
colourloop		lda topcolour,x			; read colour
				sta $d940,y				; write to colour RAM

				inx						; next colour
				iny						; next char
				cpy #39					; reaced end?
				bne colourloop			; no
				
				; ===========================================================================================================
						
spr_maximum	    ldy #$00
sprite_mpx_1	ldx zpSPRITEMAX_YDATA,y
				
				lda spriteysinus1,x

				sta	$d001,y				; y-coord

				cpx #29					; 30 y-coords
				beq sprite_mpx_1r
			
				inx
				jmp sprite_mpx_1c
				
sprite_mpx_1r	ldx #$00


sprite_mpx_1c	stx zpSPRITEMAX_YDATA,y

				ldx zpSPRITEMAX_XDATA,y

				lda spritex,y			; load the base x-coord
				clc						
				adc spriteysinusof2,x	; add an offset (sine)
				sta $d000,y				; store back in sprite's x register
				
				bmi	clearbit9			; did the value underflow?
				bcc	savex				; did the value overflow?
				
setbit9 		lda bitzON,y			; set this sprite's 9th x-coord bit
				ora $d010				;
				sta $d010				;
				
				jmp savex
				
clearbit9		lda bitzOFF,y			; clear this sprite's 9th x-coord bit
				and $d010				;
				sta $d010				;
				
savex			lda zpSPRITEYTIMER
				bne sprite_mpx_1i
				
				inx
				cpx	#30
				bne	sprite_mpx_1y
				
				ldx #$00
				
sprite_mpx_1y	stx zpSPRITEMAX_XDATA,y
			
sprite_mpx_1i	iny						; move to next sprite's y-coord
				iny
			
				cpy	#14
				bne	sprite_mpx_1
				
				dec zpSPRITEYTIMER
				bpl conts1

				lda #$01
				sta zpSPRITEYTIMER
				
conts1				
			
				ldx #$0d				; M
sprite_mpx_1b	stx	SPRITEPTR			; sprite shape (base VIC bank address + $07F8 + sprite # (0-6))
				ldx #$01				; A
				stx	SPRITEPTR+1			; 
				ldx #$18				; X
				stx	SPRITEPTR+2			; 
				ldx #$09				; I
				stx	SPRITEPTR+3			; 
				ldx #$0d				; M
				stx	SPRITEPTR+4			; 
				ldx #$15				; U
				stx	SPRITEPTR+5			; 
				ldx #$0d				; M
				stx	SPRITEPTR+6			;
				
				; ===========================================================================================================
				
rasterbar1		ldy #$62
				cpy $d012
				bne *-3	
	
				ldy #$64   				; start raster line
				ldx #$00      			; 
rbar1loop  		lda colorsrbtop,x   	; load colour from table + offset

				cpy $d012   			; reached next raster yet?
				bne *-3	    			; 

				sta $d020      			; border
				sta $d021      			; background

				cpx #6        			; 6 raster lines
				beq textscroller   		; 

				inx						; next colour
				iny            			; next raster line

				jmp rbar1loop      		; jump to loop.	
				
				; ===========================================================================================================
	
textscroller	jsr irq
		
				; ===========================================================================================================
				
rasterbar2		ldy #$92
				cpy $d012
				bne *-3
			
				ldy #$94    			; start raster line
				ldx #$00      			; load $00 into X
rbar2loop  		lda colorsrbbot,x  		; load colour from table + offset

				cpy $d012   			; reached next raster yet?
				bne *-3	    			; 

				sta $d020      			; border				
				sta $d021      			; background

				cpx #6        			; 6 raster lines
				beq raster3cycle		; 

				inx						; next colour
				iny            			; next raster line

				jmp rbar2loop      		; jump to loop.	
		
				; ===========================================================================================================
		
raster3cycle	inc zpCYCLEDELAY		; cycle delay

				bne spr_octopus

				lda zpWOBBLYRASTEROFF
				tax
				tay
				
r3cycle			lda colorsw,y
				sta $30
				iny
				lda colorsw,y
				sta $31
				iny
				lda colorsw,y
				sta $32
				iny
				lda colorsw,y
				sta $33
				iny
				lda colorsw,y
				sta $34
				iny
				lda colorsw,y
				sta $35
				iny
				lda colorsw,y
				sta $36
				iny
				lda colorsw,y
				sta $37
				
				inx
				
				cpx #40
				bne	updateoffset	
				
clearoffset		ldx #$00
				
updateoffset	stx zpWOBBLYRASTEROFF
		
				lda #$fb				; reset delay
				sta zpCYCLEDELAY
spr_octopus
				; ===========================================================================================================
			
				ldy #$00
sprite_mpx_2	ldx zpSPRITEOCT_YDATA,y
				
				lda spriteysinus2,x

				sta	$d001,y				; y-coord

				cpx #29					; 30 y-coords
				
				beq sprite_mpx_2r
			
				inx
				jmp sprite_mpx_2c
				
sprite_mpx_2r	ldx #$00

sprite_mpx_2c	stx zpSPRITEOCT_YDATA,y

				ldx zpSPRITEOCT_XDATA,y

				lda spritex,y			; load the base x-coord
				clc						
				adc spriteysinusoff,x	; add an offset (sine)
				sta $d000,y				; store back in sprite's x register
				
				bmi	clearbit9b			; did the value underflow?
				bcc	savexb				; did the value overflow?
				
setbit9b 		lda bitzON,y			; set this sprite's 9th x-coord bit
				ora $d010				;
				sta $d010				;
				
				jmp savexb
				
clearbit9b		lda bitzOFF,y			; clear this sprite's 9th x-coord bit
				and $d010				;
				sta $d010				;
				
savexb			lda zpSPRITEYTIMER
				bne sprite_mpx_2i
				
				inx
				cpx	#30
				bne	sprite_mpx_2y
				
				ldx #$00
				
sprite_mpx_2y	stx zpSPRITEOCT_XDATA,y
			
sprite_mpx_2i	iny						; move to next sprite's y-coord
				iny
			
				cpy	#14
				bne	sprite_mpx_2
			
				ldx #$0f				; O
sprite_mpx_2b	stx	SPRITEPTR			; sprite shape (base VIC bank address + $07F8 + sprite # (0-6))
				ldx #$03				; C
				stx	SPRITEPTR+1			; 
				ldx #$14				; T
				stx	SPRITEPTR+2			; 
				ldx #$0f				; O
				stx	SPRITEPTR+3			; 
				ldx #$10				; P
				stx	SPRITEPTR+4			; 
				ldx #$15				; U
				stx	SPRITEPTR+5			; 
				ldx #$13				; S
				stx	SPRITEPTR+6			;

				; ===========================================================================================================

				ldx zpWOBBLYTXTHORIZOFF	; current x-offset
				
				dec zpWOBBLYTXTTIMER	; timer delay
				bne	textraster			; ready for new value
			
				lda #$04				; yes, reset delay
				sta zpWOBBLYTXTTIMER	;

				ldx zpWOBBLYTXTHORIZOFF	; load table offset
				inx						; move to next value
				cpx #20					; last one?
				bne savehoriz			; no
				
				ldx #$00				; yes, reset to first item
				
savehoriz		stx zpWOBBLYTXTHORIZOFF	; save offset to table

textraster		stx horioffset+1		; needed for update mechanism (below)
			
				; ===========================================================================================================
			
				ldy #$e2    			; raster line to start at
				ldx #$30      			; index to colour table
				stx coloffset+1			; reset offset
loop2  			

horioffset		ldx horizscroll			; offset to scroll table
coloffset		lda $30					; offset to colour table

				cpy $d012   			; reach raster line yet?
				bne *-3	    			; 

				stx $d016				; alter the horiz scroll register	
				sta $d021      			; background colour (foreground of text!)

				cpy #$f2        		; number of bars
				beq colourcycle   		; 
				
				inc	horioffset+1		; move to next value in table
				inc coloffset+1			; move to next value in table

				iny            			; make each raster bar 2 pixels (rows) high
				iny

				jmp loop2      			; keep rastering	
				
				; ===========================================================================================================

colourcycle		jmp borderz

				.align $100
				
borderz			ldx #$f4
				jsr rastersync
				
				lda #$f8
rl1   			cmp $d012
				bne rl1

				lda #$18&$f7			; open the lower border
				sta $d011				;
				
				lda	#$ff				; 
				sta $d015 				; all sprites active
				sta $d01d 				; stretch all horizontally
				
				lda #$0a
				sta $d025				; sprite colour 1
				lda #$02
				sta $d026				; sprite colour 2
				
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
				
				cpx #$00				; have we reached the end? (last character of scroll is $00)
				bne movebotsprites		; no
				
				lda #>bottomscrolly+7	; update the address of the next sprite image
				sta sprite7xx+2			; in code
				lda #<bottomscrolly+7	;
				sta sprite7xx+1
				
movebotsprites	ldx zpBOTTOMSCROLLYOFF	; offset to x-coord table				
				lda spritemove0,x
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
				
				ldx #$ff				;
				stx $d001				; y-coords
				stx $d003				;
				stx $d005				;
				stx $d007				;
				stx $d009				;
				stx $d00b				;
				stx $d00d				;
				stx $d00f				;

animateback		cpy	$d012				; wait until raster 255
				bne *-3					;
				
				lda #$1b				; reset horiz offset
				sta $d011				;

				; ===========================================================================================================

keepgoing		ldy zpFADESTATUS		; 
				cpy #$ff				; am i waiting for the message to change
				beq messagechange		; yes
				
				cpy #$01
				beq fadeout				; fadeout
				
				cpy #$02
				beq writemessage		; next message in place
				
				; ===========================================================================================================
				
fadein			ldx zpFADECOLOUR		; offset to fade in colour table
				ldy zpFADECHAR			; offset to colour RAM
				lda fadeincolours,x		; get fade in colour
				sta $d9be,y				; 
				
				cpx #6					; reached the end of the fade in colours?
				beq nextcharacter		; yes
				
				inx						; no
				stx zpFADECOLOUR
				
				jmp mainloop
				
nextcharacter	cpy #27					; reached the end (28th character)
				beq stopfadein			; yes
				
				lda #$00				; start at begin of colour fade colours
				sta zpFADECOLOUR		;
				inc zpFADECHAR			; next character
				jmp mainloop
				
stopfadein		lda #$ff				; fade in disable
				sta zpFADESTATUS					
				jmp mainloop
				
				; ===========================================================================================================
				
fadeout			ldx zpFADECOLOUR		; offset to fade in colour table
				ldy #$00				; offset to colour RAM
				lda fadeoutcolours,x	; get fade in colour
changecolour	sta $d9be,y				; 
				iny
				cpy #28					; 28 characters per message
				bne changecolour
				
				cpx #7					; reached the end of the fade in colours?
				beq stopfade			; yes
				
				inx						; no
				stx zpFADECOLOUR
				
				jmp mainloop
				
stopfade		lda #$02				; disable fade in
				sta zpFADESTATUS
				sta zpFADECOLOUR
				sta zpFADECHAR
				jmp mainloop
				
				; ===========================================================================================================

messagechange	dec zpMESSAGETIMER		; message change delay
				bne	mainloop
				
				lda #$01				; activate fade out
				sta zpFADESTATUS		; fade in colour enable
				lda #$00
				sta zpFADECHAR
				sta zpFADECOLOUR		; offset to fade in colour table (start at beginning)
				
				jmp mainloop
				
				; ===========================================================================================================

writemessage	lda #$ff				; 255 frames delay + fade in/out time
				sta zpMESSAGETIMER
				
				lda zpCURRENTMESSAGE	; index of message data
				adc #$1b				; 27 characters per message
				cmp #$fc
				bne	storeoffset
				
				lda #$00				; reset position
				
storeoffset		sta zpCURRENTMESSAGE
				
				tax				
				ldy #$00				; screen data offset
newmessage		lda #$06	
				sta $d9be,y				; dark blue	(starts 6 chars in from edge)
				lda message1,x			; message data offset							
				sta SCREENRAM+$1be,y	; screen data (starts 6 chars in from edge)
				iny
				inx
				cpy #28					; 28 characters per message
				bne newmessage				

				lda #$00				; activate fade-out
				sta zpFADESTATUS		; fade-in colour enable
				sta zpFADECHAR
				sta zpFADECOLOUR		; offset to fade-in colour table (start at beginning)
				
				; ===========================================================================================================
							
mainloop		nop						; this ends up being a jmp main
				nop						; once the sprites have been built
				nop						;

				ldy zpRANDOMOFFSET		; current offset
				ldx	random,y			; creates a randomish appearance
							
				lda $C000+(0*64),x		; copy the sprite data to each of the 14 sprites
				sta VICBASE+(13*64),x	; one byte at a time (per frame)
				lda $C000+(1*64),x		;
				sta VICBASE+(1*64),x	;
				lda $C000+(2*64),x		;
				sta VICBASE+(24*64),x	;
				lda $C000+(3*64),x		;
				sta	VICBASE+(9*64),x	;
				lda $C000+(4*64),x		;
				sta VICBASE+(21*64),x	;
				lda $C000+(5*64),x		;
				sta VICBASE+(15*64),x	;
				lda $C000+(6*64),x		;
				sta VICBASE+(3*64),x	;
				lda $C000+(7*64),x		;
				sta VICBASE+(20*64),x	;
				lda $C000+(8*64),x		;
				sta VICBASE+(16*64),x	;
				lda $C000+(9*64),x		;
				sta VICBASE+(19*64),x	;
				
				lda copyright,x			; load character
				ora #$80				; add 128, set inverse-video mode
				ldy textoffset,x		; get correct RAM offset
				sta SCREENRAM+$370,y	; place it in video RAM
				
				dec zpRANDOMOFFSET		; next sprite byte
				bpl	copyme				; gone past $00 yet?
				
				lda copyme				; yes
				sta mainloop			; stop the copy by modifying the
				lda copyme+1			; nops above to a jmp
				sta mainloop+1			;
				lda copyme+2			;
				sta mainloop+2			;

copyme			jmp main				; no, back to the beginning of the loop

				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================
				
				.align $100
irq		        lda zpSCROLLHORIZOFF	; set horiz offset
				sta $d016				;

				lda #$80
lp       		.var *
				cmp $d012				; wait
				bne lp
				jsr scroll

				lda #$c8				; reset horiz offset
				sta $d016				;
	 
				rts						; back to main loop

scrinit  		lda #>centrescrolly		; textstart
				sta txtpos+2
				lda #<centrescrolly
				sta txtpos+1
				lda #$07				; init horiz offset for scroll
				sta zpSCROLLHORIZOFF
				rts

scroll   		lda zpSCROLLHORIZOFF	; update horiz offset
				sec						;
fscrspd  		sbc #$01				;
				and #$07				;
				sta zpSCROLLHORIZOFF	;
				bcc fver				;
				rts
		 
fver			lda line+1				; shift text to the left
				sta line				; unrolled = 2x+ faster than loop!
				lda line+2
				sta line+1
				lda line+3
				sta line+2
				lda line+4
				sta line+3
				lda line+5
				sta line+4
				lda line+6
				sta line+5
				lda line+7
				sta line+6
				lda line+8
				sta line+7
				lda line+9
				sta line+8
				lda line+$0a
				sta line+9
				lda line+$0b
				sta line+10
				lda line+$0c
				sta line+11
				lda line+$0d
				sta line+12
				lda line+$0e
				sta line+13
				lda line+$0f
				sta line+$0e
				lda line+$10
				sta line+$0f
				lda line+$11
				sta line+$10
				lda line+$12
				sta line+$11
				lda line+$13
				sta line+$12
				lda line+$14
				sta line+$13
				lda line+$15
				sta line+$14		 
				lda line+$16
				sta line+$15
				lda line+$17
				sta line+$16
				lda line+$18
				sta line+$17	 
				lda line+$19
				sta line+$18
				lda line+$1a
				sta line+$19
				lda line+$1b
				sta line+$1a
				lda line+$1c
				sta line+$1b
				lda line+$1d
				sta line+$1c
				lda line+$1e
				sta line+$1d
				lda line+$1f
				sta line+$1e
				lda line+$20
				sta line+$1f
				lda line+$21
				sta line+$20
				lda line+$22
				sta line+$21
				lda line+$23
				sta line+$22
				lda line+$24
				sta line+$23
				lda line+$25
				sta line+$24
				lda line+$26
				sta line+$25
				lda line+$27
				sta line+$26

txtpos  		lda $dead      			;
				cmp #$00       			; end of txt ?
				bne lp2        			; no !

				lda #>centrescrolly		; reset scroll text offset
				sta txtpos+2			;
				lda #<centrescrolly		;
				sta txtpos+1			;
				jmp txtpos

lp2   		   	sta line+$27			; 28th char of scroll row in RAM
				lda zpSCROLLHORIZOFF
				clc
				adc #$08
				and #$07
				sta zpSCROLLHORIZOFF

txtcount 		inc txtpos+1			; scroll left, move to next char
				bne ende				; overflow page?
				inc txtpos+2			; yes

ende		    rts

				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================

				.align $100
rastersync								; setup timing for
										; opening border
loopr1		    cpx $d012
				bne loopr1
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
loopr2			dey
				bne loopr2
				inx
				nop
				nop
         
				rts
				
				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================

				.align $100
colorsw			.byte $0b,$0c,$0f,$0d,$0d,$0f,$0c,$0b 		; grey/green
				.byte $0b,$0c,$0f,$01,$01,$0f,$0c,$0b 		; grey/white
				.byte $02,$04,$0a,$01,$01,$0a,$04,$02 		; red/purple
				.byte $06,$0e,$03,$01,$01,$03,$0e,$06		; blue/white
				.byte $09,$08,$07,$01,$01,$07,$08,$09		; yellow/white
				.byte $0b,$0c,$0f,$0d,$0d,$0f,$0c,$0b		; copy of first set, for nice cycling

spritex			.byte $4C,$00,$6D,$00,$8E,$00,$AF,$00,$D0,$00,$F1,$00,$12,$40

colorsrbtop		.byte  9, 8,10, 7,13, 3, 6
colorsrbbot		.byte  3,13, 7,10, 8, 9, 0

cat				.byte  0, 0, 0, 0, 0, 0, 0, 0
				.byte  0, 0, 0, 0, 0, 0, 9, 0
				.byte  0, 0, 0, 0, 0, 9, 8, 0 
				.byte  0, 0, 0, 0, 9, 8,10,11
				.byte  0, 0, 0, 9, 8,10, 7,11
				.byte  0, 0, 9, 8,10, 7,13, 6
				.byte  0, 9, 8,10, 7,13, 3, 6
				
fadeincolours   .byte 14,14,14,3,3,3,1
fadeoutcolours  .byte 1,3,3,3,14,14,14,6

				; greyscale
topcolour		.byte 11,11,12,12,15,15,01,01,15,15,12,12,11,11,12,12,15,15,01,01,15,15,12,12
				.byte 11,11,12,12,15,15,01,01,15,15,12,12,11,11,12,12,15,15,01,01,15,15,12,12,11,11
				; reds
;topcolour		.byte 09,09,02,02,10,10,01,01,10,10,02,02,09,09,02,02,10,10,01,01,10,10,02,02
;				.byte 09,09,02,02,10,10,01,01,10,10,02,02,09,09,02,02,10,10,01,01,10,10,02,02,09,09
			
scrollytext4	.byte $1c,$2b,$1f,$1c,$2b,$25,$3a
scrollytext4c1	.byte $0a,$0e,$0c,$0a,$0e,$0c,$08				; sprite colour 1 for this sprite
scrollytext4c2	.byte $02,$06,$0b,$02,$06,$0b,$08				; sprite colour 2 for this sprite
			
				.align $100
horizscroll		.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
				.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
				.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
				.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
				
				; generated by the code
spriteysinus1	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
spriteysinus2	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0				
				
spriteysinusoff	.byte  10, 12, 14, 16, 17, 19, 20, 20, 20, 20, 19, 17, 16, 14, 12
				.byte  10,  8,  6,  4,  3,  1,  0,  0,  0,  0,  1,  3,  4,  6,  8
				
spriteysinusof2 .byte   0,  0,  0,  0,  1,  3,  4,  6,  8, 10, 12, 14, 16, 17, 19
				.byte  20, 20, 20, 20, 19, 17, 16, 14, 12, 10,  8,  6,  4,  3,  1
				
				; ===========================================================================================================

centrescrolly 	.text 'AN UPDATE TO MY SECOND C64 DEMO IS HERE! ALL CODE AND BIG FONT BY ME, MUSIC BY ROB HUBBARD FROM '
				.text '"ONE MAN AND HIS DRIOD". GREETZ TO SARAH, ADAM, STEVE, STEVE, THE DEVELOPMENT CATS RUTHERFORD, FREEMAN, AND '
				.text 'MAXWELL, AND EVERYONE WHO STILL LOVES THE C64. DEDICATED TO JULIE, DYANNE, AND ADAM. WRITING SCROLL TEXT IS SO MUCH HARDER THAN CODING :(  '
				.byte $00	; always end with a zero

bitzON			.byte $01,$00,$02,$00,$04,$00,$08,$00,$10,$00,$20,$00,$40,$00,$80,$00
bitzOFF			.byte $fe,$00,$fd,$00,$fb,$00,$f7,$00,$ef,$00,$df,$00,$bf,$00,$7f,$00
				
				.enc "screen"
bottomscrolly	.text '        WELCOME TO MY LATEST DEMO, THIS IS ANOTHER UPDATE TO MY SECOND EVER DEMO AND INCLUDES '
				.text 'SPEED OPTIMISATIONS, THIS LOVELY BORDER-BASED SPRITE SCROLLER AND SOME OTHER MINOR UPDATES. '
				.text 'CHECK OUT THE WEBSITE - WWW.MAXIMUMOCTOPUS.COM/C64 - FOR MORE DEMOS AND C64 STUFF... '
				.text 'THIS DEMO IS COMPRISED OF 1238 LINES OF ASSEMBLER AND 648 LINES OF DATA. '
				.byte $00   ; always end with a zero
copyright		.text '      PAUL A FRESHNEY 2023      '
website			.text '     WWW.MAXIMUMOCTOPUS.COM     '
				.enc "none"
message1		.text '     FEBRUARY 27TH 2023     '
message2		.text '        FRESHNEY.ORG        '
message3		.text '   WWW.MAXIMUMOCTOPUS.COM   '
message4		.text ' GITHUB.COM/MAXIMUMOCTOPUS  '
message5		.text ' ARTSTATION.COM/MRFRESH3141 '
message6		.text '    !COMMODORE FOREVER!     '
message7		.text '         CATS RULE!         '
message8		.text '  HOPE YOU ARE HAVING FUN!  '
message9		.text '     THE CATS SAY HELLO     '
			
random			.byte $00,$1e,$01,$0e,$0c,$0a,$2e,$37,$3b,$1a,$07,$2c,$03,$23,$31,$18
                .byte $32,$04,$0f,$15,$06,$16,$3d,$08,$30,$38,$35,$09,$3c,$17,$0b,$1c
				.byte $1f,$3f,$2b,$22,$27,$20,$36,$2d,$2a,$21,$05,$24,$12,$14,$34,$28
				.byte $3e,$11,$29,$2f,$19,$0d,$13,$02,$1d,$33,$10,$39,$3a,$25,$1b,$26
				
textoffset		.byte $04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13
				.byte $14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23
				.byte $2c,$2d,$2e,$2f,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b
				.byte $3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b
				
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
				
				; ===========================================================================================================
		 
				*=MUSICLOAD
		 
				.binary "oneman.dat",2
				
				; ===========================================================================================================
		
				*=CHARRAM
		
				.binary "fonts\swiii17.64c",2
				
				; ===========================================================================================================
			
				*=VICBASE
				
				.include "include\spritefont_bas_relief_2_special.inc" ; 59 sprites
				
				; ===========================================================================================================
				
				*=$C000
				
				;M
				.byte 170,2,164,149,137,92,149,101,92,149,85,92,149,85,92,149,85,92,149,221
				.byte 92,151,50,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151
				.byte 2,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92
				.byte 127,1,252,0
				;A
				.byte 10,170,128,37,85,112,149,85,92,151,253,92,151,2,92,151,2,92,151,2
				.byte 92,151,2,92,151,2,92,151,170,92,149,85,92,149,85,92,151,253,92,151
				.byte 2,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92
				.byte 127,1,252,0
				;X
				.byte 169,2,164,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2
				.byte 92,151,2,92,55,2,112,13,169,192,3,87,0,2,86,0,9,253,128,39
				.byte 2,96,151,2,88,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92
				.byte 127,1,252,0
				;I
				.byte 170,170,164,149,85,92,149,85,92,191,223,252,0,156,0,0,156,0,0,156
				.byte 0,0,156,0,0,156,0,0,156,0,0,156,0,0,156,0,0,156,0,0
				.byte 156,0,0,156,0,0,156,0,0,156,0,170,158,164,149,85,92,149,85,92
				.byte 127,255,252,0
				;U
				.byte 169,2,164,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2
				.byte 92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151
				.byte 2,92,151,2,92,151,2,92,151,2,92,149,169,92,149,85,92,149,85,92
				.byte 63,255,240,0
				;O
				.byte 10,170,128,37,85,80,149,85,92,151,253,92,151,2,92,151,2,92,151,2
				.byte 92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151,2,92,151
				.byte 2,92,151,2,92,151,2,92,151,2,92,149,169,92,149,85,92,53,85,112
				.byte 15,255,192,0
				;C
				.byte 10,170,128,37,85,80,149,85,92,151,253,92,151,2,92,151,2,92,151,2
				.byte 92,151,2,252,151,0,0,151,0,0,151,0,0,151,0,0,151,0,0,151
				.byte 0,0,151,2,164,151,2,92,151,2,92,149,169,92,149,85,92,53,85,112
				.byte 15,255,192,0
				;T
				.byte 170,170,164,149,85,92,149,85,92,127,215,252,0,156,0,0,156,0,0,156
				.byte 0,0,156,0,0,156,0,0,156,0,0,156,0,0,156,0,0,156,0,0
				.byte 156,0,0,156,0,0,156,0,0,156,0,0,156,0,0,156,0,0,156,0
				.byte 0,124,0,0
				;P
				.byte 170,170,164,149,85,92,149,85,92,151,253,92,151,2,92,151,2,92,151,2
				.byte 92,151,2,92,151,2,92,149,170,92,149,85,92,149,85,92,151,255,252,151
				.byte 0,0,151,0,0,151,0,0,151,0,0,151,0,0,151,0,0,151,0,0
				.byte 127,0,0,0
				;S
				.byte 10,170,164,37,85,92,149,85,92,151,255,252,151,0,0,151,0,0,151,0
				.byte 0,151,0,0,151,0,0,149,170,164,149,85,92,149,85,92,127,253,92,0
				.byte 2,92,0,2,92,0,2,92,0,2,92,170,169,92,149,85,92,149,85,112
				.byte 127,255,192,0
				
				; ===========================================================================================================161