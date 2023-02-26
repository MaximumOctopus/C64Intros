; ==============================================================
; =                                                            =
; = 3x5 Scroll Demo 1                                          =
; =                                                            =
; = (c) Paul Alan Freshney 2014								   =
; =                                                            =
; = May 30th 2014											   =
; =                                                            =
; ==============================================================

				.cpu 6502
				
				; === constants =============================================================================================
				
				.include "include\colours.inc"
				
				; ==========================================================================================================

				VICBASE     = $8000
				SCREENRAM   = VICBASE   + $2000
				CHARRAM     = VICBASE   + $2800
				SPRITEPTR   = SCREENRAM + $03f8
				
				COLOURRAM   = $d800
				
				line1      	= SCREENRAM+$140		; screen RAM offset to beginning of scroll text
				line2      	= SCREENRAM+$168		; screen RAM offset to beginning of scroll text
				line3      	= SCREENRAM+$190		; screen RAM offset to beginning of scroll text
				line4      	= SCREENRAM+$1B8		; screen RAM offset to beginning of scroll text
				line5      	= SCREENRAM+$1E0		; screen RAM offset to beginning of scroll text
				
				line1cr    	= COLOURRAM+$140		; colour RAM offset to beginning of scroll text
				line2cr   	= COLOURRAM+$168		; colour RAM offset to beginning of scroll text
				line3cr    	= COLOURRAM+$190		; colour RAM offset to beginning of scroll text
				line4cr   	= COLOURRAM+$1B8		; colour RAM offset to beginning of scroll text
				line5cr    	= COLOURRAM+$1E0		; colour RAM offset to beginning of scroll text
				
				SCROLLSPEED = $01					; scroll speed of the bottom scroller
				
				MUSICLOAD   = $1300
				MUSICINIT   = $1300
				MUSICPLAY   = $1303
				
				; ==========================================================================================================

				*=$0801

				.word (+), 2005  		;pointer, line number
				.null $9e, ^start		;will be sys 16384
+				.word 0          		;basic line end

				; ==========================================================================================================

				*=$4000
				
start			sei           			; disable interrupts

				lda #$5b				; extended colour mode on
				sta $d011

				jsr scrollinit			; initialise scroll routine
				
				lda #$00
				tax
				tay
				jsr MUSICINIT			; initialise music
				
				; -----------------------------------------------------------------------------------------------------------
				
				ldy #$00
sinustable		lda #$32				; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus1,y		; store in table
				
				lda #$af				; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus2,y		; store in table
				
				iny

				cpy #30					; 30 values
				bne sinustable				

				; ==========================================================================================================

				lda #cBLACK
				sta $d022				; colour 1
				lda #cRED
				sta $d023				; colour 2
				lda #cBLUE
				sta $d024				; colour 3
				
				; ==========================================================================================================
		
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
				lda #$3d				; space character, $20 is inverse of this (in this font)
cls				sta SCREENRAM,x			; clear the screen		
				sta SCREENRAM+$100,x
				sta SCREENRAM+$200,x
				sta SCREENRAM+$300,x
				inx
				bne cls
				
				; ===========================================================================================================
				
initvars		lda #$00				;
				sta $10					; current char "bit" (0-3, 3 always clear)
				
				lda #$40				;
				sta $11					; selects text colour ($40, $80 or $c0)
				
				lda #$fa				; raster cycle delay
				sta $14					;
				
				lda #$00				; text horiz scroll table offset
				sta $1c
				lda #$04				; text horiz scroll timer
				sta $1d
				
				lda #$07				; initscroll
				sta $1e					;
				
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
				
				; ===========================================================================================================

				ldx #$00
textloop		lda copyright,x			; get char from RAM
				sta SCREENRAM+$370,x	; place it in to screen RAM

				lda website,x			; get char from RAM
				sta SCREENRAM+$398,x	; place it in to screen RAM
				
				lda #cBLACK
				sta COLOURRAM+$370,x
				sta COLOURRAM+$398,x
				
				inx
				
				cpx #40
				bne textloop
				
initfont		ldx #$00
fontloop		lda	charoff,x
				sta CHARRAM+(40*8),x
				
				lda charon,x
				sta CHARRAM+(41*8),x
				sta CHARRAM+(42*8),x
				sta CHARRAM+(43*8),x
				sta CHARRAM+(44*8),x
				
				inx
				
				cpx #$08
				bne fontloop
				
				; ===========================================================================================================
			
				ldx #$00
				ldy #$00
text			lda #$01				;
				sta	line1cr,x			; the location of the scrolly text
				sta	line2cr,x			; the location of the scrolly text
				sta	line3cr,x			; the location of the scrolly text
				sta	line4cr,x			; the location of the scrolly text
				sta	line5cr,x			; the location of the scrolly text
				
				inx		
				cpx	#40					; 40 chars in message
				bne text
				
				; ===========================================================================================================
			
sprite_init		lda	#$7f				;
				sta	$d015				; turn on first 7 sprites
				sta	$d01c				; multi-col for sprites 0-7
			
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

				sty $02,x
				tya
				ora #$10
				sta $03,x
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
			
				bit $d011 				; Wait for new frame
				bpl *-3					;
				bit $d011				;
				bmi *-3					;
				
				; ===========================================================================================================
				; == nice intro start =======================================================================================
				; ===========================================================================================================
				
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
				
				lda #cRED
				sta $d023				; colour 2
				
				; ===========================================================================================================
				
spr_maximum	    ldy #$00
sprite_mpx_1	ldx $03,y
				
				lda spriteysinus1,x

				sta	$d001,y				; y-coord

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

savex			cpx #29					; 30 y-coords
				beq sprite_mpx_1r
			
				inx
				jmp sprite_mpx_1c
				
sprite_mpx_1r	ldx #$00

sprite_mpx_1c	stx $03,y
			
				iny						; move to next sprite's y-coord
				iny
			
				cpy	#14
				bne	sprite_mpx_1
			
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
				
				ldy #$67    			; start raster line
				ldx #$01      			; offset to colour table
rasterbar1loop	lda rasterbarcols,x   	; 

				cpy $d012   			; wait for correct raster line
				bne *-3	    			; 

				sta $d020				; background colour
				sta $d021      			; border colour

				cpx #3        			; 8 colours
				beq textscroller		; 

				inx						; next colour
				iny            			; next raster line

				jmp rasterbar1loop		; jump to loop.	
				
				; ===========================================================================================================
				
textscroller	jsr irq

				; ===========================================================================================================

				ldy #$a2
				cpy $d012   			; wait for correct raster line
				bne *-3	    			; 
				
				ldy #$a4    			; start raster line
				ldx #$03      			; offset to colour table
rasterbar2loop	lda rasterbarcols,x   	; 

				cpy $d012   			; wait for correct raster line
				bne *-3	    			; 

				sta $d020				; background colour
				sta $d021      			; border colour

				dex	        			; next colour
				bmi spr_octopus			; jump when after the 0th value

										; next colour
				iny            			; next raster line

				jmp rasterbar2loop		; jump to loop.	
				
				; ===========================================================================================================
				
spr_octopus	    ldy #$00
sprite_mpx_2	ldx $02,y
				
				lda spriteysinus2,x

				sta	$d001,y				; y-coord

				lda spritex,y			; load the base x-coord
				clc						
				adc spriteysinusof2,x	; add an offset (sine)
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

savexb			cpx #29					; 30 y-coords
				beq sprite_mpx_2r
			
				inx
				jmp sprite_mpx_2c
				
sprite_mpx_2r	ldx #$00

sprite_mpx_2c	stx $02,y
			
				iny						; move to next sprite's y-coord
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

bottomraster	ldx $1c					; current x-offset
				
				dec $1d					; timer delay
				bne	textraster			; ready for new value
			
				lda #$04				; yes, reset delay
				sta $1d					;

				ldx $1c					; load table offset
				inx						; move to next value
				cpx #20					; last one?
				bne savehoriz			; no
				
				ldx #$00				; yes, reset to first item
				
savehoriz		stx $1c					; save offset to table

textraster		stx horioffset+1		; needed for update mechanism (below)
			
				; ===========================================================================================================

				ldy #$e2    			; raster line to start at
				ldx #$30      			; index to colour table
				stx coloffset+1			; reset offset
				ldx #$38
				stx coloffset2+1		; reset offset
loop2  			

horioffset		ldx horizscroll			; offset to scroll table
coloffset		lda $30					; offset to colour table

				cpy $d012   			; reach raster line yet?
				bne *-3	    			; 

				stx $d016				; alter the horiz scroll register	
				sta $d022      			; background colour (foreground of text!)
coloffset2		lda $30					; offset to colour table
				sta $d023      			; background colour (foreground of text!)

				cpy #$f2        		; number of bars
				beq raster3cycle   		; 
				
				inc	horioffset+1		; move to next value in table
				inc coloffset+1			; move to next value in table
				dec coloffset2+1			; move to next value in table

				iny            			; make each raster bar 2 pixels (rows) high
				iny

				jmp loop2      			; keep rastering
				
				; ===========================================================================================================
				
raster3cycle	inc $14					; cycle delay

				bne gomain

				lda $1f
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
				
updateoffset	stx $1f
		
				lda #$fb				; reset delay
				sta $14
				
				; ===========================================================================================================

gomain			jmp main
				
				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================
				
				.align $100
irq		        lda $1e					; set horiz offset
				sta $d016				;

				lda #$80
lp       		.var *
				cmp $d012				; wait
				bne lp
				jsr scroll
				
				lda #$9f
				cmp $d012				; wait
				bne *-3

				lda #$c8				; reset horiz offset
				sta $d016				;
				
				rts						; back to main loop

scrollinit 		lda #>centrescrolly		; textstart
				sta txtpos+2
				lda #<centrescrolly
				sta txtpos+1
				lda #$07				; init horiz offset for scroll
				sta $1e
				rts

scroll   		lda $1e					; update horiz offset
				sec						;
speed	  		sbc #$02				; speed of the scroll
				and #$07				;
				sta $1e					;
				bcc charcopy			;
				rts
		 
charcopy		lda line1+1				; shift text to the left
				sta line1				; unrolled = 2x+ faster than loop!
				lda line1+2
				sta line1+1
				lda line1+3
				sta line1+2
				lda line1+4
				sta line1+3
				lda line1+5
				sta line1+4
				lda line1+6
				sta line1+5
				lda line1+7
				sta line1+6
				lda line1+8
				sta line1+7
				lda line1+9
				sta line1+8
				lda line1+$0a
				sta line1+9
				lda line1+$0b
				sta line1+10
				lda line1+$0c
				sta line1+11
				lda line1+$0d
				sta line1+12
				lda line1+$0e
				sta line1+13
				lda line1+$0f
				sta line1+$0e
				lda line1+$10
				sta line1+$0f
				lda line1+$11
				sta line1+$10
				lda line1+$12
				sta line1+$11
				lda line1+$13
				sta line1+$12
				lda line1+$14
				sta line1+$13
				lda line1+$15
				sta line1+$14		 
				lda line1+$16
				sta line1+$15
				lda line1+$17
				sta line1+$16
				lda line1+$18
				sta line1+$17	 
				lda line1+$19
				sta line1+$18
				lda line1+$1a
				sta line1+$19
				lda line1+$1b
				sta line1+$1a
				lda line1+$1c
				sta line1+$1b
				lda line1+$1d
				sta line1+$1c
				lda line1+$1e
				sta line1+$1d
				lda line1+$1f
				sta line1+$1e
				lda line1+$20
				sta line1+$1f
				lda line1+$21
				sta line1+$20
				lda line1+$22
				sta line1+$21
				lda line1+$23
				sta line1+$22
				lda line1+$24
				sta line1+$23
				lda line1+$25
				sta line1+$24
				lda line1+$26
				sta line1+$25
				lda line1+$27
				sta line1+$26
				
				lda line2+1				; shift text to the left
				sta line2				; unrolled = 2x+ faster than loop!
				lda line2+2
				sta line2+1
				lda line2+3
				sta line2+2
				lda line2+4
				sta line2+3
				lda line2+5
				sta line2+4
				lda line2+6
				sta line2+5
				lda line2+7
				sta line2+6
				lda line2+8
				sta line2+7
				lda line2+9
				sta line2+8
				lda line2+$0a
				sta line2+9
				lda line2+$0b
				sta line2+10
				lda line2+$0c
				sta line2+11
				lda line2+$0d
				sta line2+12
				lda line2+$0e
				sta line2+13
				lda line2+$0f
				sta line2+$0e
				lda line2+$10
				sta line2+$0f
				lda line2+$11
				sta line2+$10
				lda line2+$12
				sta line2+$11
				lda line2+$13
				sta line2+$12
				lda line2+$14
				sta line2+$13
				lda line2+$15
				sta line2+$14		 
				lda line2+$16
				sta line2+$15
				lda line2+$17
				sta line2+$16
				lda line2+$18
				sta line2+$17	 
				lda line2+$19
				sta line2+$18
				lda line2+$1a
				sta line2+$19
				lda line2+$1b
				sta line2+$1a
				lda line2+$1c
				sta line2+$1b
				lda line2+$1d
				sta line2+$1c
				lda line2+$1e
				sta line2+$1d
				lda line2+$1f
				sta line2+$1e
				lda line2+$20
				sta line2+$1f
				lda line2+$21
				sta line2+$20
				lda line2+$22
				sta line2+$21
				lda line2+$23
				sta line2+$22
				lda line2+$24
				sta line2+$23
				lda line2+$25
				sta line2+$24
				lda line2+$26
				sta line2+$25
				lda line2+$27
				sta line2+$26
				
				lda line3+1				; shift text to the left
				sta line3				; unrolled = 2x+ faster than loop!
				lda line3+2
				sta line3+1
				lda line3+3
				sta line3+2
				lda line3+4
				sta line3+3
				lda line3+5
				sta line3+4
				lda line3+6
				sta line3+5
				lda line3+7
				sta line3+6
				lda line3+8
				sta line3+7
				lda line3+9
				sta line3+8
				lda line3+$0a
				sta line3+9
				lda line3+$0b
				sta line3+10
				lda line3+$0c
				sta line3+11
				lda line3+$0d
				sta line3+12
				lda line3+$0e
				sta line3+13
				lda line3+$0f
				sta line3+$0e
				lda line3+$10
				sta line3+$0f
				lda line3+$11
				sta line3+$10
				lda line3+$12
				sta line3+$11
				lda line3+$13
				sta line3+$12
				lda line3+$14
				sta line3+$13
				lda line3+$15
				sta line3+$14		 
				lda line3+$16
				sta line3+$15
				lda line3+$17
				sta line3+$16
				lda line3+$18
				sta line3+$17	 
				lda line3+$19
				sta line3+$18
				lda line3+$1a
				sta line3+$19
				lda line3+$1b
				sta line3+$1a
				lda line3+$1c
				sta line3+$1b
				lda line3+$1d
				sta line3+$1c
				lda line3+$1e
				sta line3+$1d
				lda line3+$1f
				sta line3+$1e
				lda line3+$20
				sta line3+$1f
				lda line3+$21
				sta line3+$20
				lda line3+$22
				sta line3+$21
				lda line3+$23
				sta line3+$22
				lda line3+$24
				sta line3+$23
				lda line3+$25
				sta line3+$24
				lda line3+$26
				sta line3+$25
				lda line3+$27
				sta line3+$26
				
				lda line4+1				; shift text to the left
				sta line4				; unrolled = 2x+ faster than loop!
				lda line4+2
				sta line4+1
				lda line4+3
				sta line4+2
				lda line4+4
				sta line4+3
				lda line4+5
				sta line4+4
				lda line4+6
				sta line4+5
				lda line4+7
				sta line4+6
				lda line4+8
				sta line4+7
				lda line4+9
				sta line4+8
				lda line4+$0a
				sta line4+9
				lda line4+$0b
				sta line4+10
				lda line4+$0c
				sta line4+11
				lda line4+$0d
				sta line4+12
				lda line4+$0e
				sta line4+13
				lda line4+$0f
				sta line4+$0e
				lda line4+$10
				sta line4+$0f
				lda line4+$11
				sta line4+$10
				lda line4+$12
				sta line4+$11
				lda line4+$13
				sta line4+$12
				lda line4+$14
				sta line4+$13
				lda line4+$15
				sta line4+$14		 
				lda line4+$16
				sta line4+$15
				lda line4+$17
				sta line4+$16
				lda line4+$18
				sta line4+$17	 
				lda line4+$19
				sta line4+$18
				lda line4+$1a
				sta line4+$19
				lda line4+$1b
				sta line4+$1a
				lda line4+$1c
				sta line4+$1b
				lda line4+$1d
				sta line4+$1c
				lda line4+$1e
				sta line4+$1d
				lda line4+$1f
				sta line4+$1e
				lda line4+$20
				sta line4+$1f
				lda line4+$21
				sta line4+$20
				lda line4+$22
				sta line4+$21
				lda line4+$23
				sta line4+$22
				lda line4+$24
				sta line4+$23
				lda line4+$25
				sta line4+$24
				lda line4+$26
				sta line4+$25
				lda line4+$27
				sta line4+$26
				
				lda line5+1				; shift text to the left
				sta line5				; unrolled = 2x+ faster than loop!
				lda line5+2
				sta line5+1
				lda line5+3
				sta line5+2
				lda line5+4
				sta line5+3
				lda line5+5
				sta line5+4
				lda line5+6
				sta line5+5
				lda line5+7
				sta line5+6
				lda line5+8
				sta line5+7
				lda line5+9
				sta line5+8
				lda line5+$0a
				sta line5+9
				lda line5+$0b
				sta line5+10
				lda line5+$0c
				sta line5+11
				lda line5+$0d
				sta line5+12
				lda line5+$0e
				sta line5+13
				lda line5+$0f
				sta line5+$0e
				lda line5+$10
				sta line5+$0f
				lda line5+$11
				sta line5+$10
				lda line5+$12
				sta line5+$11
				lda line5+$13
				sta line5+$12
				lda line5+$14
				sta line5+$13
				lda line5+$15
				sta line5+$14		 
				lda line5+$16
				sta line5+$15
				lda line5+$17
				sta line5+$16
				lda line5+$18
				sta line5+$17	 
				lda line5+$19
				sta line5+$18
				lda line5+$1a
				sta line5+$19
				lda line5+$1b
				sta line5+$1a
				lda line5+$1c
				sta line5+$1b
				lda line5+$1d
				sta line5+$1c
				lda line5+$1e
				sta line5+$1d
				lda line5+$1f
				sta line5+$1e
				lda line5+$20
				sta line5+$1f
				lda line5+$21
				sta line5+$20
				lda line5+$22
				sta line5+$21
				lda line5+$23
				sta line5+$22
				lda line5+$24
				sta line5+$23
				lda line5+$25
				sta line5+$24
				lda line5+$26
				sta line5+$25
				lda line5+$27
				sta line5+$26

txtpos  		ldx $dead      			; modified by code
				cpx	#$40
				beq newcolour
				cpx #$80
				beq newcolour
				cpx #$c0
				bne	isend
				
newcolour		stx	textcolour+1		; set new colour mask
				
				inc txtpos+1			; scroll left, move to next char
				bne txtpos				; overflow page?
				inc txtpos+2			; yes
				
				jmp txtpos

isend			cpx #$00       			; end of txt ?
				bne lp2        			; no !

				ldy #>centrescrolly		; reset scroll text offset
				sty txtpos+2			;
				ldy #<centrescrolly		;
				sty txtpos+1			;
				jmp txtpos

lp2   		   	ldy $10
				cpy	#03
				beq nextchar
				
textcolour		lda #$40				; modified by code, determines colour
				sta $11					; of the next character
				
				lda font3x5r1,x
				and powers,y
				ora $11
				sta line1+$27
				lda font3x5r2,x
				and powers,y
				ora $11
				sta line2+$27
				lda font3x5r3,x
				and powers,y
				ora $11
				sta line3+$27
				lda font3x5r4,x
				and powers,y
				ora $11
				sta line4+$27
				lda font3x5r5,x
				and powers,y
				ora $11
				sta line5+$27
				
				inc $10
				
				jmp cont3
				
nextchar		lda #$00				; 1 "pixel" space between characters
				sta line1+$27			; 39th char of scroll row 1 in RAM
				sta line2+$27			; 39th char of scroll row 2 in RAM
				sta line3+$27			; 39th char of scroll row 3 in RAM
				sta line4+$27			; 39th char of scroll row 4 in RAM
				sta line5+$27			; 39th char of scroll row 5 in RAM
	
				sta $10					; start from column 1 of the char data
				
txtcount 		inc txtpos+1			; scroll left, move to next char
				bne cont3				; overflow page?
				inc txtpos+2			; yes
				
cont3			lda $1e
				clc
				adc #$08
				and #$07
				sta $1e

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
rasterbarcols	.byte cBLACK,cMEDIUMGREY,cLIGHTERGREY,cWHITE
				      
powers			.byte $2c,$2a,$29
				.align $100
colorsw			.byte $0b,$0c,$0f,$0d,$0d,$0f,$0c,$0b 		; grey/green
				.byte $0b,$0c,$0f,$01,$01,$0f,$0c,$0b 		; grey/white
				.byte $02,$04,$0a,$01,$01,$0a,$04,$02 		; red/purple
				.byte $06,$0e,$03,$01,$01,$03,$0e,$06		; blue/white
				.byte $09,$08,$07,$01,$01,$07,$08,$09		; yellow/white
				.byte $0b,$0c,$0f,$0d,$0d,$0f,$0c,$0b		; copy of first set, for nice cycling
				
spritex			.byte $4C,$00,$6D,$00,$8E,$00,$AF,$00,$D0,$00,$F1,$00,$12,$40

bitzON			.byte $01,$00,$02,$00,$04,$00,$08,$00,$10,$00,$20,$00,$40,$00,$80,$00
bitzOFF			.byte $fe,$00,$fd,$00,$fb,$00,$f7,$00,$ef,$00,$df,$00,$bf,$00,$7f,$00
				
				; ===========================================================================================================
				; $40 = colour 1
				; $80 = colour 2
				; $c0 = colour 3
				.enc screen
centrescrolly 	.text 'WELCOME TO MY LATEST C64 INTRO! ',$c0,'JUNE ',$80,'1ST ',$40,'2014. CODE BY ME, MUSIC BY MARK "TDK" KNIGHT, CALLED '
				.text '"ORGASMIC CHIPPER". GREETZ TO DAN, AMY, TRACY, SARAH, THE ',$80,'DEVELOPMENT CATS',$c0,' RUTHERFORD AND FREEMAN',$40,', STEVE, STEVE, '
				.text 'ADAM, EUAN, KRUPA, ANNA, KAREN, CAROLINE, KATIE, JANINE, CELIA, RAF, NICK, SIMON AND COMMODORE FANS EVERYWHERE! '
				.text 'CHECK OUT ',$80,'WWW.MAXIMUMOCTOPUS.COM/C64',$40,' FOR MORE C64 STUFF...   '
				.byte $00	; always end with a zero
				.enc none
				
charoff			.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
charon			.byte $01,$01,$01,$01,$01,$01,$01,$ff
				
website			.byte $40,$40,$40,$40,$40,$40,$40,$40,$bc,$83,$be,$40,$50,$41,$55,$4C,$40,$41,$40,$46,$52,$45,$53,$48,$4E,$45,$59,$40,$b2,$b0,$b1,$b4,$40,$40,$40,$40,$40,$40,$40,$40
copyright		.byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$57,$57,$57,$6E,$8D,$81,$98,$89,$8D,$95,$8D,$8F,$83,$94,$8F,$90,$95,$93,$6E,$43,$4F,$4D,$40,$40,$40,$40,$40,$40,$40,$40,$40
				
				.align $100
horizscroll		.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
				.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
				.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
				.byte $c0,$c0,$c1,$c1,$c2,$c3,$c4,$c5,$c6,$c6,$c7,$c7,$c6,$c6,$c5,$c4,$c3,$c2,$c1,$c1
				
spriteysinus1	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
spriteysinus2	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0				
				
spriteysinusoff	.byte  10, 12, 14, 16, 17, 19, 20, 20, 20, 20, 19, 17, 16, 14, 12
				.byte  10,  8,  6,  4,  3,  1,  0,  0,  0,  0,  1,  3,  4,  6,  8
				
spriteysinusof2 .byte   0,  0,  0,  0,  1,  3,  4,  6,  8, 10, 12, 14, 16, 17, 19
				.byte  20, 20, 20, 20, 19, 17, 16, 14, 12, 10,  8,  6,  4,  3,  1

				; ===========================================================================================================

				.include "fonts\3x5_paf.inc"
				
				; ===========================================================================================================
		 
				*=MUSICLOAD
		 
				.binary "music\$1300\orgasmic_chipper.dat",2
				
				; ===========================================================================================================
		
				*=CHARRAM
		
				.binary "fonts\aeg_collection_10.64c",2
				
				; ===========================================================================================================
			
				*=VICBASE
				
				.include "include\spritefont_bas_relief_2.inc" ; 59 sprites
				
				; ===========================================================================================================
