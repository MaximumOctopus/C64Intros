; ==============================================================
; =                                                            =
; = Cubes Demo                                                 =
; =                                                            =
; = (c) Paul Alan Freshney 2023		     					   =
; =                                                            =
; = https://github.com/MaximumOctopus/C64Intros                =
; =                                                            =
; = Febraury 26th 2023									       =
; =                                                            =
; ==============================================================

				;.cpu "6502"
				
				; === constants =============================================================================================
				
				.include "include\colours.inc"
				
				; ==========================================================================================================

				VICBASE      = $8000
				SCREENRAM    = VICBASE   + $2000
				CHARRAM      = VICBASE   + $2800
				SPRITEPTR    = SCREENRAM + $03f8
				
				line      	 = SCREENRAM + $140		; screen RAM offset to beginning of scroll text
				
				SCROLLSPEED  = $01					; scroll speed of the bottom scroller
				
				; ==========================================================================================================
				
				MUSICLOAD    		= $1000
				MUSICINIT   		= $1000
				MUSICPLAY    		= $1003
				
				ROW1TOP             = $20
				ROW2TOP             = ROW1TOP + $2c
				ROW3TOP             = ROW2TOP + $2d
				ROW4TOP             = ROW3TOP + $2d
				ROW5TOP             = ROW4TOP + $2d
				
				CHANGE_ROW1TOP      = ROW1TOP - 10
				CHANGE_ROW2TOP      = ROW2TOP - 10
				CHANGE_ROW3TOP      = ROW3TOP - 10
				CHANGE_ROW4TOP      = ROW4TOP - 10
				CHANGE_ROW5TOP      = ROW5TOP - 10
				
				; ==========================================================================================================
				
				zpSPRITEROW1_YDATA  = $03 ; (03 - 0a) (odd bytes)
				zpSPRITEROW1_XDATA  = $0b ; (0b - 13) (even bytes)
				
				zpSPRITEROW2_YDATA  = $13 ; (13 - 1a) (even bytes)
				zpSPRITEROW2_XDATA  = $1b ; (1b - 22) (odd bytes)
				
				zpSPRITEROW3_YDATA  = $23 ; (23 - 2a) (even bytes)
				zpSPRITEROW3_XDATA  = $2b ; (2b - 32) (odd bytes)				
				
				zpSPRITEROW4_YDATA  = $33 ; (33 - 3a) (even bytes)
				zpSPRITEROW4_XDATA  = $3b ; (3b - 42) (odd bytes)
				
				zpSPRITEROW5_YDATA  = $43 ; (43 - 4a) (even bytes)
				zpSPRITEROW5_XDATA  = $4b ; (4b - 52) (odd bytes)
				
				zpSPRITEROW1_ABOVE  = $53 ;
				zpSPRITEROWX_DELAY  = $54 ;
				
				zpLINECYCLE_EVNCOL  = $55;
				zpLINECYCLE_EVNOFF  = $56;				
				zpLINECYCLE_ODDCOL  = $57;
				zpLINECYCLE_ODDOFF  = $58;

				zpBOTTOMSCROLLYOFF	= $60
				zpBOTTOMSCROLLYSPD  = $61
				
				zpSPRITEYTIMER		= $62
			
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
				
				
initbottoms		lda #>bottomscrolly+7	; update the address of the next sprite image
				sta sprite7xx+2			; in code
				lda #<bottomscrolly+7	;
				sta sprite7xx+1
				
				; ==========================================================================================================
				
				ldy #$00
sinustable		lda #ROW1TOP			; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus1,y		; store in table
				
				lda #ROW2TOP			; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus2,y		; store in table
				
				lda #ROW3TOP			; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus3,y		; store in table	

				lda #ROW4TOP			; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus4,y		; store in table
				
				lda #ROW5TOP			; lowest y value
				clc
				adc	spriteysinusoff,y	; add offset to base value
				sta spriteysinus5,y		; store in table
				
				iny

				cpy #30					; 30 values
				bne sinustable		
				
				; ==========================================================================================================

				ldx #00
				ldy #00
table_sprite1x	lda spritex
				clc
				adc	spriteysinusoff,x
				sta	sprite_sinex0,y
				
				lda spritex+2
				clc
				adc	spriteysinusoff,x
				sta	sprite_sinex1,y
				
				lda spritex+4
				clc
				adc	spriteysinusoff,x
				sta	sprite_sinex2,y

				lda spritex+6
				clc
				adc	spriteysinusoff,x
				sta	sprite_sinex3,y
				
				lda spritex+8
				clc
				adc	spriteysinusoff,x
				sta	sprite_sinex4,y	

				lda spritex+10
				clc
				adc	spriteysinusoff,x
				sta	sprite_sinex5,y	
				
				lda spritex+14
				clc
				adc	spriteysinusoff,x
				sta	sprite_sinex7,y
				
				iny
				iny
				inx
				
				cpy #60
				bne	table_sprite1x
				
				ldx #00
				ldy #00
table_sprite6x	lda spritex+12
				clc
				adc	spriteysinusoff,x
				sta	sprite_sinex6,y
				
				bmi	tcont				; did the value underflow?

tsetbit9b 		lda #$40				; set this sprite's 9th x-coord bit (#$40 = bit 6 high x-coord bit)

				iny
				
				sta	sprite_sinex6,y
				
				jmp twoiny
				
tcont			iny
twoiny			iny
				inx
				
				cpy #60
				bne	table_sprite6x

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
		
				ldx #$00				; char colour/shape offset
cls				lda #$20				; "space"
				sta SCREENRAM,x			; clear the screen		
				sta SCREENRAM+$100,x
				sta SCREENRAM+$200,x
				sta SCREENRAM+$300,x

				lda #cBLACK				;
				sta $d800,x				; colour RAM whole screen
				sta $d900,x				;
				sta $da00,x				;
				sta $db00,x				;
				
				inx
				bne cls
				
				jsr	drawscreen			; draw the 12 lines
				
textrasterinit	lda #$00
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
			
				cpx	#8
				bne	shapeloop

				lda #$0f
				sta $d025				; sprite colour 1
				lda #$0c
				sta $d026				; sprite colour 2
				lda #$0c
				sta $d026				; sprite colour 2
						
				; ===========================================================================================================
				
				lda #$00
				ldx #$00
				ldy #$00				

yloop			txa
				
				sta zpSPRITEROW1_YDATA,y
				
				clc
				adc	#$02
				
				cmp #30					; last co-ordinate
				
				bmi	ynext1				; did the value underflow?				
				
				sec
				sbc #29
					
ynext1			sta zpSPRITEROW2_YDATA,y
				
				clc
				adc	#$02
				
				cmp #30				; last co-ordinate
				
				bmi	ynext2				; did the value underflow?				
				
				sec
				sbc #29				
							
ynext2			sta zpSPRITEROW3_YDATA,y
				
				clc
				adc	#$02
				
				cmp #30					; last co-ordinate
				
				bmi	ynext3				; did the value underflow?				
				
				sec
				sbc #29			
				
ynext3			sta zpSPRITEROW4_YDATA,y
				
				clc
				adc	#$02
				
				cmp #30					; last co-ordinate
				
				bmi	ynext4				; did the value underflow?				
				
				sec
				sbc #29
				
ynext4			sta zpSPRITEROW5_YDATA,y
				
				clc
				adc	#$02
				
				cmp #30					; last co-ordinate
				
				bmi	ynext5				; did the value underflow?				
				
				sec
				sbc #69				

ynext5			iny				
				inx
				inx
				inx
				inx
				
				cpy #8
				bne yloop
				
				; ===========================================================================================================				
				
				lda #$00
				ldx #$00
				ldy #$00				

xloop			txa
				
				sta zpSPRITEROW1_XDATA,y
				
				clc
				adc	#$02
				
				cmp #60					; last co-ordinate
				
				bmi	xnext1				; did the value underflow?				
				
				sec
				sbc #59
					
xnext1			sta zpSPRITEROW2_XDATA,y
				
				clc
				adc	#$02
				
				cmp #60				; last co-ordinate
				
				bmi	xnext2				; did the value underflow?				
				
				sec
				sbc #59				
							
xnext2			sta zpSPRITEROW3_XDATA,y
				
				clc
				adc	#$02
				
				cmp #60					; last co-ordinate
				
				bmi	xnext3				; did the value underflow?				
				
				sec
				sbc #59			
				
xnext3			sta zpSPRITEROW4_XDATA,y
				
				clc
				adc	#$02
				
				cmp #60					; last co-ordinate
				
				bmi	xnext4				; did the value underflow?				
				
				sec
				sbc #59
				
xnext4			sta zpSPRITEROW5_XDATA,y
				
				clc
				adc	#$02
				
				cmp #60					; last co-ordinate
				
				bmi	xnext5				; did the value underflow?				
				
				sec
				sbc #59				

xnext5			iny				
				inx
				inx
				inx
				inx
				
				cpy #8
				bne xloop				
							
				; ===========================================================================================================
				
				bit $d011 				; Wait for new frame
				bpl *-3					;
				bit $d011				;
				bmi *-3					;
				
				; ===========================================================================================================				
				
maininit		lda	#$ff				; 
				sta	$d015				; activate sprites 0-7
				lda	#$ff				; 
				sta	$d01c				; multi-col for sprites 0-7
				
				lda #$00
				sta zpLINECYCLE_EVNCOL
				lda #$05
				sta zpLINECYCLE_EVNOFF
				
				lda	#$13				; start half-way along table
				sta zpLINECYCLE_ODDCOL
				
				lda #$24
				sta zpLINECYCLE_ODDOFF
				
				lda #$08
				sta zpSPRITEROWX_DELAY
				
				lda #$aa
				sta	zpSPRITEROW1_ABOVE

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
			
				lda #$00
				sta $d01d 				; no stretching horiz
			
				ldx #$00
				stx $d010				; clear the x-coord 9th bit

				; ===========================================================================================================				
				
				dec zpBOTTOMSCROLLYSPD	; scroll speed
				bne sprite_rows_begin
				
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
				; ==========================================================================================================

sprite_rows_begin
				
				ldx #$1e				; cube (sprite 30)
sprite_mpx_1b	stx	SPRITEPTR			; sprite shape (base VIC bank address + $07F8 + sprite # (0-6))
				stx	SPRITEPTR+1			; 
				stx	SPRITEPTR+2			; 
				stx	SPRITEPTR+3			; 
				stx	SPRITEPTR+4			; 
				stx	SPRITEPTR+5			; 
				stx	SPRITEPTR+6			;
				stx	SPRITEPTR+7			;
				
				; ==========================================================================================================
				; ==========================================================================================================

sprite_priority	dec zpSPRITEROWX_DELAY
				bne	animevenlines

				lda #$18
				sta zpSPRITEROWX_DELAY

				lda zpSPRITEROW1_ABOVE
				asl
				bcc	row1s0pu

				ora #$01

row1s0pu		sta	zpSPRITEROW1_ABOVE

				sta $d01b
				
				; ==========================================================================================================
				
animevenlines	jsr subanimeven
				
				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 1 - Sprite 0
				; ==========================================================================================================
				; ==========================================================================================================

row1s0			ldx zpSPRITEROW1_YDATA
				lda spriteysinus1,x
				sta	$d001
				
				cpx #29
				beq	row1s0_ryc

				inx
				jmp row1s0_uyc

row1s0_ryc		ldx #00

row1s0_uyc		stx zpSPRITEROW1_YDATA

				ldx zpSPRITEROW1_XDATA
				
				lda	sprite_sinex0,x
				
				sta	$d000
				
row1s0_xtimer	lda zpSPRITEYTIMER
				bne row1s1
				
				inx
				inx
				cpx	#60
				bne	row1s0_uxc
				
				ldx #$00
				
row1s0_uxc		stx zpSPRITEROW1_XDATA

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 1 - Sprite 1
				; ==========================================================================================================
				; ==========================================================================================================
				
row1s1			ldx zpSPRITEROW1_YDATA+1
				lda spriteysinus1,x
				sta	$d003
				
				cpx #29
				beq	row1s1_ryc

				inx
				jmp row1s1_uyc

row1s1_ryc		ldx #00

row1s1_uyc		stx zpSPRITEROW1_YDATA+1

				ldx zpSPRITEROW1_XDATA+1
				
				lda	sprite_sinex1,x
				
				sta	$d002
				
row1s1_xtimer	lda zpSPRITEYTIMER
				bne row1s2
				
				inx
				inx
				cpx	#60
				bne	row1s1_uxc
				
				ldx #$00
				
row1s1_uxc		stx zpSPRITEROW1_XDATA+1

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 1 - Sprite 2
				; ==========================================================================================================
				; ==========================================================================================================
				
row1s2			ldx zpSPRITEROW1_YDATA+2
				lda spriteysinus1,x
				sta	$d005
				
				cpx #29
				beq	row1s2_ryc

				inx
				jmp row1s2_uyc

row1s2_ryc		ldx #00

row1s2_uyc		stx zpSPRITEROW1_YDATA+2

				ldx zpSPRITEROW1_XDATA+2
				
				lda	sprite_sinex2,x
				
				sta	$d004
				
row1s2_xtimer	lda zpSPRITEYTIMER
				bne row1s3
				
				inx
				inx
				cpx	#60
				bne	row1s2_uxc
				
				ldx #$00
				
row1s2_uxc		stx zpSPRITEROW1_XDATA+2

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 1 - Sprite 3
				; ==========================================================================================================
				; ==========================================================================================================
				
row1s3			ldx zpSPRITEROW1_YDATA+3
				lda spriteysinus1,x
				sta	$d007
				
				cpx #29
				beq	row1s3_ryc

				inx
				jmp row1s3_uyc

row1s3_ryc		ldx #00

row1s3_uyc		stx zpSPRITEROW1_YDATA+3

				ldx zpSPRITEROW1_XDATA+3
				
				lda	sprite_sinex3,x
				
				sta	$d006
				
row1s3_xtimer	lda zpSPRITEYTIMER
				bne row1s4
				
				inx
				inx
				cpx	#60
				bne	row1s3_uxc
				
				ldx #$00
				
row1s3_uxc		stx zpSPRITEROW1_XDATA+3

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 1 - Sprite 4
				; ==========================================================================================================
				; ==========================================================================================================
				
row1s4			ldx zpSPRITEROW1_YDATA+4
				lda spriteysinus1,x
				sta	$d009
				
				cpx #29
				beq	row1s4_ryc

				inx
				jmp row1s4_uyc

row1s4_ryc		ldx #00

row1s4_uyc		stx zpSPRITEROW1_YDATA+4

				ldx zpSPRITEROW1_XDATA+4
				
				lda	sprite_sinex4,x
				
				sta	$d008
				
row1s4_xtimer	lda zpSPRITEYTIMER
				bne row1s5
				
				inx
				inx
				cpx	#60
				bne	row1s4_uxc
				
				ldx #$00
				
row1s4_uxc		stx zpSPRITEROW1_XDATA+4

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 1 - Sprite 5
				; ==========================================================================================================
				; ==========================================================================================================
				
row1s5			ldx zpSPRITEROW1_YDATA+5
				lda spriteysinus1,x
				sta	$d00b
				
				cpx #29
				beq	row1s5_ryc

				inx
				jmp row1s5_uyc

row1s5_ryc		ldx #00

row1s5_uyc		stx zpSPRITEROW1_YDATA+5

				ldx zpSPRITEROW1_XDATA+5
				
				lda	sprite_sinex5,x
				
				sta	$d00a
				
row1s5_xtimer	lda zpSPRITEYTIMER
				bne row1s6
				
				inx
				inx	
				cpx	#60
				bne	row1s5_uxc
				
				ldx #$00
				
row1s5_uxc		stx zpSPRITEROW1_XDATA+5

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 1 - Sprite 6
				; ==========================================================================================================
				; ==========================================================================================================
				
row1s6			ldx zpSPRITEROW1_YDATA+6
				lda spriteysinus1,x
				sta	$d00d
				
				cpx #29
				beq	row1s6_ryc

				inx
				jmp row1s6_uyc

row1s6_ryc		ldx #00

row1s6_uyc		stx zpSPRITEROW1_YDATA+6

				ldx zpSPRITEROW1_XDATA+6
				
				lda	sprite_sinex6,x
				
				sta	$d00c
				
				inx
				
				lda sprite_sinex6,x		; set this sprite's 9th x-coord bit
				
				ora $d010				;
				sta $d010					

row1s6_xcont	
				
row1s6_xtimer	lda zpSPRITEYTIMER
				bne row1s7
				
				inx
				cpx	#60
				bne	row1s6_uxc
				
				ldx #$00
				
row1s6_uxc		stx zpSPRITEROW1_XDATA+6

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 1 - Sprite 7
				; ==========================================================================================================
				; ==========================================================================================================
				
row1s7			ldx zpSPRITEROW1_YDATA+7
				lda spriteysinus1,x
				sta	$d00f
				
				cpx #29
				beq	row1s7_ryc

				inx
				jmp row1s7_uyc

row1s7_ryc		ldx #00

row1s7_uyc		stx zpSPRITEROW1_YDATA+7

				ldx zpSPRITEROW1_XDATA+7
				
				lda	sprite_sinex7,x
				
				sta	$d00e
				
				lda #$80				; set this sprite's 9th x-coord bit
				ora $d010				;
				sta $d010	
				
row1s7_xtimer	lda zpSPRITEYTIMER
				bne animoddlines
				
				inx
				inx
				cpx	#60
				bne	row1s7_uxc
				
				ldx #$00
				
row1s7_uxc		stx zpSPRITEROW1_XDATA+7

				; ==========================================================================================================
				; ==========================================================================================================
				;
				; ==========================================================================================================
				; ==========================================================================================================

animoddlines	jsr subanimodd

				; ==========================================================================================================

				ldy #CHANGE_ROW2TOP
				cpy $d012
				bne *-3

				lda #$00
				sta $d010
				
				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 2 - Sprite 0
				; ==========================================================================================================
				; ==========================================================================================================

row2s0			ldx zpSPRITEROW2_YDATA
				lda spriteysinus2,x
				sta	$d001
				
				cpx #29
				beq	row2s0_ryc

				inx
				jmp row2s0_uyc

row2s0_ryc		ldx #00

row2s0_uyc		stx zpSPRITEROW2_YDATA

				ldx zpSPRITEROW2_XDATA
				
				lda	sprite_sinex0,x
				
				sta	$d000
				
row2s0_xtimer	lda zpSPRITEYTIMER
				bne row2s1
				
				inx
				inx
				cpx	#60
				bne	row2s0_uxc
				
				ldx #$00
				
row2s0_uxc		stx zpSPRITEROW2_XDATA

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 2 - Sprite 1
				; ==========================================================================================================
				; ==========================================================================================================
				
row2s1			ldx zpSPRITEROW2_YDATA+1
				lda spriteysinus2,x
				sta	$d003
				
				cpx #29
				beq	row2s1_ryc

				inx
				jmp row2s1_uyc

row2s1_ryc		ldx #00

row2s1_uyc		stx zpSPRITEROW2_YDATA+1

				ldx zpSPRITEROW2_XDATA+1
				
				lda	sprite_sinex1,x
				
				sta	$d002
				
row2s1_xtimer	lda zpSPRITEYTIMER
				bne row2s2
				
				inx
				inx
				cpx	#60
				bne	row2s1_uxc
				
				ldx #$00
				
row2s1_uxc		stx zpSPRITEROW2_XDATA+1

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 2 - Sprite 2
				; ==========================================================================================================
				; ==========================================================================================================
				
row2s2			ldx zpSPRITEROW2_YDATA+2
				lda spriteysinus2,x
				sta	$d005
				
				cpx #29
				beq	row2s2_ryc

				inx
				jmp row2s2_uyc

row2s2_ryc		ldx #00

row2s2_uyc		stx zpSPRITEROW2_YDATA+2

				ldx zpSPRITEROW2_XDATA+2
				
				lda	sprite_sinex2,x
				
				sta	$d004
				
row2s2_xtimer	lda zpSPRITEYTIMER
				bne row2s3
				
				inx
				inx
				cpx	#60
				bne	row2s2_uxc
				
				ldx #$00
				
row2s2_uxc		stx zpSPRITEROW2_XDATA+2

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 2 - Sprite 3
				; ==========================================================================================================
				; ==========================================================================================================
				
row2s3			ldx zpSPRITEROW2_YDATA+3
				lda spriteysinus2,x
				sta	$d007
				
				cpx #29
				beq	row2s3_ryc

				inx
				jmp row2s3_uyc

row2s3_ryc		ldx #00

row2s3_uyc		stx zpSPRITEROW2_YDATA+3

				ldx zpSPRITEROW2_XDATA+3
				
				lda	sprite_sinex3,x
				
				sta	$d006
				
row2s3_xtimer	lda zpSPRITEYTIMER
				bne row2s4
				
				inx
				inx
				cpx	#60
				bne	row2s3_uxc
				
				ldx #$00
				
row2s3_uxc		stx zpSPRITEROW2_XDATA+3

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 2 - Sprite 4
				; ==========================================================================================================
				; ==========================================================================================================
				
row2s4			ldx zpSPRITEROW2_YDATA+4
				lda spriteysinus2,x
				sta	$d009
				
				cpx #29
				beq	row2s4_ryc

				inx
				jmp row2s4_uyc

row2s4_ryc		ldx #00

row2s4_uyc		stx zpSPRITEROW2_YDATA+4

				ldx zpSPRITEROW2_XDATA+4
				
				lda	sprite_sinex4,x
				
				sta	$d008
				
row2s4_xtimer	lda zpSPRITEYTIMER
				bne row2s5
				
				inx
				inx
				cpx	#60
				bne	row2s4_uxc
				
				ldx #$00
				
row2s4_uxc		stx zpSPRITEROW2_XDATA+4

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 2 - Sprite 5
				; ==========================================================================================================
				; ==========================================================================================================
				
row2s5			ldx zpSPRITEROW2_YDATA+5
				lda spriteysinus2,x
				sta	$d00b
				
				cpx #29
				beq	row2s5_ryc

				inx
				jmp row2s5_uyc

row2s5_ryc		ldx #00

row2s5_uyc		stx zpSPRITEROW2_YDATA+5

				ldx zpSPRITEROW2_XDATA+5
				
				lda	sprite_sinex5,x
				
				sta	$d00a
				
row2s5_xtimer	lda zpSPRITEYTIMER
				bne row2s6
				
				inx
				inx	
				cpx	#60
				bne	row2s5_uxc
				
				ldx #$00
				
row2s5_uxc		stx zpSPRITEROW2_XDATA+5

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 2 - Sprite 6
				; ==========================================================================================================
				; ==========================================================================================================
				
row2s6			ldx zpSPRITEROW2_YDATA+6
				lda spriteysinus2,x
				sta	$d00d
				
				cpx #29
				beq	row2s6_ryc

				inx
				jmp row2s6_uyc

row2s6_ryc		ldx #00

row2s6_uyc		stx zpSPRITEROW2_YDATA+6

				ldx zpSPRITEROW2_XDATA+6
				
				lda	sprite_sinex6,x
				
				sta	$d00c
				
				inx
				
				lda sprite_sinex6,x		; set this sprite's 9th x-coord bit
				
				ora $d010				;
				sta $d010					

row2s6_xcont	
				
row2s6_xtimer	lda zpSPRITEYTIMER
				bne row2s7
				
				inx
				cpx	#60
				bne	row2s6_uxc
				
				ldx #$00
				
row2s6_uxc		stx zpSPRITEROW2_XDATA+6

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 2 - Sprite 7
				; ==========================================================================================================
				; ==========================================================================================================
				
row2s7			ldx zpSPRITEROW2_YDATA+7
				lda spriteysinus2,x
				sta	$d00f
				
				cpx #29
				beq	row2s7_ryc

				inx
				jmp row2s7_uyc

row2s7_ryc		ldx #00

row2s7_uyc		stx zpSPRITEROW2_YDATA+7

				ldx zpSPRITEROW2_XDATA+7
				
				lda	sprite_sinex7,x
				
				sta	$d00e
				
				lda #$80				; set this sprite's 9th x-coord bit
				ora $d010				;
				sta $d010	
				
row2s7_xtimer	lda zpSPRITEYTIMER
				bne rasterbar3
				
				inx
				inx
				cpx	#60
				bne	row2s7_uxc
				
				ldx #$00
				
row2s7_uxc		stx zpSPRITEROW2_XDATA+7

				; ===========================================================================================================
				
rasterbar3		ldy #CHANGE_ROW3TOP
				cpy $d012
				bne *-3
spr_row3

				lda #$00
				sta $d010
				
				jsr subanimeven

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 3 - Sprite 0
				; ==========================================================================================================
				; ==========================================================================================================

row3s0			ldx zpSPRITEROW3_YDATA
				lda spriteysinus3,x
				sta	$d001
				
				cpx #29
				beq	row3s0_ryc

				inx
				jmp row3s0_uyc

row3s0_ryc		ldx #00

row3s0_uyc		stx zpSPRITEROW3_YDATA

				ldx zpSPRITEROW3_XDATA
				
				lda	sprite_sinex0,x
				
				sta	$d000
				
row3s0_xtimer	lda zpSPRITEYTIMER
				bne row3s1
				
				inx
				inx
				cpx	#60
				bne	row3s0_uxc
				
				ldx #$00
				
row3s0_uxc		stx zpSPRITEROW3_XDATA

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 3 - Sprite 1
				; ==========================================================================================================
				; ==========================================================================================================
				
row3s1			ldx zpSPRITEROW3_YDATA+1
				lda spriteysinus3,x
				sta	$d003
				
				cpx #29
				beq	row3s1_ryc

				inx
				jmp row3s1_uyc

row3s1_ryc		ldx #00

row3s1_uyc		stx zpSPRITEROW3_YDATA+1

				ldx zpSPRITEROW3_XDATA+1
				
				lda	sprite_sinex1,x
				
				sta	$d002
				
row3s1_xtimer	lda zpSPRITEYTIMER
				bne row3s2
				
				inx
				inx
				cpx	#60
				bne	row3s1_uxc
				
				ldx #$00
				
row3s1_uxc		stx zpSPRITEROW3_XDATA+1

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 3 - Sprite 2
				; ==========================================================================================================
				; ==========================================================================================================
				
row3s2			ldx zpSPRITEROW3_YDATA+2
				lda spriteysinus3,x
				sta	$d005
				
				cpx #29
				beq	row3s2_ryc

				inx
				jmp row3s2_uyc

row3s2_ryc		ldx #00

row3s2_uyc		stx zpSPRITEROW3_YDATA+2

				ldx zpSPRITEROW3_XDATA+2
				
				lda	sprite_sinex2,x
				
				sta	$d004
				
row3s2_xtimer	lda zpSPRITEYTIMER
				bne row3s3
				
				inx
				inx
				cpx	#60
				bne	row3s2_uxc
				
				ldx #$00
				
row3s2_uxc		stx zpSPRITEROW3_XDATA+2

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 3 - Sprite 3
				; ==========================================================================================================
				; ==========================================================================================================
				
row3s3			ldx zpSPRITEROW3_YDATA+3
				lda spriteysinus3,x
				sta	$d007
				
				cpx #29
				beq	row3s3_ryc

				inx
				jmp row3s3_uyc

row3s3_ryc		ldx #00

row3s3_uyc		stx zpSPRITEROW3_YDATA+3

				ldx zpSPRITEROW3_XDATA+3
				
				lda	sprite_sinex3,x
				
				sta	$d006
				
row3s3_xtimer	lda zpSPRITEYTIMER
				bne row3s4
				
				inx
				inx
				cpx	#60
				bne	row3s3_uxc
				
				ldx #$00
				
row3s3_uxc		stx zpSPRITEROW3_XDATA+3

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 3 - Sprite 4
				; ==========================================================================================================
				; ==========================================================================================================
				
row3s4			ldx zpSPRITEROW3_YDATA+4
				lda spriteysinus3,x
				sta	$d009
				
				cpx #29
				beq	row3s4_ryc

				inx
				jmp row3s4_uyc

row3s4_ryc		ldx #00

row3s4_uyc		stx zpSPRITEROW3_YDATA+4

				ldx zpSPRITEROW3_XDATA+4
				
				lda	sprite_sinex4,x
				
				sta	$d008
				
row3s4_xtimer	lda zpSPRITEYTIMER
				bne row3s5
				
				inx
				inx
				cpx	#60
				bne	row3s4_uxc
				
				ldx #$00
				
row3s4_uxc		stx zpSPRITEROW3_XDATA+4

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 3 - Sprite 5
				; ==========================================================================================================
				; ==========================================================================================================
				
row3s5			ldx zpSPRITEROW3_YDATA+5
				lda spriteysinus3,x
				sta	$d00b
				
				cpx #29
				beq	row3s5_ryc

				inx
				jmp row3s5_uyc

row3s5_ryc		ldx #00

row3s5_uyc		stx zpSPRITEROW3_YDATA+5

				ldx zpSPRITEROW3_XDATA+5
				
				lda	sprite_sinex5,x
				
				sta	$d00a
				
row3s5_xtimer	lda zpSPRITEYTIMER
				bne row3s6
				
				inx
				inx	
				cpx	#60
				bne	row3s5_uxc
				
				ldx #$00
				
row3s5_uxc		stx zpSPRITEROW3_XDATA+5

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 3 - Sprite 6
				; ==========================================================================================================
				; ==========================================================================================================
				
row3s6			ldx zpSPRITEROW3_YDATA+6
				lda spriteysinus3,x
				sta	$d00d
				
				cpx #29
				beq	row3s6_ryc

				inx
				jmp row3s6_uyc

row3s6_ryc		ldx #00

row3s6_uyc		stx zpSPRITEROW3_YDATA+6

				ldx zpSPRITEROW3_XDATA+6
				
				lda	sprite_sinex6,x
				
				sta	$d00c
				
				inx
				
				lda sprite_sinex6,x		; set this sprite's 9th x-coord bit
				
				ora $d010				;
				sta $d010					

row3s6_xcont	
				
row3s6_xtimer	lda zpSPRITEYTIMER
				bne row3s7
				
				inx
				cpx	#60
				bne	row3s6_uxc
				
				ldx #$00
				
row3s6_uxc		stx zpSPRITEROW3_XDATA+6

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 3 - Sprite 7
				; ==========================================================================================================
				; ==========================================================================================================
				
row3s7			ldx zpSPRITEROW3_YDATA+7
				lda spriteysinus3,x
				sta	$d00f
				
				cpx #29
				beq	row3s7_ryc

				inx
				jmp row3s7_uyc

row3s7_ryc		ldx #00

row3s7_uyc		stx zpSPRITEROW3_YDATA+7

				ldx zpSPRITEROW3_XDATA+7
				
				lda	sprite_sinex7,x
				
				sta	$d00e
				
				lda #$80				; set this sprite's 9th x-coord bit
				ora $d010				;
				sta $d010	
				
row3s7_xtimer	lda zpSPRITEYTIMER
				bne rasterbar4
				
				inx
				inx
				cpx	#60
				bne	row3s7_uxc
				
				ldx #$00
				
row3s7_uxc		stx zpSPRITEROW3_XDATA+7
				
				; ===========================================================================================================
				
rasterbar4		ldy #CHANGE_ROW4TOP
				cpy $d012
				bne *-3
spr_row4

				lda #$00
				sta $d010
				
				jsr subanimodd
				
				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 4 - Sprite 0
				; ==========================================================================================================
				; ==========================================================================================================

row4s0			ldx zpSPRITEROW4_YDATA
				lda spriteysinus4,x
				sta	$d001
				
				cpx #29
				beq	row4s0_ryc

				inx
				jmp row4s0_uyc

row4s0_ryc		ldx #00

row4s0_uyc		stx zpSPRITEROW4_YDATA

				ldx zpSPRITEROW4_XDATA
				
				lda	sprite_sinex0,x
				
				sta	$d000
				
row4s0_xtimer	lda zpSPRITEYTIMER
				bne row4s1
				
				inx
				inx
				cpx	#60
				bne	row4s0_uxc
				
				ldx #$00
				
row4s0_uxc		stx zpSPRITEROW4_XDATA

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 4 - Sprite 1
				; ==========================================================================================================
				; ==========================================================================================================
				
row4s1			ldx zpSPRITEROW4_YDATA+1
				lda spriteysinus4,x
				sta	$d003
				
				cpx #29
				beq	row4s1_ryc

				inx
				jmp row4s1_uyc

row4s1_ryc		ldx #00

row4s1_uyc		stx zpSPRITEROW4_YDATA+1

				ldx zpSPRITEROW4_XDATA+1
				
				lda	sprite_sinex1,x
				
				sta	$d002
				
row4s1_xtimer	lda zpSPRITEYTIMER
				bne row4s2
				
				inx
				inx
				cpx	#60
				bne	row4s1_uxc
				
				ldx #$00
				
row4s1_uxc		stx zpSPRITEROW4_XDATA+1

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 4 - Sprite 2
				; ==========================================================================================================
				; ==========================================================================================================
				
row4s2			ldx zpSPRITEROW4_YDATA+2
				lda spriteysinus4,x
				sta	$d005
				
				cpx #29
				beq	row4s2_ryc

				inx
				jmp row4s2_uyc

row4s2_ryc		ldx #00

row4s2_uyc		stx zpSPRITEROW4_YDATA+2

				ldx zpSPRITEROW4_XDATA+2
				
				lda	sprite_sinex2,x
				
				sta	$d004
				
row4s2_xtimer	lda zpSPRITEYTIMER
				bne row4s3
				
				inx
				inx
				cpx	#60
				bne	row4s2_uxc
				
				ldx #$00
				
row4s2_uxc		stx zpSPRITEROW4_XDATA+2

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 4 - Sprite 3
				; ==========================================================================================================
				; ==========================================================================================================
				
row4s3			ldx zpSPRITEROW4_YDATA+3
				lda spriteysinus4,x
				sta	$d007
				
				cpx #29
				beq	row4s3_ryc

				inx
				jmp row4s3_uyc

row4s3_ryc		ldx #00

row4s3_uyc		stx zpSPRITEROW4_YDATA+3

				ldx zpSPRITEROW4_XDATA+3
				
				lda	sprite_sinex3,x
				
				sta	$d006
				
row4s3_xtimer	lda zpSPRITEYTIMER
				bne row4s4
				
				inx
				inx
				cpx	#60
				bne	row4s3_uxc
				
				ldx #$00
				
row4s3_uxc		stx zpSPRITEROW4_XDATA+3

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 4 - Sprite 4
				; ==========================================================================================================
				; ==========================================================================================================
				
row4s4			ldx zpSPRITEROW4_YDATA+4
				lda spriteysinus4,x
				sta	$d009
				
				cpx #29
				beq	row4s4_ryc

				inx
				jmp row4s4_uyc

row4s4_ryc		ldx #00

row4s4_uyc		stx zpSPRITEROW4_YDATA+4

				ldx zpSPRITEROW4_XDATA+4
				
				lda	sprite_sinex4,x
				
				sta	$d008
				
row4s4_xtimer	lda zpSPRITEYTIMER
				bne row4s5
				
				inx
				inx
				cpx	#60
				bne	row4s4_uxc
				
				ldx #$00
				
row4s4_uxc		stx zpSPRITEROW4_XDATA+4

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 4 - Sprite 5
				; ==========================================================================================================
				; ==========================================================================================================
				
row4s5			ldx zpSPRITEROW4_YDATA+5
				lda spriteysinus4,x
				sta	$d00b
				
				cpx #29
				beq	row4s5_ryc

				inx
				jmp row4s5_uyc

row4s5_ryc		ldx #00

row4s5_uyc		stx zpSPRITEROW4_YDATA+5

				ldx zpSPRITEROW4_XDATA+5
				
				lda	sprite_sinex5,x
				
				sta	$d00a
				
row4s5_xtimer	lda zpSPRITEYTIMER
				bne row4s6
				
				inx
				inx	
				cpx	#60
				bne	row4s5_uxc
				
				ldx #$00
				
row4s5_uxc		stx zpSPRITEROW4_XDATA+5

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 4 - Sprite 6
				; ==========================================================================================================
				; ==========================================================================================================
				
row4s6			ldx zpSPRITEROW4_YDATA+6
				lda spriteysinus4,x
				sta	$d00d
				
				cpx #29
				beq	row4s6_ryc

				inx
				jmp row4s6_uyc

row4s6_ryc		ldx #00

row4s6_uyc		stx zpSPRITEROW4_YDATA+6

				ldx zpSPRITEROW4_XDATA+6
				
				lda	sprite_sinex6,x
				
				sta	$d00c
				
				inx
				
				lda sprite_sinex6,x		; set this sprite's 9th x-coord bit
				
				ora $d010				;
				sta $d010					
			
row4s6_xtimer	lda zpSPRITEYTIMER
				bne row4s7
				
				inx
				cpx	#60
				bne	row4s6_uxc
				
				ldx #$00
				
row4s6_uxc		stx zpSPRITEROW4_XDATA+6

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 4 - Sprite 7
				; ==========================================================================================================
				; ==========================================================================================================
				
row4s7			ldx zpSPRITEROW4_YDATA+7
				lda spriteysinus4,x
				sta	$d00f
				
				cpx #29
				beq	row4s7_ryc

				inx
				jmp row4s7_uyc

row4s7_ryc		ldx #00

row4s7_uyc		stx zpSPRITEROW4_YDATA+7

				ldx zpSPRITEROW4_XDATA+7
				
				lda	sprite_sinex7,x
				
				sta	$d00e
				
				lda #$80				; set this sprite's 9th x-coord bit
				ora $d010				;
				sta $d010	
				
row4s7_xtimer	lda zpSPRITEYTIMER
				bne rasterbar5
				
				inx
				inx
				cpx	#60
				bne	row4s7_uxc
				
				ldx #$00
				
row4s7_uxc		stx zpSPRITEROW4_XDATA+7

				; ===========================================================================================================

rasterbar5		jsr subanimeven				
				
				ldy #CHANGE_ROW5TOP
				cpy $d012
				bne *-3
spr_row5
				lda #$00
				sta $d010

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 5 - Sprite 0
				; ==========================================================================================================
				; ==========================================================================================================

row5s0			ldx zpSPRITEROW5_YDATA
				lda spriteysinus5,x
				sta	$d001
				
				cpx #29
				beq	row5s0_ryc

				inx
				jmp row5s0_uyc

row5s0_ryc		ldx #00

row5s0_uyc		stx zpSPRITEROW5_YDATA

				ldx zpSPRITEROW5_XDATA
				
				lda	sprite_sinex0,x
				
				sta	$d000
				
row5s0_xtimer	lda zpSPRITEYTIMER
				bne row5s1
				
				inx
				inx
				cpx	#60
				bne	row5s0_uxc
				
				ldx #$00
				
row5s0_uxc		stx zpSPRITEROW5_XDATA

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 5 - Sprite 1
				; ==========================================================================================================
				; ==========================================================================================================
				
row5s1			ldx zpSPRITEROW5_YDATA+1
				lda spriteysinus5,x
				sta	$d003
				
				cpx #29
				beq	row5s1_ryc

				inx
				jmp row5s1_uyc

row5s1_ryc		ldx #00

row5s1_uyc		stx zpSPRITEROW5_YDATA+1

				ldx zpSPRITEROW5_XDATA+1
				
				lda	sprite_sinex1,x
				
				sta	$d002
				
row5s1_xtimer	lda zpSPRITEYTIMER
				bne row5s2
				
				inx
				inx
				cpx	#60
				bne	row5s1_uxc
				
				ldx #$00
				
row5s1_uxc		stx zpSPRITEROW5_XDATA+1

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 5 - Sprite 2
				; ==========================================================================================================
				; ==========================================================================================================
				
row5s2			ldx zpSPRITEROW5_YDATA+2
				lda spriteysinus5,x
				sta	$d005
				
				cpx #29
				beq	row5s2_ryc

				inx
				jmp row5s2_uyc

row5s2_ryc		ldx #00

row5s2_uyc		stx zpSPRITEROW5_YDATA+2

				ldx zpSPRITEROW5_XDATA+2
				
				lda	sprite_sinex2,x
				
				sta	$d004
				
row5s2_xtimer	lda zpSPRITEYTIMER
				bne row5s3
				
				inx
				inx
				cpx	#60
				bne	row5s2_uxc
				
				ldx #$00
				
row5s2_uxc		stx zpSPRITEROW5_XDATA+2

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 5 - Sprite 3
				; ==========================================================================================================
				; ==========================================================================================================
				
row5s3			ldx zpSPRITEROW5_YDATA+3
				lda spriteysinus5,x
				sta	$d007
				
				cpx #29
				beq	row5s3_ryc

				inx
				jmp row5s3_uyc

row5s3_ryc		ldx #00

row5s3_uyc		stx zpSPRITEROW5_YDATA+3

				ldx zpSPRITEROW5_XDATA+3
				
				lda	sprite_sinex3,x
				
				sta	$d006
				
row5s3_xtimer	lda zpSPRITEYTIMER
				bne row5s4
				
				inx
				inx
				cpx	#60
				bne	row5s3_uxc
				
				ldx #$00
				
row5s3_uxc		stx zpSPRITEROW5_XDATA+3

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 5 - Sprite 4
				; ==========================================================================================================
				; ==========================================================================================================
				
row5s4			ldx zpSPRITEROW5_YDATA+4
				lda spriteysinus5,x
				sta	$d009
				
				cpx #29
				beq	row5s4_ryc

				inx
				jmp row5s4_uyc

row5s4_ryc		ldx #00

row5s4_uyc		stx zpSPRITEROW5_YDATA+4

				ldx zpSPRITEROW5_XDATA+4
				
				lda	sprite_sinex4,x
				
				sta	$d008
				
row5s4_xtimer	lda zpSPRITEYTIMER
				bne row5s5
				
				inx
				inx
				cpx	#60
				bne	row5s4_uxc
				
				ldx #$00
				
row5s4_uxc		stx zpSPRITEROW5_XDATA+4

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 5 - Sprite 5
				; ==========================================================================================================
				; ==========================================================================================================
				
row5s5			ldx zpSPRITEROW5_YDATA+5
				lda spriteysinus5,x
				sta	$d00b
				
				cpx #29
				beq	row5s5_ryc

				inx
				jmp row5s5_uyc

row5s5_ryc		ldx #00

row5s5_uyc		stx zpSPRITEROW5_YDATA+5

				ldx zpSPRITEROW5_XDATA+5
				
				lda	sprite_sinex5,x
				
				sta	$d00a
				
row5s5_xtimer	lda zpSPRITEYTIMER
				bne row5s6
				
				inx
				inx	
				cpx	#60
				bne	row5s5_uxc
				
				ldx #$00
				
row5s5_uxc		stx zpSPRITEROW5_XDATA+5

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 5 - Sprite 6
				; ==========================================================================================================
				; ==========================================================================================================
				
row5s6			ldx zpSPRITEROW5_YDATA+6
				lda spriteysinus5,x
				sta	$d00d
				
				cpx #29
				beq	row5s6_ryc

				inx
				jmp row5s6_uyc

row5s6_ryc		ldx #00

row5s6_uyc		stx zpSPRITEROW5_YDATA+6

				ldx zpSPRITEROW5_XDATA+6
				
				lda	sprite_sinex6,x
				
				sta	$d00c
				
				inx
				
xsetbit9 		lda sprite_sinex6,x		; set this sprite's 9th x-coord bit
				
				ora $d010				;
				sta $d010					

row5s6_xcont	
				
row5s6_xtimer	lda zpSPRITEYTIMER
				bne row5s7
				
				inx
				cpx	#60
				bne	row5s6_uxc
				
				ldx #$00
				
row5s6_uxc		stx zpSPRITEROW5_XDATA+6

				; ==========================================================================================================
				; ==========================================================================================================
				; ROW 5 - Sprite 7
				; ==========================================================================================================
				; ==========================================================================================================
				
row5s7			ldx zpSPRITEROW5_YDATA+7
				lda spriteysinus5,x
				sta	$d00f
				
				cpx #29
				beq	row5s7_ryc

				inx
				jmp row5s7_uyc

row5s7_ryc		ldx #00

row5s7_uyc		stx zpSPRITEROW5_YDATA+7

				ldx zpSPRITEROW5_XDATA+7
				
				lda	sprite_sinex7,x
				
				sta	$d00e
				
				lda #$80				; set this sprite's 9th x-coord bit
				ora $d010				;
				sta $d010	
				
row5s7_xtimer	lda zpSPRITEYTIMER
				bne rownext2
				
				inx
				inx
				cpx	#60
				bne	row5s7_uxc
				
				ldx #$00
				
row5s7_uxc		stx zpSPRITEROW5_XDATA+7
				
				; ==========================================================================================================
				; ==========================================================================================================
				
rownext2		dec zpSPRITEYTIMER
				bpl colourcycle

				lda #$01
				sta zpSPRITEYTIMER
				
				jsr subanimodd
				
				; ==========================================================================================================
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
				
				lda #cCYAN				; sprite colour 1
				sta $d025				;
				lda #cBLUE				; sprite colour 2
				sta $d026				;
				
scroll3sprite0	lda	#$20				; code modifies this value
				sta	SPRITEPTR
scroll3sprite1	lda	#$20				; code modifies this value
				sta	SPRITEPTR+1
scroll3sprite2	lda	#$20				; code modifies this value
				sta	SPRITEPTR+2
scroll3sprite3	lda	#$20				; code modifies this value
				sta	SPRITEPTR+3
scroll3sprite4	lda	#$20				; code modifies this value
				sta	SPRITEPTR+4
scroll3sprite5	lda	#$20				; code modifies this value
				sta	SPRITEPTR+5
scroll3sprite6	lda	#$20				; code modifies this value
				sta	SPRITEPTR+6
scroll3sprite7	ldx	#$20				; code modifies this value
				stx	SPRITEPTR+7
				
				cpx #$00				; have we reached the end? (last character of scroll is $00)
				bne movebotsprites		; no
				
				lda #>bottomscrolly+7	; update the address of the next sprite image
				sta sprite7xx+2			; in code
				lda #<bottomscrolly+7	;
				sta sprite7xx+1
				
				lda #$25
				sta sprite_rows_begin+1	; modify sprites to cats ;)
				
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
							
mainloop		jmp main

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
				
drawscreen		ldy #$00
				ldx #$00
				
drawloop		lda screenline1,y		; this is modified below
				sta SCREENRAM,y			; this is modified below
				
				iny						;
				
				cpy #40					; 40 chars per line
				bne drawloop
				
nextscrenline	lda	drawloop+1			; load screenline data address
				
				clc
				adc #40					; next screenline data
				sta drawloop+1			; save new address
				
				bcc	nextramline
				
				inc drawloop+2			; cross page boundary? yes!
				
nextramline		lda	drawloop+4			; load
				
				clc
				adc #80					; next screen RAM address (alternate rows)
				sta drawloop+4			; save new address
				
				bcc	contdraw
				
				inc drawloop+5			; cross page boundary? yes!
				
contdraw		inx						; next line (for loop)
				ldy #$00				; reset to beginning of line

				cpx #12					; 12 lines on screen
				bne	drawloop
				
				rts						; return to initcode
				
				; ===========================================================================================================
				
subanimeven		ldy #$00
				ldx zpLINECYCLE_EVNCOL		; current index to colour table
				cpx #40						; number of colours in colour table
				bne loadcolour
				
				ldx #$00
				stx zpLINECYCLE_EVNCOL
				
loadcolour		lda	colorsw,x				; load colour from table
				ldx zpLINECYCLE_EVNOFF		; index to colour table
charloopeven	sta $d800,x					; row 1
				sta $d8A0,x					; row 3
				sta $d940,x					; row 5
				sta $d9e0,x					; row 7
				sta $da80,x					; row 9
				sta $db20,x					; row 11

				inx
				iny

				cpy	#$04
				bne	charloopeven
				
				cpx #37						; end column
				bne charloopevenx
				
				ldx #$05					; start column
				inc zpLINECYCLE_EVNCOL		; next index to colour table
				
charloopevenx	stx zpLINECYCLE_EVNOFF		;				
				
				rts
				
				; ===========================================================================================================
				
subanimodd		ldy #$00
				ldx zpLINECYCLE_ODDCOL		; current index to colour table
				cpx #40
				bne loadcolouro
				
				ldx #$00
				stx zpLINECYCLE_ODDCOL
				
loadcolouro		lda	colorsw,x
				ldx zpLINECYCLE_ODDOFF		; current column
charloopodd		sta $d850,x					; row 2
				sta $d8F0,x					; row 4
				sta $d990,x					; row 6
				sta $da30,x					; row 8
				sta $dad0,x					; row 10
				sta $db70,x					; row 12

				dex
				iny

				cpy	#$04
				bne	charloopodd
				
				cpx #$04					; end column
				bne charloopoddx
				
				ldx #$24					; start column
				inc zpLINECYCLE_ODDCOL
				
charloopoddx	stx zpLINECYCLE_ODDOFF		; move to next index to colour table				

				rts
				
				; ===========================================================================================================
				; ===========================================================================================================
				; ===========================================================================================================

				.align $100
                ;                                                                  *    represents high bit for sprite 7      
spritex			.byte $35,$00,$56,$00,$77,$00,$98,$00,$B9,$00,$Da,$00,$Fb,$00,$1c,$80 ; $21 delta

				; generated by the code
sprite_sinex0	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0				
sprite_sinex1	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
sprite_sinex2	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0				
sprite_sinex3	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
sprite_sinex4	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0				
sprite_sinex5	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0				
sprite_sinex6	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0				
sprite_sinex7	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				; generated by the code
spriteysinus1	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
spriteysinus2	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
spriteysinus3	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
spriteysinus4	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
spriteysinus5	.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
				.byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
spriteysinusoff	.byte   5,  6,  7,  8,  8,  9, 10, 10, 10, 10,  9,  9,  8,  7,  6
				.byte   5,  4,  3,  2,  1,  1,  0,  0,  0,  0,  1,  1,  2,  3,  4
				
				; ===========================================================================================================
				
				.enc "screen"
bottomscrolly	.text '        WELCOME TO MY LATEST INTRO, THE WIBBLY WOBBLY ^CUBES^. IT''S FEBRUARY, IT''S COLD, AND I''M '
				.text 'AT MY DESK WRITING THIS SCROLL TEXT# RUTHERFORD THE CAT KEEPS ROLLING OVER THE KEYBOARD, WHICH MAKES '
				.text 'THINGS A BIT MORE DIFFICULT, BUT I THINK SHE''S ASLEEP NOW... SO, GREETS TO THE DEVELOPMENT CATS ^ '
				.text 'RUTHERFORD, FREEMAN, AND MAXWELL^ SARAH, STEVE, STEVE, DAN, AMY, TARTY, AND C64 FANS EVERYWHERE# '
				.text 'DEDICATED TO JULIE, DYANNE, AND ADAM. '
				.text 'ALL CODE AND GRAPHICS BY ME, THE FUNKY MUSIC IS BY LEMING "FUZZY BREAK". '
				.text 'THIS INTRO IS 3238 LINES OF ASSEMBLER, EDITED WITH NOTEPAD** AND ASSEMBLED WITH 64TASS. '
				.text 'CHECK OUT - GITHUB.COM/MAXIMUMOCTOPUS - FOR MORE COOL STUFF... '
				.text 'THE CATS SAY MEOW ....  '
				.byte $00   ; always end with a zero
				.enc "none"
				
screenline1		.text '     ------------------------------     '
screenline2		.text '            MAXIMUM OCTOPUS             '
screenline3		.text '        WIBBLY WOBBLY CUBES DEMO        '
screenline4		.text '     ------------------------------     '
screenline5		.text '         WWW.MAXIMUMOCTOPUS.COM         '
screenline6		.text '        GITHUB.COM/MAXIMUMOCTOPUS       '
screenline7		.text '            WWW.FRESHNEY.ORG            '
screenline8		.text '     ------------------------------     '
screenline9		.text '        MADE IN NEWARK, ENGLAND!        '
screenline10	.text '           FEBRUARY 26TH 2023           '
screenline11	.text '           PAUL ALAN FRESHNEY           '
screenline12	.text '     ------------------------------     '
			
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
				
colorsw			.byte $0b,$0c,$0f,$0d,$0d,$0f,$0c,$0b 		; grey/green
				.byte $0b,$0c,$0f,$01,$01,$0f,$0c,$0b 		; grey/white
				.byte $02,$04,$0a,$01,$01,$0a,$04,$02 		; red/purple
				.byte $06,$0e,$03,$01,$01,$03,$0e,$06		; blue/white
				.byte $09,$08,$07,$01,$01,$07,$08,$09		; yellow/white
				
				; ===========================================================================================================
		 
				*=MUSICLOAD
		 
				.binary "music\$1000\fuzzy_break.dat",2
				
				; ===========================================================================================================
		
				*=CHARRAM
		
				.binary "fonts\swiii17.64c",2
				
				; ===========================================================================================================
			
				*=VICBASE
				
				.include "include\spritefont_bas_relief_2.inc" ; 59 sprites
				
				; ===========================================================================================================
				
				*=$C000
				
				.byte 0,16,0,0,84,0,1,85,0,5,85,64,21,85,80,85,85,84,149,85
				.byte 92,165,85,124,169,85,252,170,87,252,170,159,252,170,175,252,170,175,252,170
				.byte 175,252,170,175,252,170,175,252,42,175,240,10,175,192,2,175,0,0,172,0
				.byte 0,32,0,0
				
				; ===========================================================================================================161