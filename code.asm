;*******************************************************************
;                               TEMPLATE PROVIDED BY CENTRE

;         TITLE:  #### TRAFFIC LIGHTS CONTROL SYSTEM #####
;         AUTHOR: ######## THOMAS BOXALL #########
;         DATE:   ####### 17-03-2022 ######
;

;******************************************************************
; PROGRAM DESCRIPTION:
;
; ####  State here what the program does  #######
;
;*********************************************************************
;			DEFINITIONS
;*********************************************************************
    list    p=16F88             	; tells the assembler which PIC chip to program for
    radix	dec                 	; set default number radix to decimal
    ;radix	hex                 	uncomment this to  set  radix to hex
    __config h'2007', 0x3F50	; internal oscillator, RA5 as i/o, wdt off
    __config h'2008', 0x3FFF	
    errorlevel -302             	; hide page warnings

W           	EQU h'00'	; pointer to Working register
F            	EQU h'01'	; pointer to file

;****** REGISTER USAGE ******

;For PIC16F88, user RAM starts at h'20'. The following definitions
;will be found useful in many programs.

; Register page 1
TRISA	EQU h'85'	; data direction registers
TRISB	EQU h'86'
OSCCON	EQU h'8F'	; internal oscillator speed
ANSEL	EQU h'9B'	; ADC port enable bits

; Register page 0        
STATUS	EQU h'03' 	; status
PORTA	EQU h'05'	; input / output ports
PORTB	EQU h'06'
INTCON	EQU h'0B'	; interrupt control
ADRESH	EQU h'1E'	; ADC result
ADCON0	EQU h'1F'	; ADC control

B0	EQU h'20'	; general use byte registers B0 to B27
B1	EQU h'21'
B2	EQU h'22'
B3	EQU h'23'
B4	EQU h'24'
B5	EQU h'25'
B6	EQU h'26'
B7	EQU h'27'
B8	EQU h'28'
B9	EQU h'29'
B10	EQU h'2A'
B11	EQU h'2B'
B12	EQU h'2C'
B13	EQU h'2D'
B14	EQU h'2E'
B15	EQU h'2F'
B16	EQU h'30'
B17	EQU h'31'
B18	EQU h'32'
B19	EQU h'33'
B20	EQU h'34'	; used in interrupt routine
B21	EQU h'35'	; used in interrupt routine
B22	EQU h'36'
B23	EQU h'37'
B24	EQU h'38'
B25	EQU h'39'
B26	EQU h'3A'
B27	EQU h'3B'

WAIT1	EQU h'3C'	; counters used in wait delays
WAIT10	EQU h'3D'
WAIT100	EQU h'3E'
WAIT1000	EQU h'3F'
ADCTEMP	EQU h'40'	; adc loop counter

;my vars below
loopX EQU h'41' ;loop counter variable x
loopY EQU h'42' ;loop counter variable y

;****** REGISTER BITS ******

C          	EQU h'00' 	; carry flag
Z           	EQU h'02'	; zero flag
RP0         	EQU h'05'	; register page bit
INT0IF      	EQU h'01'	; interrupt 0 flag
INT0IE     	EQU h'04'	; interrupt 0 enable
GIE         	EQU h'07'	; global interrupt enable

;*********************************************************************
;			VECTORS
;*********************************************************************

;The PIC16F88 reset vectors 

    ORG     h'00'       	; reset vector address
	goto 	start  	; goes to first instruction on reset/power-up
    ORG     h'04'     	; interrupt vector address
	goto	interrupt
;
;*********************************************************************
;			SUBROUTINES
;*********************************************************************
; Predefined wait subroutines - wait1ms, wait10ms, wait100ms, wait1000ms

wait1ms		; (199 x 5) + 5 instructions = 1000us = 1ms @ 4MHz resonator
	movlw d'199'    ; 1
	movwf WAIT1  	; 1
loop5ns
	clrwdt         	; 1 this loop 1+1+1+2 = 5 instructions
	nop            	; 1
	decfsz WAIT1,F	; 1 
	goto loop5ns	; 2
	nop            	; 1
	return          	; 2
wait10ms
	movlw d'10'     	; 10 x 1ms = 10ms
	movwf WAIT10	
loop10ms
	call 	wait1ms	
	decfsz 	WAIT10,F	
	goto 	loop10ms	 
	return          

wait100ms
	movlw d'100'	; 100 x 1ms = 100ms
	movwf WAIT100	
loop100ms
	call 	wait1ms	
	decfsz 	WAIT100,F
	goto 	loop100ms	 
	return          

wait1000ms
	movlw 	d'10'     ; 10 x 100ms = 1000ms
	movwf 	WAIT1000	 
loop1000ms
	call 	wait100ms	
	decfsz 	WAIT1000,F
	goto 	loop1000ms	
	return          

; MY SUBROUTINES BELOW

;wait five seconds and check the pedestian crossing while looping
;wait5SecondsButtonCheck
;	;setup times etc for loops
;	movlw d'50' ;set val of 50 into working register
;	movf loopX ;move 50 into loopX
;	movlw d'100' ;set val of 100 into working regsiter
;	movf loopY ;move 100 into loopY
;	
;	movf loopY, 0  ; move loopY into W register
;	sublw d'100' ;subtract W register (loopY) from 100
;	btfss STATUS, Z ;skip next line if the zero bit is set (finished the loop)
;	goto contY ;goto the continue working on loopY


;contY	movf loopY, 0 ;moove loopY into W register
	

waitFiveSecondsCheckButton
	;first, stary loop x
	movlw d'5' ;set val of 5 into w register
	;movf loopX ; move the val in the working register (5) into loopX

topX decfsz loopX, 1 ;decrement loopX and place the new value back into loopX
	goto startY ;line skipped if result is 0
	return ;if result of loopX - 1 is 0, run this line

startY movlw d'101'; ;set value of 101 into w register
	;movf loopY ;move value in working register (101) into loopY

topY decfsz loopY, 1 ;decrement loopY and place new value back into loopY
	goto finalPart ;goto final part of the function
	goto topX ;run if loopY = 0

finalPart call checkButton
	call wait10ms ;wait for 10ms function
	goto topY ;go back up to topY and loop around again
;end of function


waitOneSecondCheckButton
	movlw d'101' ;set value of 101 into w register
	movf loopY ;move val from working register into loopY
topZ decfsz loopY, 1 ;decrement loopY and place new value back into loopY
	goto finalPart2 ;goto final part of the function (if loopyY>0)
	return ; go back to where called from if loopY=0
finalPart2 call checkButton
	call wait10ms ;wait for 10ms function
	goto topZ ;go back up to the decrement of loopY
; end of function


;function to check the button
checkButton
	btfss PORTB, 1 ;skip next line of code if button pressed (active high)
	return ;can go back to where called from
	bsf PORTB, 2 ;turn white wait LED on
	return ;go back to main code
;end of function


; function to cycle the pedestrian crossing
cycleCrossing
	bcf PORTB, 2 ;turn off red wait LED
	bcf PORTB, 3 ;turn off red stop LED
	bsf PORTB, 4 ;turn on green go LED
	bsf PORTB, 5 ;turn on buzzer
	call wait1000ms ;wait 1second
	call wait1000ms ;wait 1second
	call wait1000ms ;wait 1second
	;now have waited 3seconds so invert things and return to main sequence
	bcf PORTB, 4 ;turn off green go LED
	bsf PORTB, 3 ;turn on red stop LED
	bcf PORTB, 5 ;turn off buzzer
	return ;return to the main program
;end of function

greenTrafficLightSequence
	bsf PORTA, 4 ;turn on amber led
	call waitOneSecondCheckButton ;wait 5s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bcf PORTA, 4 ;turn off amber led
	bcf PORTA, 3 ;turn off red led
	bsf PORTB, 7 ;turn on greeen led
	call waitOneSecondCheckButton ;wait 10s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton 
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bcf PORTB, 7 ;turn off green led
	bsf PORTA, 4 ;turn on amber led
	call waitOneSecondCheckButton ;wait 5s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bsf PORTA, 3 ;turn on red led
	bcf PORTA, 4 ;turn off amber led
	return ;go back to main code
;end of function

pinkTrafficLightSequence
	bsf PORTA, 1 ;turn on amber led
	call waitOneSecondCheckButton ;wait 5s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bcf PORTA, 1 ;turn off amber led
	bcf PORTA, 0 ;turn off red led
	bsf PORTA, 2 ; turn on greeen led
	call waitOneSecondCheckButton ;wait 10s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton 
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bcf PORTA, 2 ;turn off green led
	bsf PORTA, 1 ;turn on amber led
	call waitOneSecondCheckButton ;wait 5s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bsf PORTA, 0 ;turn on red led
	bcf PORTA, 1 ;turn off amber led
	return ;go back to main code
;end of function

blueTrafficLightSequence
	bsf PORTA, 7 ;turn on amber led
	call waitOneSecondCheckButton ;wait 5s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bcf PORTA, 7 ;turn off amber led
	bcf PORTA, 6 ;turn off red led
	bsf PORTB, 0 ; turn on greeen led
	call waitOneSecondCheckButton ;wait 10s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton 
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bcf PORTB, 0 ;turn off green led
	bsf PORTA, 7 ;turn on amber led
	call waitOneSecondCheckButton ;wait 5s
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	call waitOneSecondCheckButton
	bsf PORTA, 6 ;turn on red led
	bcf PORTA, 7 ;turn off amber led
	return ;go back to main code
;end of function

checkPedestrian
	;function to check if the wait led is on
		;if yes - cycle pedestrian crossing
		;if no - return to main 
	btfsc PORTB, 2 ;skip next line if wait led NOT on therefore if on, run next line
	call cycleCrossing
	return
;end function

; Predefined ADC subroutines - readadc0, readac1, readadc2

readadc0
	movlw	b'00000001'	; setup mask for pin A.0
	call	readadc	; do the adc conversion
	movwf	B0         	; save result in B0
	return
readadc1
	movlw	b'00000010'	; setup mask for pin A.1
	call	readadc	; do the adc conversion
	movwf	B1          	; save result in B1
	return
readadc2
	movlw	b'00000100' 	; setup mask for pin A.2
	call	readadc	; do the adc conversion
	movwf	B2          	; save result in B2
	return

readadc
; Generic sub routine to read ADC 0, 1 or 2 (pass appropriate mask in W)
; To start conversion we need mask (001, 010, 100) in ANSEL bits 0-2
; but the actual channel number (0, 1, 2) in ADCON0 channel select bits
; Then set the ADCON0, GO bit to start the conversion

	bsf     	STATUS,RP0	; select register page 1
	movwf	ANSEL		; move mask value 001,010,100 into ANSEL
	bcf     	STATUS,RP0	; select register page 0
	movwf	ADCTEMP	; 00000??? 	get mask value
	rlf     	ADCTEMP,F	; 0000???x	rotate twice
	rlf     	ADCTEMP,W	; 000???xx
	andlw	b'00011000'	; 000??000	mask off the unwanted bits
	iorlw	b'00000001'	; 000??001	set the 'ADC on' bit	 
	movwf	ADCON0	; move working into ADCON0
    	movlw   	d'10'      	; 10 x 3 = 30us acquistion time
    	movwf   ADCTEMP    	; re-use ADC1 register as a counter
loopacq
    	decfsz  	ADCTEMP,F  	; loop around to create short delay
    	goto    	loopacq    	; each loop is 1+2 = 3 instructions = 3us @ 4MHz
	bsf     	ADCON0,2	; now start the conversion
loopadc
	clrwdt      		; pat the watchdog
	btfsc	ADCON0,2	; is conversion finished?
	goto	loopadc	; no, so wait a bit more
	movf	ADRESH,W	; move result into W
	return          		; return with result in W

; NOTE for PICAXE users:  the following four specific subroutines and two instructions are not supported by PICAXE compiler
readtemp1:
readtemp2:
readtemp3:
debug:
lcd:
	clrw               		; instruction not supported by this template
	return			; instruction not supported by this template
;*********************************************************************
;			MAIN PROGRAM
;*********************************************************************

;****** INITIALISATION ******
start
	bsf     	STATUS,RP0	; select register page 1
	movlw  	b'01100000'	; set to 4MHz internal operation
	movwf	OSCCON	
	clrf	ANSEL		; disable ADC (enabled at power-up)
	bcf     	STATUS,RP0	; select register page 0

;The data direction registers TRISA and TRISB live in the special register set. A '1' in
;these registers sets the corresponding port line to an Input, and a
;'0' makes the corresponding line an output.

Init
	clrf    	PORTA     	; make sure PORTA output latches are low
    	clrf    	PORTB     	; make sure PORTB output latches are low   
	bsf     	STATUS,RP0	; select register page 1
;Modify the next line to correspond with your input output reqirements	
	movlw	b'00000000'	; set port A data direction (0 = output bit, 1 = input bit)
	movwf	TRISA		; 
;Modify the next line to correspond with your input output requirements	
	movlw	b'00000010'	; set port B data direction (0 = output bit, 1 = input bit)
	movwf	TRISB		; 	
	bcf     	STATUS,RP0	; select register page 0

;****** MAIN PROGRAM ******
;************* remove semicolons from next two lines to enable interrupt routine************
   	; bsf    	INTCON,INT0IE  ; set external interrupt enable
   	; bsf    	INTCON,GIE        ; enable all interrupts 

main 
;*********************************************************************

; YOUR PROGRAM GOES HERE ######################

	;first turn on all stop leds
	bsf PORTA, 0 ;turn on pink set stop eld
	bsf PORTA, 3 ;turn on green set stop led
	bsf PORTA, 6 ;turn on blue set stop led
	bsf PORTB, 3 ;turn on pedestrian stop led

mTop call pinkTrafficLightSequence
	call checkPedestrian
	call greenTrafficLightSequence
	call checkPedestrian
	call blueTrafficLightSequence
	call checkPedestrian
	goto mTop

;*********************************************************************
;			INTERRUPT SERVICE ROUTINE
;*********************************************************************

W_SAVE	EQU B20     		; backup registers used in interrupts

interrupt
	movwf 	W_SAVE 	; Copy W to save register
	
    	btfss  	INTCON,INT0IF  ; check correct interrupt has occurred
    	retfie                		; no, so return and re-enable GIE

;**********The interrupt service routine (if required) goes here*********

   	bcf    INTCON,INT0IF  	; clear interrupt flag 
	movf   W_SAVE,W       	; restore W
	retfie             		; return and re-set GIE bit
	
    	END                		; all programs must end with this

 