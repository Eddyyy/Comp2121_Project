;

;
; Created: 15/10/2017 19:35:51
; Author : a
;


.include "m2560def.inc"


.def temp = r16
.def speed = r17
.def number = r18
.def counter = r19
.def value = r21
.def input = r22
.def target = r23
.def flag = r31


.set LCD_DISP_ON = 0b00001110
.set LCD_DISP_OFF = 0b00001000
.set LCD_DISP_CLR = 0b00000001

.set LCD_FUNC_SET = 0b00111000 						; 2 lines, 5 by 7 characters
.set LCD_ENTR_SET = 0b00000110 						; increment, no display shift
.set LCD_HOME_LINE = 0b10000000 					; goes to 1st line (address 0)

.set LCD_SEC_LINE = 0b10101000 						; goes to 2nd line (address 40)

.macro clear
	ldi YL, low(@0)		
	ldi YH, high(@0)
	clr temp
	st Y+, temp			
	st Y, temp
.endmacro

.macro do_lcd_command
	ldi r16, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_data
	mov r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro do_lcd_char
	ldi r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro lcd_set
	sbi PORTA, @0
.endmacro

.macro lcd_clr
	cbi PORTA, @0
.endmacro

.dseg
.org 0x0200
Zero:
	.byte 1
DebounceCounter:
	.byte 2   
TC:	
	.byte 2 

.cseg

.org 0x0000
	jmp RESET
.org INT0addr
	jmp PB_0
.org INT1addr
	jmp PB_1
.org INT2addr
   jmp motor_speed

.org OVF0addr
	jmp Timer0OVF	
	jmp DEFAULT

DEFAULT: reti		; continued


RESET:
	ldi r16, low(RAMEND)
	out SPL, r16
	ldi r16, high(RAMEND)
	out SPH, r16

	ser temp
	out DDRF, temp
	out DDRA, temp
	clr temp
	out PORTF, temp
	out PORTA, temp

	clear DebounceCounter			;clear counters
	clear TC
	clr speed
	clr counter
	clr number
	clr value

	do_lcd_command LCD_FUNC_SET
	rcall sleep_5ms
	do_lcd_command LCD_FUNC_SET
	rcall sleep_1ms
	do_lcd_command LCD_FUNC_SET
	do_lcd_command LCD_FUNC_SET
	do_lcd_command LCD_DISP_OFF
	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_ENTR_SET
	do_lcd_command LCD_DISP_ON
	rjmp main

main:
	clr input
	clr target
	ldi temp, (1<<PE4)		;labeled PE2 acctully PE4 
	out DDRE, temp
	ser temp
	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp
	ldi temp, (1 << ISC21 | 1 << ISC11 | 1 << ISC01)      ; set INT2 as falling-
    sts EICRA, temp             ; edge triggered interrupt
    in temp, EIMSK              ; enable INT2
    ori temp, (1<<INT2 | 1<<INT1 | 1<<INT0)
    out EIMSK, temp
	;set timer interrupt

	ldi temp, (1<< WGM30)|(1<<COM3B1) ; set the Timer3 to Phase Correct PWM mode.
	sts TCCR3A, temp
	ldi temp, (1 << CS32)
	sts TCCR3B, temp		; Prescaling value=8
	
	clr temp
	out TCCR0A, temp
	ldi temp, (1<<CS01)
	out TCCR0B, temp		; Prescaling value=8
	ldi temp, 1<<TOIE0		; Enable timeroverflow flag
	sts TIMSK0, temp	
	sts OCR3BH,input
	sts OCR3BL,input
	sts Zero, input
	sei						; Enable global interrupt

loop:
	rjmp loop

PB_0:
	cpi flag,0	; Check if the DebounceFlag is enabled
	breq DecreaseSpeed
	reti	
DecreaseSpeed:
	ldi flag,1
	cpi target, 0
	breq return0
	subi target,20
return0:
	reti
PB_1:
	cpi flag,0	; Check if the DebounceFlag is enabled
	breq IncreaseSpeed	
	reti
IncreaseSpeed:
	ldi flag,1
	cpi target, 100
	breq return1
	subi target,-20
return1:
	reti


Timer0OVF:

	in temp, SREG
	push temp			; Prologue starts.
	push YH				; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24
	// Update Debounce Flag here
	lds R24,DebounceCounter
	lds R25,DebounceCounter+1
	adiw r25:r24,1;
	cpi r24, low(1700)		; We set the debounce rate to ~200ms
	ldi temp,high(1700)
	cpc r25, temp
	breq SetDebounceFlag
	sts DebounceCounter,r24
	sts DebounceCounter+1,r25
	rjmp TimeCounter

SetDebounceFlag:
	clear DebounceCounter
	clr flag


TimeCounter:
	lds r24, TC
	lds r25, TC+1 
	adiw r25:r24, 1
	cpi r25, high(1900)
	ldi temp, low(1900)
	cpc r24, temp
	brne NotaSecond
updataSpeed:
	cpi target, 0
	brne continue
	clr input
	sts OCR3BL, input
	rjmp UpdateTime
continue:
	cp speed, target
	breq UpdateTime
	lds input, OCR3BL
	cp speed, target
	brsh decrease
	cpi input, 255
	breq UpdateTime
	subi input, -3
	sts OCR3BL, input
	rjmp UpdateTime
decrease:
	cpi input, 0
	breq UpdateTime
	subi input, 3
	sts OCR3BL, input
UpdateTime:
	clear TC
	rcall UpdateSpeed
	rjmp Endif

NotaSecond:
	sts TC, r24
	sts TC+1, r25

Endif:
	pop	r24
	pop	r25
	pop	YL
	pop	YH
	pop	temp
	out SREG, temp
	reti

UpdateSpeed:
	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_HOME_LINE
	mov speed, target
	rcall display
	do_lcd_command LCD_SEC_LINE
	mov speed, value
	clr value
	rcall display
	ret

display:
	push speed
Dloop:
	cpi speed, 100
	brsh hunderd
	cpi speed, 10
	brsh ten
	lds temp, Zero
	clr number
	sts Zero, number
	cpi temp, 1
	brne go
	do_lcd_char '0'
go:
	subi speed, -'0'
	do_lcd_data speed
return:
	pop speed
	ret
hunderd:
	ldi temp, 1
	sts Zero, temp
	ldi counter, 100
Hloop:
	dec speed
	dec counter
	cpi counter, 0
	brne Hloop
	inc number
	cpi speed, 100
	brlo showNumber
	rjmp Dloop

ten:
	clr temp
	sts Zero, temp
	ldi counter, 10
Tloop:
	dec speed
	dec counter
	cpi counter, 0
	brne Tloop
	inc number
	cpi speed, 10
	brlo showNumber
	rjmp Dloop

showNumber:
	subi number, -'0'
	do_lcd_data number
	clr number
	rjmp Dloop

motor_speed:
	in temp, SREG
	push temp
	inc value
	pop temp
	out SREG, temp
	reti
;==============================
; Delay Constants
.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4 				; 4 cycles per iteration - setup/call-return overhead

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

lcd_command:
	out PORTF, r16
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, r16
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r16
	clr r16
	out DDRF, r16
	out PORTF, r16
	lcd_set LCD_RW
lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in r16, PINF
	lcd_clr LCD_E
	sbrc r16, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r16
	out DDRF, r16
	pop r16
	ret

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret

