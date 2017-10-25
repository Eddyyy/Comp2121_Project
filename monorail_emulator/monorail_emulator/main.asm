;
; monorail_emulator.asm
;
; Created: 20/10/2017 6:25:51 PM
; Author : Edward Thomsonn, Zhiwei Cao

;IMPORTANT NOTICE: 
;The labels on PORTL are reversed, i.e., PLi is actually PL7-i (i=0, 1, ¡­, 7).  

;Board settings: 
;Connect the four columns C0-C3 of the keypad to PL3-PL0 of PORTL and the four rows R0-R3 to PL7-PL4 of PORTL.
;For LCD data connect D0-D7 to PF0-PF7 respectively and for LCD commands connect BE,RW,E,RS to PA4-PA7 respectively
    
; For I/O registers located in extended I/O map, "IN", "OUT", "SBIS", "SBIC", 
; "CBI", and "SBI" instructions must be replaced with instructions that allow access to 
; extended I/O. Typically "LDS" and "STS" combined with "SBRS", "SBRC", "SBR", and "CBR".

.include "m2560def.inc"
.def row = r17
.def col = r18
.def mask = r19
.def temp2 = r20
.def temp = r21
.def counter = r22
.def buffer = r23
.def flag = r9
.def mode = r10
.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F


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
;A1 = A
;A9 = I 
;B1 = J
;B9 = R
;C1 = S
;C8 = Z

; mode = 0 Number Typing
; mode = 1 Char Typing
.dseg


.cseg
jmp RESET

.org 0x72
RESET:
ldi temp, low(RAMEND)
out SPL, temp
ldi temp, high(RAMEND)
out SPH, temp
ldi temp, PORTLDIR ; columns are outputs, rows are inputs
STS DDRL, temp     ; cannot use out

ser temp
out DDRF, temp
out DDRA, temp
clr temp
out PORTF, temp
out PORTA, temp

clr counter
clr flag
clr mode
clr buffer
do_lcd_command 0b00111000 ; 2x5x7
rcall sleep_5ms
do_lcd_command 0b00111000 ; 2x5x7
rcall sleep_1ms
do_lcd_command 0b00111000 ; 2x5x7
do_lcd_command 0b00111000 ; 2x5x7
do_lcd_command 0b00001000 ; display off?
do_lcd_command 0b00000001 ; clear display
do_lcd_command 0b00000110 ; increment, no display shift
do_lcd_command 0b00001110 ; Cursor on, bar, no blink


; keypad keeps scanning the keypad to find which key is pressed.
keypad:
	ldi mask, INITCOLMASK ; initial column mask
	clr col ; initial column
colloop:
	STS PORTL, mask ; set column to mask value
	; (sets column 0 off)

	ldi temp, 0xFF ; implement a delay so the
	; hardware can stabilize
delay:
	dec temp
	brne delay

	LDS temp, PINL ; read PORTL. Cannot use in 
	andi temp, ROWMASK ; read only the row bits
	cpi temp, 0xF ; check if any rows are grounded
	breq nextcol ; if not go to the next column
	ldi mask, INITROWMASK ; initialise row check
	clr row ; initial row

rowloop:
	mov temp2, temp
	and temp2, mask ; check masked bit
	brne skipconv ; if the result is non-zero,
	; we need to check again
	rcall convert ; if bit is clear, convert the bitcode
	jmp keypad ; and start again
skipconv:
	inc row ; else move to the next row
	lsl mask ; shift the mask to the next bit
	jmp rowloop          
nextcol:     
	cpi col, 3 ; check if we are on the last column
	brne Continue ; if so, no buttons were pushed,
	; so start again.
	clr flag
	jmp keypad


Continue:
	sec ; else shift the column mask:
	; We must set the carry bit
	rol mask ; and then rotate left by a bit,
	; shifting the carry into
	; bit zero. We need this to make
	; sure all the rows have
	; pull-up resistors
	inc col ; increment column value
	jmp colloop ; and check the next column
	; convert function converts the row and column given to a
	; binary number and also outputs the value to PORTC.
	; Inputs come from registers row and col and output is in
	; temp.
convert:
	cpi col, 3 ; if column is 3 we have a letter
	breq letters
	cpi row, 3 ; if row is 3 we have a symbol or 0
	breq symbols
;we need to change this to display letters when the numbers are pressed
	mov temp, row ; otherwise we have a number (1-9)
	lsl temp ; temp = row * 2
	add temp, row ; temp = row * 3
	add temp, col ; add the column address
	; to get the offset from 1
	inc temp ; add 1. Value of switch is
	; row*3 + col + 1.
	jmp branch

letters:
	ldi temp, 0b01000001
	add temp, row ; increment from 0xA by the row value
	jmp branch
symbols:
	cpi col, 0 ; check if we have a star
	breq star
	cpi col, 1 ; or if we have zero
	breq zero
	ldi temp, 0b00100011 ; we'll output 0xF for hash
	jmp branch
star:
	ldi temp, 0b00101010 ; we'll output 0xE for star
	jmp branch
zero:
	clr temp ; set to zero
	jmp branch


branch:
	sbrs flag, 0		;cpi flag, 1
	rjmp convert_ret
	sbrs mode, 0	;cpi mode, 1
	rjmp character_mode
	rjmp number_mode

character_mode:
	cpi buffer, 0
	cpi temp, 0b01000100
	breq end_input
	breq compare_A
	cpi temp, 10
	brlo load_values
	rjmp compare_A
load_values:
	add temp, buffer
	clr buffer
	inc counter
	do_lcd_data temp
	rjmp convert_end
compare_A:
	cpi temp, 0b01000001
	brne compare_B
	ldi buffer,0b01000000; A-1
	rjmp convert_end
compare_B:
	cpi temp, 0b01000010
	brne compare_C
	ldi buffer,0b01001001; J-1
	rjmp convert_end
compare_C:
	cpi temp, 0b01000011
	brne convert_end
	ldi buffer,0b01010010; S-1
	rjmp convert_end
number_mode:

convert_end:
	inc flag
;	inc counter
;	do_lcd_data temp

end_input:

convert_ret:
ret ; return to caller

;-------------------------------LCD PART

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro

;
; Send a command to the LCD (r16)
;

lcd_command:
	out PORTF, r16
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	ret

lcd_data:
	out PORTF, r16
	lcd_set LCD_RS
	nop
	nop
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r16
	clr r16
	out DDRF, r16
	out PORTF, r16
	lcd_set LCD_RW
lcd_wait_loop:
	nop
	lcd_set LCD_E
	nop
	nop
        nop
	in r16, PINF
	lcd_clr LCD_E
	sbrc r16, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r16
	out DDRF, r16
	pop r16
	ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

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
	
; Generated by delay loop calculator
; at http://www.bretmulvey.com/avrdelay.html
;
; Delay 15 999 992 cycles
; 999ms 999us 500 ns at 16 MHz
; extra 8 cycles for rcall, ret, push and pop makes 16*10^6 cycles at 16 MHz
wait_1s:
	push r23
	push r24
	push r25

    ldi  r18, 82
    ldi  r19, 43
    ldi  r20, 254
L1: dec  r20
    brne L1
    dec  r19
    brne L1
    dec  r18
    brne L1
    rjmp PC+1
	pop r25
	pop r24
	pop r23
	ret

get_chars:
	;while(c<10 or temp != D
	rcall keypad
	;do logic on temp takes into account mode
	display temp ;if temp != A,B,C,D
	store_config ;if temp == D && mode == 1 --> store ';'



main:
	; asks first question 'Please type the maximum number of stations:' 
	display_message,xl,xh ;A will hold the message to send to LCD
	clr mode
	rcall get_chars ;return result
	store_result

	;for(i=0;i<result;i++)
