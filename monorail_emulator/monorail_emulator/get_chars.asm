;IMPORTANT NOTICE: 
;The labels on PORTL are reversed, i.e., PLi is actually PL7-i (i=0, 1, ¡­, 7).  

;Board settings: 
;Connect the four columns C0~C3 of the keypad to PL3~PL0 of PORTL and the four rows R0~R3 to PL7~PL4 of PORTL.
;Connect LED0~LED7 of LEDs to PC0~PC7 of PORTC.
    
; For I/O registers located in extended I/O map, "IN", "OUT", "SBIS", "SBIC", 
; "CBI", and "SBI" instructions must be replaced with instructions that allow access to 
; extended I/O. Typically "LDS" and "STS" combined with "SBRS", "SBRC", "SBR", and "CBR".

.include "m2560def.inc"
.def row =r17
.def col =r18
.def mask =r19
.def temp2 =r20
.def temp =r21
.def i = r22
;.def counter = r23
;.def assci_zero =r21
.equ PORTLDIR = 0xF0;11110000
.equ INITCOLMASK = 0xEF;11101111
.equ INITROWMASK = 0x01;00000001
.equ ROWMASK = 0x0F;00001111


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

.macro ten_times
	lsl @0
	lsl @0
	ldi r16, 2
	add @0, r16
.endmacro

.dseg
buffer: .byte 1
String_container: .byte 10
Number_container: .byte 1

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


; main keeps scanning the keypad to find which key is pressed.
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
	; we need to look again
	rcall convert ; if bit is clear, convert the bitcode
	jmp keypad ; and start again
skipconv:
	inc row ; else move to the next row
	lsl mask ; shift the mask to the next bit
	jmp rowloop          
nextcol:     
	cpi col, 3 ; check if we^Òre on the last column
	brne Continue ; if so, no buttons were pushed,

	ldi xl, low(buffer)
	ldi xh, high(buffer)
	ld temp2, X
	cpi  temp2, 0
	breq keypad
	clr xl
	clr xh

	; so start again.
	ret;return to caller
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
	mov temp, row ; otherwise we have a number (1-9)
	lsl temp ; temp = row * 2
	add temp, row ; temp = row * 3
	add temp, col ; add the column address
	; to get the offset from 1
	inc temp ; add 1. Value of switch is
	; row*3 + col + 1.
	jmp convert_end
letters:
	ldi temp, 0xA
	add temp, row ; increment from 0xA by the row value
	jmp convert_end
	symbols:
	cpi col, 0 ; check if we have a star
	breq star
	cpi col, 1 ; or if we have zero
	breq zero
	ldi temp, 0xF ; we'll output 0xF for hash
	jmp convert_end
star:
	ldi temp, 0xE ; we'll output 0xE for star
	jmp convert_end
zero:
	clr temp ; set to zero
convert_end:
	ldi xl, low(buffer)
	ldi xh, high(buffer)
	ld temp2, X
	cpi  temp2, 0
	brne convert_ret
	std X, temp
	clr xl
	clr xh
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



get_chars:
	clr r17;set r17 as buffer of get_chars
	clr i
	; initialize buffer
	ldi xl, low(buffer)
	ldi xh, high(buffer)
	clr temp2
	st X, temp2

	sbrs mode, 0
	rjmp character_mode
	rjmp number_mode



	character_mode:
		cpi i, 11
		brlo not_error_char
		jmp error_handler
	not_error_char:
		rcall keypad

		ldi xl, low(buffer)
		ldi xh, high(buffer)
		ld temp, X
		clr temp2
		st X, temp2; initialize buffer

		cpi i,0;
		brne compare_A; clear LCD
		do_lcd_command 0b00000001 ; clear display
		do_lcd_command 0b00000110 ; increment, no display shift
		do_lcd_command 0b00001110 ; Cursor on, bar, no blink

		compare_A:
			cpi temp, 0b01000001
			brne compare_B
			ldi r17,0b01000000; A-1
			rjmp compare_end
		compare_B:
			cpi temp, 0b01000010
			brne compare_C
			ldi r17,0b01001001; J-1
			rjmp compare_end
		compare_C:
			cpi temp, 0b01000011
			brne compare_D
			ldi r17,0b01010010; S-1
		compare_D:
			cpi temp, 0b01000100
			brne is_buffer_zer0

		end_get_chars:
			ldi temp, 00111011
			ldi xl, low(String_container)
			ldi xh, high(String_container)
			std x+i, temp
			ret
			
		compare_end:
			jmp character_mode

		is_buffer_zer0:
		cpi r17, 0
		brne character_loaded
		jmp character_mode

			character_loaded:
				cpi temp, 0
				brne temp_not_zero
				jmp character_mode

				temp_not_zero:
					cpi temp, 11
					brlo load_end
					jmp character_mode

					load_end:
						add temp, buffer
						ldi xl, low(String_container)
						ldi xh, high(String_container)
						std x+i, temp
						do_lcd_data temp
						inc i
						jmp character_mode


	number_mode:
		cpi i, 3
		brlo not_error_number
		jmp error_handler
	not_error_number:
		rcall keypad
		ldi xl, low(buffer)
		ldi xh, high(buffer)
		ld temp, X
		clr temp2
		st X, temp2; initialize buffer

		cpi i,0;
		brne number_compare_D; clear LCD
		do_lcd_command 0b00000001 ; clear display
		do_lcd_command 0b00000110 ; increment, no display shift
		do_lcd_command 0b00001110 ; Cursor on, bar, no blink

		number_compare_D:
			cpi temp, 0b01000100
			brne is_i_equal_to_two
			jmp number_end

			is_i_equal_to_two:
				cpi i,2
				brne temp_test
				jmp error_handler

				temp_test:
					cpi temp, 10
					brlo number_continue
					jmp number_mode

				number_continue:
					cpi r17,0
					brne sum
					cpi temp,0
					brne load
					jmp number_mode

					load:
						mov r17, temp
						inc i
						clr temp
						jmp number_mode
					sum:
						ten_times r17
						add r17, temp
						inc i
						clr temp
						jmp number_mode
		number_end:
			cpi r17, 11
			brsh error_handler
			ldi xl,low(Number_container)
			ldi xh,high(Number_container)
			st X,r17
			ret
	error_handler:
		do_lcd_command 0b00000001 ; clear display
		do_lcd_command 0b00000110 ; increment, no display shift
		do_lcd_command 0b00001110 ; Cursor on, bar, no blink
		do_lcd_data 'I'
		do_lcd_data 'n'
		do_lcd_data 'c'
		do_lcd_data 'o'
		do_lcd_data 'r'
		do_lcd_data 'r'
		do_lcd_data 'e'
		do_lcd_data 'c'
		do_lcd_data 't'
		do_lcd_data '!'
		jmp get_chars

main: