;
; monorail_emulator.asm
;
; Created: 20/10/2017 6:25:51 PM
; Author : Edward Thomsonn, Zhiwei Cao

;Board settings: 
;Connect the four columns C0-C3 of the keypad to PL3-PL0 of PORTL and the four rows R0-R3 to PL7-PL4 of PORTL.
;For LCD data connect D0-D7 to PF0-PF7 respectively and for LCD commands connect BE,RW,E,RS to PA4-PA7 respectively

.include "m2560def.inc"
.def row =r17
.def col =r18
.def mask =r19
.def temp2 =r20
.def temp =r21
.def i = r22
.def mode = r23
;.def assci_zero =r21
.equ PORTLDIR = 0xF0 ;11110000
.equ INITCOLMASK = 0xEF;11101111
.equ INITROWMASK = 0x01;00000001
.equ ROWMASK = 0x0F;00001111


.set LCD_DISP_ON = 0b00001110
.set LCD_DISP_OFF = 0b00001000
.set LCD_DISP_CLR = 0b00000001

.set LCD_FUNC_SET = 0b00111000 						; 2 lines, 5 by 7 characters
.set LCD_ENTR_SET = 0b00000110 						; increment, no display shift
.set LCD_HOME_LINE = 0b10000000 					; goes to 1st line (address 0)

.set LCD_SEC_LINE = 0b10101000 						

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


.dseg
MESSAGE: .byte 17
buffer: .byte 1
String_container: .byte 10
Number_container: .byte 1
num_stations: .byte 1
config_array: .byte 160
config_array_index: .byte 1


.cseg
.org 0x00
jmp RESET
rjmp bypass
	string0: .db "MAX NUM STATION:;"
	string1: .db "NAME STATION;"
	string2: .db "STATION;"
	err_string: .db "INCORRECT;"
bypass:


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

do_lcd_command LCD_FUNC_SET ; 2x5x7
rcall sleep_5ms
do_lcd_command LCD_FUNC_SET ; 2x5x7
rcall sleep_1ms
do_lcd_command LCD_FUNC_SET ; 2x5x7
do_lcd_command LCD_FUNC_SET ; 2x5x7
do_lcd_command LCD_DISP_OFF ; display off?
do_lcd_command LCD_DISP_CLR ; clear display
do_lcd_command LCD_ENTR_SET ; increment, no display shift
do_lcd_command LCD_DISP_ON ; Cursor on, bar, no blink

jmp main

; main keeps scanning the keypad to find which key is pressed.
keypad_start:
push row
push col
push mask
push temp2
push temp
push i
push mode

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

	pop mode
	pop i
	pop temp
	pop temp2
	pop mask
	pop col
	pop row

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
	jmp number_convert
letters:
	ldi temp, 'A'
	add temp, row ; increment from 0xA by the row value
	jmp convert_end
symbols:
	cpi col, 0 ; check if we have a star
	breq star
	cpi col, 1 ; or if we have zero
	breq zero
	ldi temp, 0b00100011 ; we'll output 0xF for hash
	jmp convert_end
star:
	ldi temp, 0b00101010; we'll output 0xE for star
	jmp convert_end
zero:
	clr temp ; set to zero

number_convert:
	ldi temp2, '0'
	add temp, temp2
	clr temp2

convert_end:
	ldi xl, low(buffer)
	ldi xh, high(buffer)
	ld temp2, X
	cpi  temp2, 0
	brne convert_ret
	st X, temp
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
	push r18
	push r19
	push r20

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
	pop r20
	pop r19
	pop r18
	ret


get_chars: ;r17 mode=r
	push r17
	push temp
	push temp2
get_chars_start:
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
		rcall keypad_start

		ldi xl, low(buffer)
		ldi xh, high(buffer)
		ld temp, X
		clr temp2
		st X, temp2; initialize buffer

		cpi temp,'0'
		brlo compare_A
		cpi temp,0b00111010 ; character after '9'
		brsh compare_A
		ldi temp2,'0'
		sub temp, temp2
		clr temp2

		compare_A:
			cpi temp, 'A'
			brne compare_B
			ldi r17,0b01000000; A-1
			rjmp compare_end
		compare_B:
			cpi temp, 'B'
			brne compare_C
			ldi r17,0b01001001; J-1
			rjmp compare_end
		compare_C:
			cpi temp, 'C'
			brne compare_D
			ldi r17,0b01010010; S-1
		compare_D:
			cpi temp, 'D'
			brne block_number_first
			cpi i,0
			brne end_get_chars
			jmp character_mode

		end_get_chars:
			ldi temp, 0b00111011 ; ';'
			ldi xl, low(String_container)
			ldi xh, high(String_container)

			push r15
			clr r15
			add xl, i
			adc xh, r15
			pop r15

			st x, temp
			jmp end_get_char
			
		compare_end:
			jmp character_mode

		block_number_first:
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
						
						add temp, r17
						ldi xl, low(String_container)
						ldi xh, high(String_container)

						cpi temp, '[';0b01011011
						brne load_continue
						ldi temp,' ';0b00100000
						load_continue:

						push r15
						clr r15
						add xl, i
						adc xh, r15
						pop r15

						st x, temp
						do_lcd_data temp
						inc i
						clr r17
						jmp character_mode


	number_mode:
		cpi i, 3
		brlo not_error_number
		jmp error_handler
	not_error_number:
		rcall keypad_start
		ldi xl, low(buffer)
		ldi xh, high(buffer)
		ld temp, X
		clr temp2
		st X, temp2; initialize buffer

		cpi temp,'0'
		brlo number_compare_D
		cpi temp,0b00111010 ; character after '9'
		brsh number_compare_D
		ldi temp2,'0'
		sub temp, temp2
		clr temp2


		number_compare_D:
			cpi temp, 'D'
			brne is_i_equal_to_two
			jmp is_number_zero

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
						
						ldi temp2,'0'
						add temp2, temp
						do_lcd_data temp2
						clr temp2

						inc i
						clr temp
						jmp number_mode
					sum:
						clr r18
						ldi r18,10
						mul r17,r18
						mov r17,r0

						add r17,temp
						ldi temp2,'0'
						add temp2, temp
						do_lcd_data temp2
						clr temp2

						inc i
						clr temp
						jmp number_mode

		is_number_zero:
		cpi r17,0
		brne number_end
		jmp number_mode

		number_end:
			cpi r17, 11
			brsh error_handler


			ldi xl,low(Number_container)
			ldi xh,high(Number_container)
			st X,r17
			jmp end_get_char
	error_handler:
		do_lcd_command 0b00000001 ; clear display
		do_lcd_command 0b00000110 ; increment, no display shift
		do_lcd_command 0b00001110 ; Cursor on, bar, no blink

		clr r17
		ldi zl, low(err_string<<1)
		ldi zh, high(err_string<<1)

		lpm r17, z+
		display_err:
			cpi r17, ';'
			breq end_err
			do_lcd_data r17
			lpm r17, z+
		jmp display_err
		end_err:
		do_lcd_command  0b11000000 ;print("\n") -> newline
		jmp get_chars_start

		end_get_char:
			pop temp2
			pop temp
			pop r17
			ret

display_message: ;(&message=Y)
	push xl
	push xh
	push r16

	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink

	clr r16
	ldi xl, low(MESSAGE)
	ldi xh, high(MESSAGE)

	ld r16, x+
	display_char:
		cpi r16, ';'
		breq end_str
		do_lcd_data r16
		ld r16, x+
	jmp display_char

	end_str:
	do_lcd_command  0b11000000 ;print("\n") -> newline
	pop r16
	pop xh
	pop xl
	ret

store_result: ;(&result, &config_array, &start_point)
	push xl
	push xh
	push zl
	push zh
	push r16
	push r17
	

	ldi zl, low(config_array_index)
	ldi zh, high(config_array_index)
	ld r17, z
	
	ldi zl, low(config_array)
	ldi zh, high(config_array)
	clr r16
	add zl, r17
	adc zh, r16

	sbrs mode, 0
	rjmp store_characters
	rjmp store_numbers

	store_characters:
	ldi xl, low(String_container)
	ldi xh, high(String_container)
	
	ld r16, x+
	store_char_loop:
		cpi r16, ';'
		breq end_store_char_loop
		st z+, r16
		ld r16, x+
	jmp store_char_loop

	end_store_char_loop:
	st z+, r16
	
	jmp store_result_return

	store_numbers:
	ldi xl,low(Number_container)
	ldi xh,high(Number_container)
	ld r16, x
	st z, r16
	jmp store_result_return

store_result_return:
	pop r17
	pop r16
	pop zh
	pop zl
	pop xh
	pop xl
	ret
infloop: rjmp infloop
main:
	; asks first question 'Please type the maximum number of stations:' 
	ldi xl, low(MESSAGE)
	ldi xh, high(MESSAGE)
	ldi zl, low(string0<<1)
	ldi zh, high(string0<<1)

	get_string0:
		lpm r16, z+
		cpi r16, ';'
		breq end_get_string0
		st x+, r16
		rjmp get_string0
	end_get_string0:
		st x+, r16
	
	rcall display_message ;MESSAGE will hold the message to send to LCD

	ldi r16, 'G'
	do_lcd_data r16

	ser mode
	rcall get_chars ;return result
	ldi xl, low(Number_container)
	ldi xh, high(Number_container)
	ldi zl, low(num_stations)
	ldi zh, high(num_stations)

	ld r15, x
	st z, r15
	rcall store_result;(&result, &config_array, mode)

clr r14
inc r14
get_station_name:
	cp r15, r14
	brlo end_get_station_name
	ldi xl, low(MESSAGE)
	ldi xh, high(MESSAGE)
	ldi zl, low(string1<<1)
	ldi zh, high(string1<<1)


	get_string1:
		lpm r16, z+
		cpi r16, ';'
		breq end_get_string1
		st x+, r16
		rjmp get_string1
	end_get_string1:
		clr r16
		ldi r16, '0'
		add r16, r14
		st x+, r16
		ldi r16, ';'
		st x+, r16

	rcall display_message ;MESSAGE will hold the message to send to LCD
	clr mode
	rcall get_chars ;return result
	rcall store_result;(&result, &config_array)
	inc r14
rjmp get_station_name
	end_get_station_name:

clr r14
get_travel_time:
	cp r14, r15
	brsh end_get_travel_time
	ldi xl, low(MESSAGE)
	ldi xh, high(MESSAGE)
	ldi zl, low(string2<<1)
	ldi zh, high(string2<<1)


	get_string2:
		lpm r16, z+
		cpi r16, ';'
		breq end_get_string2
		st x+, r16
		rjmp get_string2
	end_get_string2:
		clr r16
		ldi r16, '0'
		add r16, r14
		st x+, r16

		ldi r16, '-'
		st x+, r16

	get_station_to:
		lpm r16, z+
		cpi r16, ';'
		breq end_get_station_to
		st x+, r16
		rjmp get_station_to
	end_get_station_to:
		clr r16
		ldi r16, '0'
		add r16, r14
		st x+, r16

		ldi r16, ';'
		st x+, r16

	rcall display_message ;MESSAGE will hold the message to send to LCD
	clr mode
	rcall get_chars ;return result
	rcall store_result;(&result, &config_array)
	inc r14
rjmp get_travel_time
	end_get_travel_time:


inf: rjmp inf
