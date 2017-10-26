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



.macro clear
	ldi YL, low(@0) ; load the memory address to Y pointer
	ldi YH, high(@0)
	clr r16 ; set temp to 0
	st Y+, r16 ; clear the two bytes at @0 in SRAM
	st Y, r16
.endmacro


.dseg
	MESSAGE: .byte 17
	buffer: .byte 1
	String_container: .byte 10
	Number_container: .byte 1
	num_stations: .byte 1
	config_array: .byte 160
	config_array_index: .byte 1
	stop_time: .byte 1
	SecondCounter: .byte 2 
	TempCounter: .byte 2 
	Flash_flag: .byte 1
	Stop_next_station: .byte 1
	Stop_flag: .byte 1
	ON_Simulation: .byte 1
	Motor_speed: .byte 1
	Speed: .byte 1
.cseg
.org 0x00
jmp RESET
.org INT0addr
jmp PB_0
.org INT1addr
jmp PB_1
.org INT2addr
   jmp motor_speed_detective
.org OVF0addr ; OVF0addr is the address of Timer0 Overflow Interrupt Vector
jmp Timer0OVF ; jump to the interrupt handler for Timer0 overflow.

rjmp bypass
	string0: .db "MAX NUM STATION:;"
	string1: .db "NAME STATION;"
	string2: .db "STAT;"
	string3: .db "STOP TIME:;"
	err_string: .db "INCORRECT;"
bypass:
jmp DEFAULT ; default service for all other interrupts.
DEFAULT: reti ; no interrupt handling 

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

clear TempCounter ; initialize the temporary counter to 0
clear SecondCounter ; initialize the second counter to 0
clr r18
clr r19

ldi xl, low(Stop_flag)
ldi xh, high(Stop_flag)
st x, r18

ldi xl, low(ON_Simulation)
ldi xh, high(ON_Simulation)
clr r18
st x, r18

ldi xl, low(Speed)
ldi xh, high(Speed)
ser r18
st x, r18
clr r18


 clear TempCounter ; initialize the temporary counter to 0
 clear SecondCounter ; initialize the second counter to 0
 clr r18
 clr r19
 ldi r18, 1
 ldi xl, low(Flash_flag)
 ldi xh, high(Flash_flag)
 st x, r18
clr r22
clr r23
ldi r16, (1<<PE4)		;labeled PE2 acctully PE4 
out DDRE, r16
clr r16

sts OCR3BL, r16
sts OCR3BH, r16
ldi r16, (1 << ISC21 | 1 << ISC11 | 1 << ISC01)      ; set INT2 as falling-
sts EICRA, r16             ; edge triggered interrupt
in r16, EIMSK              ; enable INT2
ori r16, (1<<INT2 | 1<<INT1 | 1<<INT0)
out EIMSK, r16
;set timer interrupt

ldi r16, (1<< WGM30)|(1<<COM3B1) ; set the Timer3 to Phase Correct PWM mode.
sts TCCR3A, r16
ldi r16, (1 << CS32)
sts TCCR3B, r16		; Prescaling value=8



ldi r16, 0b00000000
out TCCR0A, r16
ldi r16, 0b00000010
out TCCR0B, r16 ; set prescalar value to 8
ldi r16, 1<<TOIE0 ; TOIE0 is the bit number of TOIE0 which is 0
sts TIMSK0, r16 ; enable Timer0 Overflow Interrupt
ser temp
out DDRC, temp
clr temp
out PORTC, temp
out DDRD, temp
out PORTD, temp
ldi temp, (2 << ISC10) | (2 << ISC00)
sts EICRA, temp
in temp, EIMSK
ori temp, (1<<INT0) | (1<<INT1)
out EIMSK, temp
sei
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

sleep_50ms:
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	ret
sleep_200ms:
	rcall sleep_50ms
	rcall sleep_50ms
	rcall sleep_50ms
	rcall sleep_50ms
	ret
sleep_1s:
	rcall sleep_200ms
	rcall sleep_200ms
	rcall sleep_200ms
	rcall sleep_200ms
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
	ldi yl, low(String_container)
	ldi yh, high(String_container)
	ldi zl,low(Number_container)
	ldi zh,high(Number_container)
	
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


			push r15
			clr r15
			add xl, i
			adc xh, r15
			pop r15

			st y+, temp
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
					
						cpi temp, '[';0b01011011
						brne load_continue
						ldi temp,' ';0b00100000
						load_continue:

						

						st y+, temp
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


			st z,r17
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
	lsl r17
	lsl r17
	lsl r17
	lsl r17

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

	push r17
	push r18
	ldi r17, 11
	clr r18
	add zl, r17
	adc zh, r18 
	pop r18
	pop r17
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

;The timer


 Timer0OVF: ; interrupt subroutine to Timer0
	 push r16
	 in r16, SREG
	 push r16 ; prologue starts
	 push YH ; save all conflicting registers in the prologue
	 push YL
	 push r25
	 push r24 ; prologue ends
	 push r19
	 push xl
	 push xh
	 push r23
	 push r18
	 push r17
	 ; Load the value of the temporary counter
	 lds r24, TempCounter
	 lds r25, TempCounter+1
	 adiw r25:r24, 1 ; increase the temporary counter by one

	 cpi r24, low(2600) ; check if (r25:r24) = 7812
	 ldi r16, high(2600) ; 7812 = 106/128
	 cpc r25, r16
	 breq is_simulation
	jmp NotSecond
	is_simulation:
	ldi xl,low(ON_Simulation)
	ldi xh,high(ON_Simulation)
	ld r19, x
	cpi r19,1
	breq Desicion
	jmp endinc

	Desicion:
	ldi xl,low(Stop_flag)
	ldi xh,high(Stop_flag)
	ld r19, x
	cpi r19,1
	breq flashing


	jmp Motor_mode
	Motor_mode:
	ldi r19, 0b00000000
	out PORTC, r19
	ldi xl, low(Motor_speed)
	ldi xh, high(Motor_speed)
	ld r18, x
	ldi xl, low(Speed)
	ldi xh, high(Speed)
	ld r19, x
	cpi r18,80

	brlo accerlation
	decerlation:
	sts OCR3BL, r19
	ldi r17,3
	sub r19,r17

	rjmp end_motor_mode
	accerlation:
	sts OCR3BL, r19
	ldi r17,3
	add r19,r17

	end_motor_mode:

	ldi xl, low(Speed)
	ldi xh, high(Speed)
	st x, r19
	ldi xl, low(Motor_speed)
	ldi xh, high(Motor_speed)
	clr r19
	st x,r19
	rjmp endinc
	flashing:
	ldi xl, low(Flash_flag)
	ldi xh, high(Flash_flag)
	ld r17,x
	lds r18, SecondCounter
	cpi r17,0
	breq flash_2
	flash_1:
	ldi r19, 0b00000001
	out PORTC, r19
	clr r17
	rjmp endinc
	flash_2:
	ldi r19, 0b00000010
	out PORTC, r19
	ldi r17,1

endinc:
	 st x,r17
	 clear TempCounter ; reset the temporary counter
	 ; Load the value of the second counter
	 lds r24, SecondCounter
	 lds r25, SecondCounter+1
	 adiw r25:r24, 1 ; increase the second counter by one
	 sts SecondCounter, r24
	 sts SecondCounter+1, r25
	 rjmp EndIF

NotSecond: ; store the new value of the temporary counter
	 sts TempCounter, r24
	 sts TempCounter+1, r25
EndIF:
	 pop r17
	 pop r18
	 pop r23
	 pop xh
	 pop xl
	 pop r19
	 pop r24 ; epilogue starts
	 pop r25 ; restore all conflicting registers from the stack
	 pop YL
	 pop YH
	 pop r16
	 out SREG, r16
	 pop r16
	 reti ; return from the interrupt

motor_speed_detective:
	push r16
	in r16, SREG
	push r16
	push r21
	push xl
	push xh
	ldi xl, low(Motor_speed)
	ldi xh, high(Motor_speed)
	ld r21, x
	inc r21
	st x,r21
	pop xh
	pop xl
	pop r21
	pop r16
	out SREG, r16
	pop r16
	reti

PB_0:
	push xl
	push xh
	push r16
	ldi xl,low(Stop_next_station)
	ldi xh,high(Stop_next_station)
	ldi r16,1
	st x, r16
	pop r16
	pop xh
	pop xl
	reti

PB_1:
	push xl
	push xh
	push r16
	ldi xl,low(Stop_next_station)
	ldi xh,high(Stop_next_station)
	ldi r16,1
	st x, r16
	pop r16
	pop xh
	pop xl
	reti





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

	ser mode
	rcall get_chars ;return result
	ldi xl, low(Number_container)
	ldi xh, high(Number_container)
	ldi zl, low(num_stations)
	ldi zh, high(num_stations)

	ld r15, x
	st z, r15

ldi zl, low(num_stations)
ldi zh, high(num_stations)
ld r15, z
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
		cpi r16, ':'
		breq store_ten0
		st x+, r16
		rjmp continue_ten0
		store_ten0:
		ldi r16, '1'
		st x+, r16
		ldi r16, '0'
		st x+, r16

		continue_ten0:
		ldi r16, ';'
		st x+, r16

	rcall display_message ;MESSAGE will hold the message to send to LCD
	clr mode
	rcall get_chars ;return result

	dec r14
	ldi xl, low(config_array_index)
	ldi xh, high(config_array_index)
	st x,r14
	inc r14

	rcall store_result;(&result, &config_array, &index)
	inc r14
rjmp get_station_name
	end_get_station_name:

ldi zl, low(num_stations)
ldi zh, high(num_stations)
ld r15, z
clr r14
inc r14
get_travel_time:
	cp r15, r14
	brlo long_jump0

	rjmp next
	long_jump0:
	jmp end_get_travel_time

	next:
	ldi xl, low(MESSAGE)
	ldi xh, high(MESSAGE)
	ldi zl, low(string2<<1)
	ldi zh, high(string2<<1)
	get_station_from: ;unpack string2
		lpm r16, z+
		cpi r16, ';'
		breq end_get_station_from
		st x+, r16
		rjmp get_station_from
	end_get_station_from:
		clr r16
		ldi r16, '0'
		add r16, r14
		cpi r16, ':'
		breq store_ten1
		st x+, r16
		rjmp continue_ten1

		store_ten1:
		ldi r16, '1'
		st x+, r16
		ldi r16, '0'
		st x+, r16

		continue_ten1:
		ldi r16, '-'
		st x+, r16

	ldi zl, low(string2<<1)
	ldi zh, high(string2<<1)
	get_station_to: ;unpack string2
		lpm r16, z+
		cpi r16, ';'
		breq end_get_station_to
		st x+, r16
		rjmp get_station_to
	end_get_station_to:
		clr r16
		ldi r16, '0'
		add r16, r14
		inc r16

		cp r14, r15 ;r16<=r15 === r15>=r16
		breq load_one
		cpi r16, ':'
		breq store_ten2
		st x+, r16
		rjmp second_continue

		load_one:
		ldi r16, '1'
		st x+, r16
		rjmp second_continue

		store_ten2:
		ldi r16, '1'
		st x+, r16
		ldi r16, '0'
		st x+, r16
		rjmp second_continue

	second_continue:
		ldi r16, ';'
		st x+, r16

	rcall display_message ;MESSAGE will hold the message to send to LCD
	ser mode
	rcall get_chars ;return result

	dec r14
	ldi xl, low(config_array_index)
	ldi xh, high(config_array_index)
	st x,r14
	inc r14


	rcall store_result;(&result, &config_array, &index)
	inc r14
rjmp get_travel_time
	end_get_travel_time:

	; asks first question 'Please type the maximum number of stations:' 
	ldi xl, low(MESSAGE)
	ldi xh, high(MESSAGE)
	ldi zl, low(string3<<1)
	ldi zh, high(string3<<1)

	get_string3:
		lpm r16, z+
		cpi r16, ';'
		breq end_get_string3
		st x+, r16
		rjmp get_string3
	end_get_string3:
		st x+, r16
	
	rcall display_message ;MESSAGE will hold the message to send to LCD

	ser mode
	rcall get_chars ;return result
	ldi xl, low(Number_container)
	ldi xh, high(Number_container)
	ldi zl, low(stop_time)
	ldi zh, high(stop_time)

	ld r15, x
	st z, r15


ldi xl, low(ON_Simulation)
ldi xh, high(ON_Simulation)
ldi r18,1
st x, r18
clr r18


Monorail_emulation_part:
	ldi zl, low(num_stations)
	ldi zh, high(num_stations)
	ld r15, z; r15 represent Max_station
	clr r14 ; current_station
	Station_loop:
		cp r14,r15
		brlo station_continue
		jmp Monorail_emulation_part

		station_continue:
		ldi xl, low(config_array_index)
		ldi xh, high(config_array_index)
		st x,r14

		rcall Get_massage
		rcall display_message
		ldi xl, low(Number_container)
		ldi xh, high(Number_container)
		ld r17,x

		ldi r22, '0'
		add r22,r17
		do_lcd_data r22

		clr r16
		driving_now:
		cp r16,r17	
		brsh is_stop
			rcall sleep_1s
			inc r16
			rjmp driving_now
		is_stop:
		clr r16
		clr r17
		clr r18
		ldi xl, low(stop_time)
		ldi xh, high(stop_time)		
		ld r17,x

		ldi xl, low(Stop_next_station)
		ldi xh, high(Stop_next_station)		
		ld r18,x

		cpi r18,1
		breq stop_now
		jmp station_loop_end
		stop_now:
			ldi xl, low(Stop_flag)
			ldi xh, high(Stop_flag)		
			ldi r18,1
			st x, r18
			cp r16,r17
			brsh end_stop
			rcall sleep_1s
			inc r16
			rjmp stop_now
			end_stop:
				ldi xl, low(Stop_flag)
				ldi xh, high(Stop_flag)		
				clr r18
				st x, r18
				ldi xl, low(Stop_next_station)
				ldi xh, high(Stop_next_station)	
				st x, r18

		station_loop_end:
			inc r14
			jmp Station_loop
inf: rjmp inf




Get_massage:; get config_array to MESSAGE
	push r16
	push r17
	push zl
	push zh
	push xl
	push xh

	
	ldi zl, low(config_array_index)
	ldi zh, high(config_array_index)
	ld r17, z
	lsl r17
	lsl r17
	lsl r17
	lsl r17

	ldi zl, low(config_array)
	ldi zh, high(config_array)
	clr r16
	add zl, r17
	adc zh, r16
	ldi xl, low(MESSAGE)
	ldi xh, high(MESSAGE)

	string_copy:
		ld r16, z+
		cpi r16, ';'
		breq string_copy_done
		st x+, r16
		rjmp string_copy
	string_copy_done:
		st x+, r16

	
	ldi zl, low(config_array)
	ldi zh, high(config_array)
	ldi xl, low(Number_container)
	ldi xh, high(Number_container)

	clr r16
	add zl, r17
	adc zh, r16
	ldi r17,11
	add zl, r17
	adc zh, r16

	ld r16,z
	st x,r16

	
	pop xh
	pop xl
	pop zh
	pop zl
	pop r17
	pop r16
	ret