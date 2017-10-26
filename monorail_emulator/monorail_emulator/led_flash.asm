.include "m2560def.inc"
.equ PATTERN = 0b11110000 ; define a pattern for 8 LEDs

.macro clear
	ldi YL, low(@0) ; load the memory address to Y pointer
	ldi YH, high(@0)
	clr r16 ; set temp to 0
	st Y+, r16 ; clear the two bytes at @0 in SRAM
	st Y, r16
.endmacro

.dseg
	SecondCounter: .byte 2 ; two-byte counter for counting seconds.
	TempCounter: .byte 2 ; temporary counter used to determine if one second has passed
.cseg
.org 0x0000
	jmp RESET
	jmp DEFAULT ; no handling for IRQ0.
	jmp DEFAULT ; no handling for IRQ1.
.org OVF0addr ; OVF0addr is the address of Timer0 Overflow Interrupt Vector
	jmp Timer0OVF ; jump to the interrupt handler for Timer0 overflow.
	jmp DEFAULT ; default service for all other interrupts.
	DEFAULT: reti ; no interrupt handling 

RESET: ldi r16, high(RAMEND) ; initialize the stack pointer SP
	 out SPH, r16
	 ldi r16, low(RAMEND)
	 out SPL, r16
	 ser r16 ; set Port C as output
	 out DDRC, r16
	 rjmp main ; jump to main program

 Timer0OVF: ; interrupt subroutine to Timer0
	 in r16, SREG
	 push r16 ; prologue starts
	 push YH ; save all conflicting registers in the prologue
	 push YL
	 push r25
	 push r24 ; prologue ends
	 ; Load the value of the temporary counter
	 lds r24, TempCounter
	 lds r25, TempCounter+1
	 adiw r25:r24, 1 ; increase the temporary counter by one

	 cpi r24, low(2600) ; check if (r25:r24) = 7812
	 ldi r16, high(2600) ; 7812 = 106/128
	 cpc r25, r16
	 brne NotSecond


	lds r18, SecondCounter

	ldi r19,3
	out PORTC, r19
	clr r19
	out PORTC, r19

endinc:
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
	 pop r24 ; epilogue starts
	 pop r25 ; restore all conflicting registers from the stack
	 pop YL
	 pop YH
	 pop r16
	 out SREG, r16
	 reti ; return from the interrupt



 main:

 clear TempCounter ; initialize the temporary counter to 0
 clear SecondCounter ; initialize the second counter to 0
 clr r18
 clr r19

 ldi r16, 0b00000000
 out TCCR0A, r16
 ldi r16, 0b00000010
 out TCCR0B, r16 ; set prescalar value to 8
 ldi r16, 1<<TOIE0 ; TOIE0 is the bit number of TOIE0 which is 0
 sts TIMSK0, r16 ; enable Timer0 Overflow Interrupt
 sei ; enable global interrupt
 loop: rjmp loop ; loop forever