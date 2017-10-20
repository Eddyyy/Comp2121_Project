;
; monorail_emulator.asm
;
; Created: 20/10/2017 6:25:51 PM
; Author : Edward Thomsonn, Zhwei Cao
;
.include "m2560def.inc"

start:
    inc r16
    rjmp start
