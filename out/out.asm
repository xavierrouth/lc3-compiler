; ---------------------------------------------------------------------------
; C-LC3 Compiler, by HKN for UIUC Students                                   
; Disclaimer: Not all C features are supported by this compiler. Do not base 
; assumptions about valid C programming on what you see here.                
; Please report any bugs or unexpected crashes to <xrouth2@illinois.edu>     
; To simulate output, use https://wchargin.com/lc3web/                       
; ---------------------------------------------------------------------------
.ORIG x3000
    LD  R6, USER_STACK
    ADD R5, R6, #-1
    JSR main

printx
; callee setup:
    ADD R6, R6, #-1                     ; allocate spot for return value
    ADD R6, R6, #-1
    STR R7, R6, #0                      ; push R7 (return address)
    ADD R6, R6, #-1
    STR R5, R6, #0                      ; push R5 (caller frame pointer)
    ADD R5, R6, #-1                     ; set frame pointer

; function body:

    LD  R0, printx.x                    ; load static variable
    ADD R0, R0, #1
    ST  R0, printx.x                    ; assign to static variable

    LEA R0, printx.x                    ; load address of static variable
    STR R0, R5, #3                      ; write return value, always R5 + 3
    BR   printx.teardown

printx.teardown
    ADD R6, R5, #1                      ; pop local variables
    LDR R5, R6, #0                      ; pop frame pointer
    ADD R6, R6, #1
    LDR R7, R6, #0                      ; pop return address
    ADD R6, R6, #1
    RET
; end function.
main
    ADD R6, R6, #-1                     ; allocate space for 'x_ptr'

    JSR printx                          ; call function.
    LDR R0, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #0                      ; pop arguments
    STR R0, R5, #0                      ; assign to variable x_ptr

    JSR printx                          ; call function.
    LDR R0, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #0                      ; pop arguments
    STR R0, R5, #0                      ; assign to variable x_ptr

    LDR R0, R5, #0                      ; load local variable 'x_ptr'
    LDR R0, R0, #0                      ; dereference
    ADD R0, R0, #1
    LDR R1, R5, #0                      ; load local variable 'x_ptr'
    STR R0, R1, #0

    JSR printx                          ; call function.
    LDR R0, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #0                      ; pop arguments

    LDR R0, R5, #0                      ; load local variable 'x_ptr'
    LDR R0, R0, #0                      ; dereference
    STI R0, RETURN_SLOT                 ; write return value from main
    HALT


; ------ Data Section ------ 
printx.x          .FILL #0000
USER_STACK        .FILL xFDFF
RETURN_SLOT       .FILL xFDFF
.END
