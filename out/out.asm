; ---------------------------------------------------------------------------
; C-LC3 Compiler, by HKN for UIUC Students                                   
; Disclaimer: Not all C features are supported by this compiler. Do not base 
; assumptions about valid C programming on what you see here.                
; Please report any bugs or unexpected crashes to <xrouth2@illinois.edu>     
; ---------------------------------------------------------------------------
accumulate
; callee setup:
    AND R6, R6, #-1                     ; allocate spot for return value
    AND R6, R6, #-1
    STR R7, R6, #0                      ; push R7 (return address)
    AND R6, R6, #-1
    STR R7, R6, #0                      ; push R5 (caller frame pointer)
    AND R5, R6, #-1                     ; set frame pointer

; function body:

    LD  R0, accumulate.x                ; load static variable
    ADD R0, R0, #1
    ST  R0, accumulate.x                ; assign to static variable

    LD  R1, accumulate.x                ; load static variable
    STR R1, R5, #3                      ; write return value, always R5 + 3
    BR   accumulate.teardown

accumulate.teardown
    AND R6, R5, #1                      ; pop local variables
    AND R5, R6, #0                      ; pop frame pointer
    AND R6, R6, #1
    AND R7, R6, #0                      ; pop return address
    AND R6, R6, #1
    RET
; end function.
main
; callee setup:
    AND R6, R6, #-1                     ; allocate spot for return value
    AND R6, R6, #-1
    STR R7, R6, #0                      ; push R7 (return address)
    AND R6, R6, #-1
    STR R7, R6, #0                      ; push R5 (caller frame pointer)
    AND R5, R6, #-1                     ; set frame pointer

; function body:
    JSR accumulate                      ; call function.
    LDR R2, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #0                      ; pop arguments

    JSR accumulate                      ; call function.
    LDR R2, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #0                      ; pop arguments

    JSR accumulate                      ; call function.
    LDR R2, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #0                      ; pop arguments

    JSR accumulate                      ; call function.
    LDR R2, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #0                      ; pop arguments

    JSR accumulate                      ; call function.
    LDR R2, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #0                      ; pop arguments
    STR R2, R5, #3                      ; write return value, always R5 + 3
    BR   main.teardown

main.teardown
    AND R6, R5, #1                      ; pop local variables
    AND R5, R6, #0                      ; pop frame pointer
    AND R6, R6, #1
    AND R7, R6, #0                      ; pop return address
    AND R6, R6, #1
    RET
; end function.
; ------ Data Section ------ 
accumulate.x .FILL                    4
