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

main
    ADD R6, R6, #-1                     ; allocate space for 'a'
    AND R0, R0, #0
    STR R0, R5, #0                      ; initialize 'a'

; for loop initialization
    ADD R6, R6, #-1                     ; allocate space for 'i'
    AND R0, R0, #0
    STR R0, R5, #0                      ; initialize 'i'
main.for.0, ; test condition
    AND R0, R0, #0
    ADD R0, R0, #10
    LDR R1, R5, #0                      ; load local variable 'i'
    NOT R1, R1                          ; evaluate '<'
    ADD R1, R1, #1
    ADD R1, R1, R0
    AND R1, R1, R1                      ; load condition into NZP
    BRnz main.for.0.end                 ; if false, skip over loop body
    LDR R0, R5, #0                      ; load local variable 'a'
    LDR R1, R5, #0                      ; load local variable 'i'
    ADD R0, R0, R1
    STR R0, R5, #0                      ; assign to variable a

; update expression
    LDR R0, R5, #0                      ; load local variable 'i'
    ADD R0, R0, #1
    STR R0, R5, #0                      ; assign to variable i
    BR   main.for.0                     ; loop

    LDR R0, R5, #0                      ; load local variable 'a'
    STI R0, RETURN_SLOT                 ; write return value from main
    HALT


; ------ Data Section ------ 
USER_STACK        .FILL xFDFF
RETURN_SLOT       .FILL xFDFF
.END
