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

    ADD R6, R6, #-1                     ; allocate space for 'b'
    LDR R0, R5, #0                      ; load local variable or parameter
    STR R0, R5, #-10                    ; initialize 'b'

    ADD R6, R6, #-1                     ; allocate space for 'c'

    LDR R0, R5, #-10                    ; load local variable or parameter
    LDR R1, R5, #-11                    ; load local variable or parameter
    ADD R0, R0, R1
    STR R0, R5, #0                      ; assign to variable a
    LDR R1, R5, #0                      ; load local variable or parameter

    HALT

; ------ Data Section ------ 
USER_STACK        .FILL xFDFF
RETURN_SLOT       .FILL xFDFF
.END
