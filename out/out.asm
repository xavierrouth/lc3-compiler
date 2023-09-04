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

    AND R0, R0, #0
    ADD R0, R0, #10
    STR R0, R5, #-1                     ; assign to variable b
    LDR R1, R5, #-1                     ; load local variable or parameter

    LDR R0, R5, #-1                     ; load local variable or parameter
    STR R0, R5, #0                      ; assign to variable a
    LDR R1, R5, #0                      ; load local variable or parameter

    HALT

; ------ Data Section ------ 
USER_STACK        .FILL xFDFF
RETURN_SLOT       .FILL xFDFF
.END
