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
    ADD R6, R6, #-15                    ; allocate space for 'a'

    AND R0, R0, #0
    ADD R0, R0, #5
    ADD R1, R5, #-14                    ; load base of array access for 'a'
    AND R2, R2, #0
    ADD R2, R2, #10
    ADD R3, R1, R2                      ; calculate index into array
    STR R0, R3, #0

    ADD R0, R5, #-14                    ; load base of array access for 'a'
    AND R1, R1, #0
    ADD R1, R1, #10
    ADD R2, R0, R1                      ; calculate index into array
    LDR R2, R2, #0                      ; load element from array
    STI R2, RETURN_SLOT                 ; write return value from main
    HALT

    HALT

; ------ Data Section ------ 
USER_STACK        .FILL xFDFF
RETURN_SLOT       .FILL xFDFF
.END
