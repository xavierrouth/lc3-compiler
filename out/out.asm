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
    AND R0, R0, #0
    ADD R0, R0, #1
    AND R1, R1, #0
    ADD R1, R1, #2
    NOT R0, R0                          ; evaluate '>'
    ADD R0, R0, #1
    ADD R1, R1, R0

    AND R1, R1, R1                      ; load condition into NZP
    BRnz main.if.0.end                  ; if false, jump over if statement

    AND R1, R1, #0
    ADD R1, R1, #3
    STI R1, RETURN_SLOT                 ; write return value from main

main.if.0.end

    AND R0, R0, #0
    ADD R0, R0, #6
    STI R0, RETURN_SLOT                 ; write return value from main

    HALT

; ------ Data Section ------ 
USER_STACK        .FILL xFDFF
RETURN_SLOT       .FILL xFDFF
.END
