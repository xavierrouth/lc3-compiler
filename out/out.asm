; ---------------------------------------------------------------------------
; C-LC3 Compiler, by HKN for UIUC Students                                   
; Disclaimer: Not all C features are supported by this compiler. Do not base 
; assumptions about valid C programming on what you see here.                
; Please report any bugs or unexpected crashes to <xrouth2@illinois.edu>     
; ---------------------------------------------------------------------------
main
    LDR R0, R5, #0,                              ; load local variable or parameter
    ADD R0, R0, #3
    ADD R0, R0, #7
    AND R1, R1, #0
    ADD R1, R1, #6
    ADD R1, R1, #3
; ------ Data Section ------ 
