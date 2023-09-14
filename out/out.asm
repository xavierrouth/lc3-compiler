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
    ADD R6, R6, #-2                     ; allocate space for 'node1'

    ADD R6, R6, #-2                     ; allocate space for 'node2'

    ADD R6, R6, #-2                     ; allocate space for 'node3'

    AND R0, R0, #0
    ADD R0, R0, #10
    ADD R1, R5, #-1                     ; calculate address of 'node1'
    ADD R2, R1, #0                      ; calculate index into struct
    STR R0, R2, #0

    AND R0, R0, #0
    ADD R0, R0, #20
    ADD R1, R5, #-3                     ; calculate address of 'node2'
    ADD R2, R1, #0                      ; calculate index into struct
    STR R0, R2, #0

    AND R0, R0, #0
    ADD R0, R0, #30
    ADD R1, R5, #-5                     ; calculate address of 'node3'
    ADD R2, R1, #0                      ; calculate index into struct
    STR R0, R2, #0

    ADD R0, R5, #-3                     ; take address of 'node2'
    ADD R1, R5, #-1                     ; calculate address of 'node1'
    ADD R2, R1, #1                      ; calculate index into struct
    STR R0, R2, #0

    ADD R0, R5, #-5                     ; take address of 'node3'
    ADD R1, R5, #-3                     ; calculate address of 'node2'
    ADD R2, R1, #1                      ; calculate index into struct
    STR R0, R2, #0

    AND R0, R0, #0
    ADD R1, R5, #-5                     ; calculate address of 'node3'
    ADD R2, R1, #1                      ; calculate index into struct
    STR R0, R2, #0

    ADD R6, R6, #-2                     ; allocate space for 'current'
    ADD R0, R5, #-1                     ; take address of 'node1'
    STR R0, R5, #-7                     ; initialize 'current'

    ADD R6, R6, #-1                     ; allocate space for 'sum'
    AND R0, R0, #0
    STR R0, R5, #-8                     ; initialize 'sum'

; for loop initialization
    ADD R6, R6, #-1                     ; allocate space for 'i'
    AND R0, R0, #0
    STR R0, R5, #-9                     ; initialize 'i'
main.for.0                              ; test condition
    AND R0, R0, #0
    ADD R0, R0, #1
    AND R0, R0, R0                      ; load condition into NZP
    BRnz main.for.0.end                 ; if false, skip over loop body
    AND R0, R0, #0
    ADD R0, R0, #1
    ADD R1, R5, #-7                     ; calculate address of 'current'
    LDR R1, R1, #0                      ; dereference struct pointer
    ADD R2, R1, #1                      ; calculate index into struct
    LDR R2, R2, #0                      ; load element next from struct
    NOT R2, R2                          ; evaluate '<'
    ADD R2, R2, #1
    ADD R2, R2, R0

    AND R2, R2, R2                      ; load condition into NZP
    BRnz main.if.0.end                  ; if false, jump over if statement

    AND R0, R0, #0
    ADD R0, R0, #1
    ADD R1, R5, #-7                     ; calculate address of 'current'
    LDR R1, R1, #0                      ; dereference struct pointer
    ADD R2, R1, #1                      ; calculate index into struct
    LDR R2, R2, #0                      ; load element next from struct
    NOT R0, R0                          ; evaluate '>'
    ADD R0, R0, #1
    ADD R2, R2, R0

    AND R2, R2, R2                      ; load condition into NZP
    BRnz main.if.0.end                  ; if false, jump over if statement

    LDR R0, R5, #-8                     ; load local variable 'sum'
    STI R0, RETURN_SLOT                 ; write return value from main
    HALT

main.if.0.end

main.if.0.end

    LDR R0, R5, #-8                     ; load local variable 'sum'
    ADD R1, R5, #-7                     ; calculate address of 'current'
    LDR R1, R1, #0                      ; dereference struct pointer
    ADD R2, R1, #0                      ; calculate index into struct
    LDR R2, R2, #0                      ; load element data from struct
    ADD R0, R0, R2
    STR R0, R5, #-8                     ; assign to variable sum

    ADD R0, R5, #-7                     ; calculate address of 'current'
    LDR R0, R0, #0                      ; dereference struct pointer
    ADD R1, R0, #1                      ; calculate index into struct
    LDR R1, R1, #0                      ; load element next from struct
    STR R1, R5, #-7                     ; assign to variable current

; update expression
    ADD R0, R5, #-9                     ; calculate address of 'i'
    BR   main.for.0                     ; loop
main.for.0.end

    LDR R0, R5, #-8                     ; load local variable 'sum'
    ADD R1, R5, #-7                     ; calculate address of 'current'
    LDR R1, R1, #0                      ; dereference struct pointer
    ADD R2, R1, #0                      ; calculate index into struct
    LDR R2, R2, #0                      ; load element data from struct
    ADD R0, R0, R2
    STR R0, R5, #-8                     ; assign to variable sum

    ADD R0, R5, #-7                     ; calculate address of 'current'
    LDR R0, R0, #0                      ; dereference struct pointer
    ADD R1, R0, #1                      ; calculate index into struct
    LDR R1, R1, #0                      ; load element next from struct
    STR R1, R5, #-7                     ; assign to variable current

    LDR R0, R5, #-8                     ; load local variable 'sum'
    ADD R1, R5, #-7                     ; calculate address of 'current'
    LDR R1, R1, #0                      ; dereference struct pointer
    ADD R2, R1, #0                      ; calculate index into struct
    LDR R2, R2, #0                      ; load element data from struct
    ADD R0, R0, R2
    STR R0, R5, #-8                     ; assign to variable sum

    ADD R0, R5, #-7                     ; calculate address of 'current'
    LDR R0, R0, #0                      ; dereference struct pointer
    ADD R1, R0, #1                      ; calculate index into struct
    LDR R1, R1, #0                      ; load element next from struct
    STR R1, R5, #-7                     ; assign to variable current

    LDR R0, R5, #-8                     ; load local variable 'sum'
    ADD R1, R5, #-7                     ; calculate address of 'current'
    LDR R1, R1, #0                      ; dereference struct pointer
    ADD R2, R1, #0                      ; calculate index into struct
    LDR R2, R2, #0                      ; load element data from struct
    ADD R0, R0, R2
    STR R0, R5, #-8                     ; assign to variable sum

    LDR R0, R5, #-8                     ; load local variable 'sum'
    STI R0, RETURN_SLOT                 ; write return value from main
    HALT


; ------ Data Section ------ 
USER_STACK        .FILL xFDFF
RETURN_SLOT       .FILL xFDFF
.END
