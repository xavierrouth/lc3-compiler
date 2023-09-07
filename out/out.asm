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

maxArray
; callee setup:
    ADD R6, R6, #-1                     ; allocate spot for return value
    ADD R6, R6, #-1
    STR R7, R6, #0                      ; push R7 (return address)
    ADD R6, R6, #-1
    STR R5, R6, #0                      ; push R5 (caller frame pointer)
    ADD R5, R6, #-1                     ; set frame pointer

; function body:
    ADD R6, R6, #-1                     ; allocate space for 'i'

; for loop initialization
    AND R0, R0, #0
    STR R0, R5, #0                      ; assign to variable i
maxArray.for.0                          ; test condition
    AND R0, R0, #0
    ADD R0, R0, #3
    LDR R1, R5, #0                      ; load local variable 'i'
    NOT R1, R1                          ; evaluate '<'
    ADD R1, R1, #1
    ADD R1, R1, R0
    AND R1, R1, R1                      ; load condition into NZP
    BRnz maxArray.for.0.end             ; if false, skip over loop body
    LDR R0, R5, #4                      ; load parameter 'x'
    LDR R1, R5, #0                      ; load local variable 'i'
    ADD R2, R0, R1                      ; calculate index into array
    LDR R2, R2, #0                      ; load element from array
    LDR R0, R5, #5                      ; load parameter 'y'
    LDR R1, R5, #0                      ; load local variable 'i'
    ADD R3, R0, R1                      ; calculate index into array
    LDR R3, R3, #0                      ; load element from array
    NOT R2, R2                          ; evaluate '>'
    ADD R2, R2, #1
    ADD R3, R3, R2

    AND R3, R3, R3                      ; load condition into NZP
    BRnz maxArray.if.0.end              ; if false, jump over if statement

    LDR R0, R5, #5                      ; load parameter 'y'
    LDR R1, R5, #0                      ; load local variable 'i'
    ADD R2, R0, R1                      ; calculate index into array
    LDR R2, R2, #0                      ; load element from array
    LDR R0, R5, #4                      ; load parameter 'x'
    LDR R1, R5, #0                      ; load local variable 'i'
    ADD R3, R0, R1                      ; calculate index into array
    STR R2, R3, #0

maxArray.if.0.end

; update expression
    LDR R0, R5, #0                      ; load local variable 'i'
    ADD R0, R0, #1
    STR R0, R5, #0                      ; assign to variable i
    BR   maxArray.for.0                 ; loop
maxArray.for.0.end

    BR   maxArray.teardown

maxArray.teardown
    ADD R6, R5, #1                      ; pop local variables
    LDR R5, R6, #0                      ; pop frame pointer
    ADD R6, R6, #1
    LDR R7, R6, #0                      ; pop return address
    ADD R6, R6, #1
    RET
; end function.

main
    ADD R6, R6, #-3                     ; allocate space for 'a'

    AND R0, R0, #0
    ADD R0, R0, #1
    ADD R1, R5, #-2                     ; load base of array access for 'a'
    AND R2, R2, #0
    ADD R3, R1, R2                      ; calculate index into array
    STR R0, R3, #0

    AND R0, R0, #0
    ADD R0, R0, #3
    ADD R1, R5, #-2                     ; load base of array access for 'a'
    AND R2, R2, #0
    ADD R2, R2, #1
    ADD R3, R1, R2                      ; calculate index into array
    STR R0, R3, #0

    AND R0, R0, #0
    ADD R0, R0, #4
    ADD R1, R5, #-2                     ; load base of array access for 'a'
    AND R2, R2, #0
    ADD R2, R2, #2
    ADD R3, R1, R2                      ; calculate index into array
    STR R0, R3, #0

    ADD R6, R6, #-3                     ; allocate space for 'b'

    AND R0, R0, #0
    ADD R0, R0, #3
    ADD R1, R5, #-5                     ; load base of array access for 'b'
    AND R2, R2, #0
    ADD R3, R1, R2                      ; calculate index into array
    STR R0, R3, #0

    AND R0, R0, #0
    ADD R0, R0, #1
    ADD R1, R5, #-5                     ; load base of array access for 'b'
    AND R2, R2, #0
    ADD R2, R2, #1
    ADD R3, R1, R2                      ; calculate index into array
    STR R0, R3, #0

    AND R0, R0, #0
    ADD R0, R0, #1
    ADD R1, R5, #-5                     ; load base of array access for 'b'
    AND R2, R2, #0
    ADD R2, R2, #2
    ADD R3, R1, R2                      ; calculate index into array
    STR R0, R3, #0

    ADD R0, R5, #-5                     ; load base of array access for 'b'
    ADD R6, R6, #-1
    STR R0, R6, #0                      ; push argument to stack.

    ADD R0, R5, #-2                     ; load base of array access for 'a'
    ADD R6, R6, #-1
    STR R0, R6, #0                      ; push argument to stack.

    JSR maxArray                        ; call function.

    LDR R0, R6, #0                      ; load return value.
    ADD R6, R6, #1
    ADD R6, R6, #2                      ; pop arguments

    ADD R0, R5, #-2                     ; load base of array access for 'a'
    AND R1, R1, #0
    ADD R2, R0, R1                      ; calculate index into array
    LDR R2, R2, #0                      ; load element from array
    ADD R0, R5, #-2                     ; load base of array access for 'a'
    AND R1, R1, #0
    ADD R1, R1, #1
    ADD R3, R0, R1                      ; calculate index into array
    LDR R3, R3, #0                      ; load element from array
    ADD R2, R2, R3
    ADD R0, R5, #-2                     ; load base of array access for 'a'
    AND R1, R1, #0
    ADD R1, R1, #2
    ADD R3, R0, R1                      ; calculate index into array
    LDR R3, R3, #0                      ; load element from array
    ADD R2, R2, R3
    STI R2, RETURN_SLOT                 ; write return value from main
    HALT


; ------ Data Section ------ 
USER_STACK        .FILL xFDFF
RETURN_SLOT       .FILL xFDFF
.END
