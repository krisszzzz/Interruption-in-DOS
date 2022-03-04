;.model tiny
;.code
;.186
;org 100h

Whitespace equ ' '
ERANGE     equ 00Fh

;locals @@


.putchar    macro

            push ax
            push dx

            mov ah, 002h
            mov dl, '$'
            int 21h
            pop dx
            pop ax

            endm

;-------------------------------------------------
;-------------------------------------------------
; Main macro used to generate itoa function
; Serves to generate itoa with different number systems.
; Used to generate code for an arbitrary number system and
; for a number system that is a multiple of a power of two

; See documentation at the end of program

; LABEL_ID - used to make unique labels. The program will not work if some labels coinciding
; ADDOP    - additional operation. This operation individual for arbitrary number system and for
;            a number system that is a multiple of a power of two

; DIVOP1, DIVOP2 - Instructions to get incomplete quotient (and probably remainder, as case of DIV operation)
; MODOP1, MODOP2 - Instructions to get remainder
;-------------------------------------------------


.genitoa macro LABEL_ID, ADDOP, DIVOP1, DIVOP2, MODOP1, MODOP2
        ADDOP
        mov si, di

        cmp dl, 00Ah
        ja @@UseLettersNum&LABEL_ID


@@UseDecimalNum&LABEL_ID:
        mov ax, bx

        DIVOP1
        DIVOP2

        add al, '0'
        stosb

        MODOP1
        MODOP2

        cmp bx, 000h
        jne @@UseDecimalNum&LABEL_ID
        jmp @@ItoaExit

@@UseLettersNum&LABEL_ID:
        mov ax, bx

        DIVOP1
        DIVOP2

        cmp al, 009h
        jbe @@DecimalNum&LABEL_ID

        sub al, 00Ah
        add al, 'A'
        jmp @@WriteSym&LABEL_ID

@@DecimalNum&LABEL_ID:
        add al, '0'

@@WriteSym&LABEL_ID:
        stosb

        MODOP1
        MODOP2

        cmp bx, 000h
        jne @@UseLettersNum&LABEL_ID

        jmp @@ItoaExit

        endm

.endcheck macro SEG_REG, REG

        cmp byte ptr SEG_REG:[REG - 1], '$'
        jne @@NotStrEnd&REG
        sub byte ptr SEG_REG:[REG - 1], '$'
@@NotStrEnd&Reg:
        endm

;-------------------------------------------------
;-------------------------------------------------

Comment /
__start:

        mov di, offset string
        mov dl, 00Ah
        mov bx, 0FFFFh
        xor cx, cx
        xor dh, dh

        call itoa

        mov dx, offset string
        mov ah, 009h
        int 21h


        mov ax, 4c00h
        int 21h
        string db 25 dup(?)
/
;-------------------------------------------------
;-------------------------------------------------
; Convert string containing number to integer-type number
; Entry: ES:DI - address of string that need to be converted
;
; Note: String should be ended with '$'. be carefull, because ES should be correct (don't change the value of ES)
;       BH must be equal 00h
; Return: AX:DX - integer-type number (AX contain ...). If overflowing errno will contain ERANGE value
; Destr: AX, BX, CL, DI, DX
;-------------------------------------------------

atoi    proc
        xor ax, ax              ; ax = 0
        cmp byte ptr es:[di], Whitespace
        jne @@TakeSign

@@IgnoreSpace:
        add di, 1
        cmp byte ptr es:[di], Whitespace
        je @@IgnoreSpace

@@TakeSign:

        cmp byte ptr es:[di], '+'
        je @@SkipPlus             ; Skip plus by inc di

        cmp byte ptr es:[di], '-'
        jne @@TakeNumber      ; By default if not provided '+' the number is positive

        mov cl, 00Ch          ; mark minus for identification
@@SkipPlus:
        add di, 1

@@TakeNumber:
        cmp byte ptr es:[di], '0'
        jb @@Exit

        cmp byte ptr es:[di], '9'
        ja @@Exit

        mov bl, 10   ; To mul 10
        mul bl

        mov bl, byte ptr es:[di] ; Take number
        sub bl, '0'              ; Convert ASCII-->number

        add ax, bx

        jo @@Overflow
        add di, 1

        jmp @@TakeNumber

@@Overflow:
        mov di, offset errno
        mov byte ptr ds:[di], ERANGE
        mov ax, 0FFFFh
        jmp @@ret

@@Exit:
        cmp cl, 00Ch
        jne @@ret
        neg ax

@@ret:
        ret
        errno db ?
atoi    endp



;-------------------------------------------------
;-------------------------------------------------
; Find character in string
; Entry: AL      - character to find
;        ES:[DI] - string, that will start the finding
; Ret:   SI - character position
; Destr: SI

strchr  proc
        mov si, di
@@Find:
        cmp byte ptr es:[di], '$'
        je @@NotFounded
        scasb
        jnz @@Find
        sub di, 1

        xchg si, di
        ret

@@NotFounded:
        mov di, si
        xor si, si              ; Return SI = 0, if not founded

        ret
        endp

;-------------------------------------------------
;-------------------------------------------------
; Find the len of the string with terminate symbol $
; Entry: ES:[DI] - string with terminate symbol $
; Ret:  BX string length
; Destr: BX, AL

strlen  proc
        mov al, '$'
        mov bx, di

@@FindEnd:
        scasb
        jnz @@FindEnd
        xchg bx, di

        sub  bx, di
        sub  bx, 1

        ret
        endp


;-------------------------------------------------
;-------------------------------------------------
; Compare two strings with each other
; If string coincide BX will be zeroed
; If first string is larger (ASCII symbol is larger) then BX > 0
; Else BX < 0
;
; Entry:  DS:[DI] - first string
;         ES:[SI] - second string
;         CX      - number of character to compare
;
; Ret:    CX      - comparison result
; Destr:  CX, DI, SI

strncmp proc

@@Compare:
        cmp byte ptr ds:[di - 1], '$'
        je @@Exit

        cmpsb
        loopz @@Compare

        cmp cx, 000h
        je @@ret

        .endcheck ds, di
        .endcheck es, si

        mov cl, byte ptr ds:[di - 1]
        sub cl, byte ptr es:[si - 1]
        jmp @@ret
@@Exit:
        xor cx, cx
@@ret:
        ret
        endp

;-------------------------------------------------
; Output hex value in buffer in register-type style (0000h)
; Entry: DS:[SI] - buffer to output (Buffer should be at least 4-byte)
;        ES:[DI] - position in video segment
;        ES = videoseg
;        AH - color
; Destr: SI, DI, AL, CX

OutHex  proc
        mov cx, 4

@@OutHexNum:
        lodsb

        cmp al, '$'
        je @@CheckCX

        stosw

        loop @@OutHexNum

@@CheckCX:
        cmp cx, 0
        je @@Exit

        mov al, '0'
@@OutZero:
        stosw
        loop @@OutZero

@@Exit:
        ret
        endp


;-------------------------------------------------
;-------------------------------------------------
; Convert integer number to string
; Entry:  ES:[DI] - destination buffer to write number
;         BX      - number to conver
;         DL      - radix (from 2 to 36)

; Ret:  ES:[DI] will contain string with numbers
; Note: The string will contain end-symbol $
; Destr: AX, BX, CX, DI, SI, DL - optinally (only if DL is not power of two)


itoa    proc

        cmp dl, 002h
        jae @@NoExit
        cmp dl, 024h    ; = 36d
        jbe @@NoExit
        jmp far ptr @@ItoaExit

@@NoExit:
        mov cl, 5       ; Compare with all power of 2 beginning from 5 (2^5, 2^4, 2^3, 2^2, 2^1)

@@CheckOpt2Pow:      ; Check for optimization, if radix is power of 2
        mov al, 001h
        shl ax, cl      ; mul ax to 2^(CL)
        cmp al, dl
        je @@Opt2Pow

        sub cl, 001h
        cmp cl, 001h

        jae @@CheckOpt2Pow

        .genitoa 1, <xchg cl, dl>, <div cx>, <xchg dx, ax>, <mov bx, dx>, <xor dx, dx> ; -------------------------+
        jmp @@ItoaExit                                                                 ;                          |
                                                                                       ;                          |
@@Opt2Pow:                                                                             ;                          |
        .genitoa 2, <sub dl, 1>, <and ax, dx>, <>,  <shr bx, cl>       ;-----------------------------+            |
                                                                                      ;              |            |
 @@ItoaExit:                                                                          ;              |            |
        mov byte ptr es:[di], '$'                                                     ;              |            |
        sub di, 001h                                                                  ;              |            |
                                                                                      ;              |            |
@@Reverse:                                                                            ;              |            |
        mov ah, byte ptr es:[di]                                                      ;              |            |
        mov bh, byte ptr es:[si]                                                      ;              |            |
        mov byte ptr es:[si], ah                                                      ;              |            |
        mov byte ptr es:[di], bh                                                      ;              |            |
                                                                                      ;              |            |
        sub di, 001h                                                                  ;              |            |
        add si, 001h                                                                  ;              |            |
                                                                                      ;              |            |
        cmp di, si                                                                    ;              |            |
        ja @@Reverse                                                                  ;              |            |
                                                                                      ;              |            |
                                                                                      ;              |            |
@@ErrorExit:                                                                          ;              |            |
                                                                                      ;              |            |
        ret                                                                           ;              |            |
        endp                                                                          ;              |            |
                                                                                      ;              |            |
                                                                                      ;              |            |
; end __start                                                                          ;             |            |
                                                                                      ;              |            |
; Used to generate code for an arbitrary number system and    <--------------------------------------+------------+
; Convert number to string (With arbitrary number system, so this is slower then optimized one)      |
; Description                                                                                        |
;                                                                                                    |
; Entry: ES:[DI] - destination buffer to write number                                                |
;        BX      - number to convert                                                                 |
;        DL      - arbitrary radix from 2 to 36                                                      |
; Note:  For correct work CX should be zeroed!                                                       |
; Ret:   ES:[DI] will contain translated number and '$' - terminal symbol                            |
; Destr: CL, DX, AX, BX, SI, DI                                                                      |
; Expended macro                                                                                     |
;                                                                                                    |
;-------------------------------------------------                                                   |
COMMENT /                                                                                            |
        xchg cl, dl            ; Change CL and DL, because we need DX to use DIV                     |
                               ;                                                                     |
                                                                                                     |
        mov si, di             ; Save buffer begining                                                |
                                                                                                     |
        cmp dl, 00Ah          ; Compare to 10d to optimize convertation                              |
        jae @@UseLettersNum1                                                                         |
                                                                                                     |
                                                                                                     |
@@UseDecimalNum1:                                                                                    |
                                                                                                     |
        mov ax, bx            ; Write number to ax to complete division                              |
        div cx                                                                                       |
        xchg dx, ax           ; Change remainder (dx) with incomplete quotient                       |
                                                                                                     |
        add al, '0'           ; Convert Using Decimal number (if number system is decimal or lower)  |
                                                                                                     |
        stosb                                                                                        |
                                                                                                     |
        mov bx, dx            ; Write incomplete quotient to BX                                      |
        xor dx, dx            ; To correct division                                                  |
                                                                                                     |
        cmp bx, 000h          ; If BX == 0 go to exit, and reverse numbers                           |
        jne @@UseDecimalNum1                                                                         |
        jmp @@ItoaExit                                                                               |
                                                                                                     |
@@UseLettersNum1:                   ; Same as UseDecimalNum1, but need to use not                    |
        mov ax, bx                  ; only decimal numbers but letters, such as A, B, C, etc.        |
        and ax, dx                                                                                   |
                                                                                                     |
        cmp al, 009h                                                                                 |
        jbe @@DecimalNum1                                                                            |
                                                                                                     |
        sub al, 00Ah                ; This segment convert number to letters 'A', 'B' etc.           |
        add al, 'A'                 ; Needed if radix more then 16                                   |
        jmp @@WriteSym1                                                                              |
                                                                                                     |
@@DecimalNum1:                                                                                       |
        add al, '0'                                                                                  |
                                                                                                     |
@@WriteSym1:                                                                                         |
        stosb                                                                                        |
        mov bx, dx                                                                                   |
        xor dx, dx                                                                                   |
                                                                                                     |
        cmp bx, 000h                                                                                 |
        jne @@UseLettersNum1                                                                         |
        jmp @@ItoaExit                                                                               |
                                                                                                     |
/ ;                                                                                                  |
;                                                +---------------------------------------------------+
;                                                V
; for a number system that is a multiple of a power of two
; Description
;
; Entry: ES:[DI] - destination buffer to write number
;        BX      - number to convert
;        DL      - radix that is a multiple of a power of two (2, 4, 8, 16, 32)
;        CL      - power of two
; Note:
; Ret:   ES:[DI] will contain translated number and '$' - terminal symbol
; Destr: CL, AX, BX, SI, DI
;
;

COMMENT /
        sub dl, 1  ; To mod 2^radix we use logical operator AND
                   ; For example to get remainder of a division by 4
                   ; We need to use AND BX, 11b. But 11b = 4 - 1 = 3d
                   ; So this why we SUB 1 from radix (CH)

        mov si, di ; save current buffer index

        cmp dl, 00Ah                    ; Note that we sub 1 before

        jae @@UseLettersNum2            ; Only needed for 16, and 32 radix
                                        ; To indicate next numbers use letter

@@UseDecimalNum2:

        mov ax, bx
        and ax, dx

        add al, '0'                 ; Convertation to decimal number

        stosb

        shr bx, cl                  ; Diviosion to 2^(CL), so to radix

        cmp bx, 000h
        jne @@UseDecimalNum2
        jmp @@ItoaExit

@@UseLettersNum2:
        mov ax, bx
        and ax, dx

        cmp al, 009h
        jbe @@DecimalNum2

        sub al, 00Ah                ; This segment convert number to letters 'A', 'B' etc.
        add al, 'A'                 ; Needed if radix more then 16
        jmp @@WriteSym2

@@DecimalNum2:
        add al, '0'

@@WriteSym2:
        stosb

        shr bx, cl
        cmp bx, 000h
        jne @@UseLettersNum2

/
