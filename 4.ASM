;.model tiny

;.code
;org 100h

;locals @@

VideoSeg    equ  0b800h ; Video segment begin
Green       equ  0Ah    ; Green 1-byte RGB
WhiteSpace  equ  ' '
SpaceCtl    equ  0AFFFh ; Constant used to control amount of space in matrix-mode
StringPos   equ  0A2h   ; String position to output
F_Width     equ  015h   ; Width of frame
F_Height    equ  010h   ; Height of frame
FramePos    equ  000h   ; Position of frame
CmdEnd      equ  00Dh   ; Comand line end symbol
FillPos     equ  0A2h   ; = 162d
MinSymCount equ  00Bh
Color       equ  004h   ; Red color


.vstosw         macro
;                nop
                mov es:[di], ax
                add di, 000A0h ; = 160d
;                nop
                endm

.wait           macro
                nop
                push cx
                push dx
                mov ah, 086h   ; wait function
                mov cx, 00000h
                mov dx, 0C350h ; Wait 0.05 sec
                int 15h
                pop dx
                pop cx
                nop
                endm

.BufferExchangeOp macro ProcedureName, VideoIndexReg

 ProcedureName  proc

                mov dx, cx
                jmp @@SaveRow

@@GoToNextRow:
                mov cx, dx
                add VideoIndexReg, 80*2

                sub VideoIndexReg, cx
                sub VideoIndexReg, cx

@@SaveRow:
                rep movsw

                sub bx, 1
                cmp bx, 0
                ja @@GoToNextRow

                ret
                endp

                endm

COMMENT /

Start:
        mov ah, 04Eh
		mov dl, 0Ah
		mov dh, 0Fh

		lea si, BoxFill
		lea cx, String
		mov bp, 70*2 + 80*2 + 2

		mov bx, 0b800h
		mov es, bx

		mov di, 70*2

        call DrawBox

        mov bx, es
        mov ds, bx

        mov bx, cs
        mov es, bx

        mov cx, 0Ah
        mov bx, 0Fh

        mov si, 70*2
        lea di, Buffer

        call SaveTable

        push ds
        push es
        pop  ds
        pop  es

        lea si, Buffer
        mov di, 40*2
        mov cx, 0Ah
        mov bx, 0Fh

        call OutTable

        mov ax, 4c00h
        int 21h
        Buffer  db 300 dup(?)
        ;BoxFill db '.ººèí¼éí»'
        BoxFill db '.12345678'
		String  db 'f$'


/

;-------------------------------------------------
; Proccess comand line arguments to correct work for DrawBox function
; Entry: DI = 80h - comand line fragment begin
; Note:  ES - should have default value, that his have in begining of program
; Ret: None
; Destr: AX, BX, CX, DX, BP, DI, SI  - @ALL


CmdProc         proc
                mov cl, es:[di]         ; di = 80h
                cmp cl, MinSymCount
                ja @@NoError

                add di, 1               ; 81h - first command line symbol (it should be space)

                mov ah, 009h            ; Error processing
                mov dx, offset error
                int 21h

@@NoError:      mov si, offset matrix
                repe cmpsb               ; Compare to ' matrix'

                sub di, 007h             ; 7 = len(' matrix')
                cmp di, 081h             ; 81h - begining command line symbol

                jne @@NotMatrix          ; if not matrix-mode

                xor di, di

                mov ax, VideoSeg         ; For correct work of DrawMatrix
                mov es, ax

                call DrawMatrix
                jmp @@ret

@@NotMatrix:
                mov ah, Color       ; Red RGB-byte

                mov si, 082h        ; Skip space
;                mov bx, es          ; Save prev es

                mov cx, VideoSeg    ; change es to VideoSeg
                mov es, cx

                mov di, FramePos    ; position of frame
                mov cx, 8Ch         ; 9-byte offset from 82h

                mov bp, StringPos   ; remember string position

                mov dl, F_Width
                mov dh, F_Height

                call DrawBox

@@ret:          ret
                matrix db ' matrix'
                error db 'Error: Fuck you!$'
                endp


;-------------------------------------------------
; Draw a box containing the string
; Entry: AH - color of symbols
;        DL - width of box
;        DH - height of box
;        SI - address of 9-byte massive of symbols (|\+-++-+)
;        CX - address of string to output (DS:[CX])
;        BP - string to output position
;        ES - videoseg addr (ob800h)
;        DI - left corner of box
; Note: SI syntax: first        symbol  - is fill symbol
;                  second       symbol  - is a symbol used to draw left vertical line
;                  third        symbol  - is a symbol used to draw right verical line
;                  four-sixth   symbols - is 3-byte array used to draw lower horizontal line (watch DrawHorLine doc)
;                  seven-nineth symbols - is 3-byte array used to draw upper horizontal line
; Destr: AX, BX, CX, DX, DI, SI, BP, DS

DrawBox         proc
;                mov ds, bx        ; Write prev es to ds

                push cx           ; Save cx, di, dx
                push di
                push dx

                add  di, FillPos  ; Move to correct filling

                sub  dl, 2        ; Change Fill size
                sub  dh, 2        ;

                lodsb             ; Load first symbol

                call Fill

                pop  dx
                pop  di

                lodsb                ; Load second symbol
                push di              ; Remember the left-upper corner
                push di              ; Write Left vertical line

                call DrawVertLine


                pop  di              ; Go back to left corner

                xor bx, bx           ; Use to add di + dl
                mov bl, dl           ; dl = width, so di + width

                add di, bx           ; Go to right-upper corner
                add di, bx           ; By adding width
                sub di, 2            ;

                lodsb                ; Load third symbol
                call DrawVertLine    ; Write right vertical line

                xor bx, bx           ; Use to sub di - dl
                mov bl, dl           ; dl = width, so di - width

                sub di, bx           ; Go to left-lower corner
                sub di, bx

                add di, 2

                xor cx, cx
                mov cl, dl
                call DrawHorLine    ; Write lower horizontal line

                pop  di             ; Go to left-upper corner
                xor  cx, cx         ;
                mov  cl, dl
                call DrawHorLine    ; Write upper horizontal line
                pop  di
                call OutString

                ret

DrawBox         endp

;-------------------------------------------------
; Fill rectangle with symbol (in videomemory)
; Entry: AH - color of filled symbols
;        DL - width of rectangle
;        DH - height of rectangle
;        AL - symbol to fill
;        ES - videoseg addr (ob800h)
;        DI - left corner of rectangle
; Exit: None
; Destr: CX, DX, AX, DI


Fill            proc
@@DrawLine:
                xor cx, cx
                mov cl, dl
                rep stosw             ; Write one line

                mov cl, dl 

                sub di, cx
                sub di, cx
                add di, 80*2           ; Go to next line

                sub dh, 1
                ja @@DrawLine          ; Repeat DH count

                ret
Fill            endp

;------------------------------------------------
; draw a vertical line in frame
; Entry: AH - color of line
;        AL - symbol
;        DH - height of line
;        DI - address of start of line 
; Note:  ES = videoseg addr (0b800h), BX used as temp for CX
; Exit:  None
; Destr: SI, DI, BX, CX, probably AH

DrawVertLine    proc
                mov bx, cx          ; save cx
                mov cl, dh
                sub cl, 1
                ja @@NextSym

; If errored:
                mov ah, 09h
                mov dx, offset V_Error_Msg
                int 21h
                jmp @@ret

@@NextSym:      .vstosw             ; wrote a symbol and go to the next line
                loop @@NextSym

@@ret:          mov cx, bx
                ret
                V_Error_Msg db 'Fatal error in DrawVertLine: DX should be positive$'
DrawVertLine    endp


;-------------------------------------------------
; Draw a line in a frame
; Entry: AH - color of line
;        CX - length of string
;        SI - addr of 3-byte array containing line elements (offset string)
;        DI - address of start of line
; Note:  ES = videoseg addr (0b800h)
; Exit:  None
; Destr: AL, CX, SI, DI


DrawHorLine     proc

                lodsb
                stosw
                lodsb

                sub cx, 2
                jbe @@StopLine

                rep stosw

                lodsb
                stosw
                jmp @@ret


@@StopLine:     mov ah, 09h
                mov dx, offset H_Error_Msg
                int 21h

@@ret:          ret
                H_Error_Msg  db 'Fatal error: DrawHorLine procedure cannot work, if cx <= 2$'
DrawHorLine     endp

;-------------------------------------------------
; Draw lines with same size in column
; Entry: AH - color of lines
;        CX - length of each line
;       DS:[SI] - address of array containing lines to output (This string should be ended with '$')
;       ES:[DI] - adress of start of first line in video segment
;        ES = video segment adrress (0b800h)
; Ret: None
; Destr: AL, CX, DX, SI, DI

WriteInColumn   proc
                mov dx, cx  ; Temp for cx

@@DrawColumn:

                lodsb
                cmp al, '$'
                je @@Exit

                stosw

                loop @@DrawColumn

                mov cx, dx

                sub di, cx        ; Go back for length of line. Note that we subbed 2 times because
                                  ; videomemory has two byte for one symbol
                sub di, cx
                add di, 80*2

                jmp @@DrawColumn
@@Exit:
                ret
                endp

;-------------------------------------------------
; Save symbols in table from videobuffer to buffer
; Entry: CX - width of box
;        BX - length of box
;   DS:[SI] - Source (Table in videobuffer)
;   ES:[DI] - Destination buffer

;   Destr: DX, CX, BX, SI, DI

.BufferExchangeOp SaveTable, si ; See the macro in the begining of program
                                ; Generate SaveTable procedure to save table into buffer

;-------------------------------------------------
; Entry: CX  - width of box
;        BX  - length of box
;    DS:[SI] - Source (Table in buffer)
;    ES:[DI] - Destination videobuffer (table left corner)

; Destr: DX, CX, BX, SI, DI

.BufferExchangeOp OutTable, di

;-------------------------------------------------
; Output string to videobuffer
; Entry: AH - color of string
;        DI - pointer to string (with terminate symbol $)
;        BP - string to output position
;        ES = Video segment (0B800h)
; Destr: AL, BP, DI


OutString       proc
@@Puts:
                mov al, byte ptr ds:[di]  ; Exchange buffer from command line to videomemory
                cmp al, '$'               ; Go exit if Command line is ended
                je @@exit

                mov es:[bp], ax           ;
                add bp, 2                 ; Go to the next symbol in videomemory
                add di, 1                 ; Go to the next symbol in command line

                jmp @@Puts
@@exit:
                ret
                endp

;------------------------------------------------
;------------------------------------------------  
;_    _  ---  ----- +---+   o  \     /
;|\  /| |   |   |   |   |   |    \ /                   
;| \/ | |---|   |   |--\+   |    / \
;|    | |   |   |   |   \   |  /     \
;------------------------------------------------
; Draw Matrix like symbols in DOS
; Entry: DI - 0
;        ES - start of video segment (0b800h)
; Ret:   None
; Destr: CX, DX, AX, DI, SI

DrawMatrix      proc
                mov cx, 993h ; (Start Seed)
                mov dx, 993h
                mov si, 0000h
                
@@DrawLine:
                call DrawMatrixLine  
                mov  ch, 25    ; Use Maximum Scroll Down (In DOS max height is 25)

                .wait          ; Get output "smoother"

                call ScrollDown

                add cx, 10    ; Change Seed (to generate random symbols)
                add dx, 5
                

                mov di, 0
                add si, 1     
                cmp si, 00FFh ; si = iteration count               

                jne @@DrawLine
                
                ret                                              
                endp


;------------------------------------------------
; Use to scroll down in text video-mode
; Entry: CH - number of line to scroll down
;        ES - video segment start
;
; Destr: BX, DI, AX, CH, DI

ScrollDown      proc
                cmp ch, 25
                jbe @@NoDelLast
                mov ch, 24 ; Maximum line to scroll (25 will be "destroyed")
            
@@NoDelLast:    sub ch, 1

@@Scroll:       mov al, ch
                mov bh, 2*80 ; Next symbol in next line has offset 2*80 byte
                mul bh

                mov di, ax
                add ax, 160d
                mov bx, ax

@@Cpy:
                mov ax, es:[di]
                mov es:[di + 2*80], ax  ; Cpy symbols from one line to other
                add di, 2

                cmp di, bx              ; rep movs

                jb @@Cpy

                sub ch, 1
                jae @@Scroll 

                ret

ScrollDown      endp


;------------------------------------------------

; Draw matrix-like line (Using Rand)
; Entry: ES - Video segment start (0b800h)
;        DI - Position to draw line
;        CX, DX - seed
; Ret:   None
; Destr: AX, BX, BP, CX, DX, DI

DrawMatrixLine  proc
                mov bp, 80

@@NextSym:      call Rand
                cmp ax, SpaceCtl ; Use the value of 32000d to have more whitespace
                jae @@IsNotSpace
                mov al, WhiteSpace 

                
@@IsNotSpace:   stosb                       ; Wrote symbol
                mov byte ptr es:[di], Green ; Wrote green color
                inc di

                sub bp, 1
                ja @@NextSym

                ret
DrawMatrixLine  endp
                


;------------------------------------------------

; Get random symbol to output by 02h DOS function
; Entry: CX - seed
; Note: you can use .time_seed to generate symbols
; Ret: AX - generated number
; Destr: BX, CX, DX, AX

Rand            proc
                add cx, 2555h
                add dx, 9993h

                mov bh, dl
                mov bl, ch

                or ch, dl

                mul bx

                mov bh, ah
                mov bl, dl

                or ax, dx

                mul bx
                
                ret
Rand            endp
                
;end Start

; strcmp, atoi, itoa
