; some int from DOS, some straight to mem
.model tiny
.186
.code
org 100h

locals @@

HotkeyPressed    equ '#'
HotkeyUnPressed  equ '$'
F3Hotkey	 equ 03Dh        ; F3 scan code
OneIntSize	 equ 004h	; One interruption in interrupt table include segment register (cs) and offset (ip) = 4 byte

TableColor       equ 01Ah
TableWidth	 equ 00Bh

TableHeight	 equ 00Ah
TablePos	 equ 140*2
ColumnPos	 equ 81*2
AXInBuffPos	 equ TableWidth*2 + 8
RegisterCount	 equ 008h

.SaveBuffer	macro BufferName
		mov bx, 0B800h	; Change ds = 0B800h
		mov ds, bx

		mov bx, cs	; Change es = cs
		mov es, bx

		mov cx, TableWidth
		mov bx, TableHeight

		mov si, TablePos

		lea di, BufferName
		call SaveTable

		endm
;-------------------------------------------------
; Macros to output buffer to videobuffer
; Destr: BX, DS, ES, CX, BX, DI, SI


.OutBuffer	macro BufferName
		mov bx, cs
		mov ds, bx

		mov bx, 0B800h
		mov es, bx

		mov cx, TableWidth
		mov bx, TableHeight

		mov di, TablePos
		lea si, BufferName

		call OutTable

		endm

;-------------------------------------------------
; Macro to replace standart interruption and save it
; IntNamePos - position of interruption - for example 08h * 4
; ReplaceHandler - Name of procedure that will be replaced in interruption table
; SaveHandler    - Name of buffer, where will be saved info about system handler of interruption
; Note: You should write cli - sti by your own to correct work, and es should be zeroed!
;
ReplaceSaveInt  macro IntNamePos, ReplaceHandler, SaveHandler
		mov ax, es:[IntNamePos * OneIntSize]
		mov word ptr SaveHandler, ax

		mov es:[IntNamePos * OneIntSize], offset ReplaceHandler

		mov ax, es:[IntNamePos * OneIntSize + 2]
		mov word ptr SaveHandler + 2, ax

		push cs		; AX = CS
		pop ax

		mov es:[IntNamePos * OneIntSize + 2], ax

		endm

			

start:

                call ReplaceSave08h09h


                mov ax, 3100h	; Exit, but stay resident
                mov dx, offset ProgEnd
                shr dx, 4
                inc dx

                int 21h



NewHandler08h	proc

		push bx

		lea bx, HotkeyPress
		cmp byte ptr cs:[bx], HotkeyUnpressed

		je  @@CallSys08h

		; --------------------------
		; Start of New handler

		pop  bx		; Get Old bx value
		push bx		; Push them back

		push ax cx dx bp di si ds es ; Register that will be used

		push ax bx cx dx si di es ds ; Register to output info


		mov bp, RegisterCount

		;; push cs
		;; pop  ds

		push cs
		pop  es


@@OutputReg:
		mov dl, 10h	; 16 radix system
		pop bx		; Get Registers value

		lea di, RegValue

		call itoa

		lea si, RegValue
		mov di, offset DisplayBuffer + AXInBuffPos

		mov ax, TableWidth * 2
		dec bp

		mul bp
		add di, ax

		mov ah, TableColor

		call OutHex

		cmp bp, 0
		ja @@OutputReg

 		.OutBuffer DisplayBuffer

		pop es ds si di bp dx cx ax

@@CallSys08h:
		pop bx
		db 0EAh			; jmp far
		SysHandler08h dd 0
		RegValue      db 5 dup(?)

		endp

;-------------------------------------------------
;
NewHandler09h	proc
		push ax

		in  al, 60h	; Check 60h port
		cmp al, F3Hotkey

		je @@WriteRegInfo

		pop ax

		db 0EAh		; Jump far ptr SysHandler09h
		SysHandler09h dd 0


@@WriteRegInfo:
		push bx cx dx bp si di ds es

		lea bx, HotkeyPress

		cmp byte ptr cs:[bx], HotkeyPressed
		je @@SecondPress

		mov byte ptr cs:[bx], HotkeyPressed

	;; --------------------------------------
	;; 	First press
		.SaveBuffer BackBuffer
		push cs
		pop ds

		mov bx, 0B800h	; Setup to output table
		mov es, bx

		mov ah, TableColor

		mov dl, TableWidth
		mov dh, TableHeight

		lea si, BoxFill

		lea cx, UselessString
		xor bp, bp	; Bp is UselessString position in videosegment
				; it could be any value
		mov di, TablePos

		call DrawBox	; Draw simple box

		mov cx, 02h
		lea si, Registers

		mov di, TablePos + ColumnPos

		call WriteInColumn

		.SaveBuffer DisplayBuffer ; Save Table

		jmp @@InterruptEnd

@@SecondPress:
		mov byte ptr cs:[bx], HotkeyUnPressed
		.OutBuffer BackBuffer

@@InterruptEnd:
		in al, 061h
		mov ah, al

		or al, 080h
		out 61h, al

		mov al, ah
		out 61h, al

		mov al, 020h
		out 020h, al

		pop es ds di si bp dx cx bx ax
		iret

		HotkeyPress   db HotkeyUnPressed
		BackBuffer    db 220 dup(?)
		DisplayBuffer db 220 dup(?)
		Registers     db 'AXBXCXDXSIDIESDS$'

                BoxFill        db ' ∫∫»Õº…Õª' ; Element used to draw the box
		UselessString  db '$$$'	      ; This string need only for correct work DrawBox procedure

NewHandler09h	endp


;-------------------------------------------------
; Replace 08h and 09h in interruption table and save system 08h and 09h
; Entry: None
; Ret:   None
; Destr: BX, ES, AX
; Note:
;-------------------------------------------------


ReplaceSave08h09h proc

		cli
		xor bx, bx
		mov es, bx

		; 	interruption name, new interruption handler (procedure name), buffer to save standart interruption
		;		|			|					  |
		;		V	----------------+					  |
		;			V						          |
		ReplaceSaveInt 08h, NewHandler08h, SysHandler08h ; <------------------------------+

		ReplaceSaveInt 09h, NewHandler09h, SysHandler09h

		sti
		ret
		endp

;-------------------------------------------------
; Compare videobuffer with buffer and replace differences
; Entry: ES:[SI] - videobuffer
; 	 DS:[DI] - buffer that will be used to compare elements
; 	 DS:[BX] - buffer to write differences
; 	 CX	 - videobuffer width
; 	 DX	 - videobuffer length

CmpBuffer	proc
		mov ax, cx
		mov bp, di
		jmp @@Compare

@@GoToNextLine:
		mov cx, ax
		add si, 160
		sub si, cx
		sub si, cx

@@Compare:
		dec cx
		cmpsw
		jne @@NoDiff


		sub di, 2
		sub si, 2

		sub di, bp
		add bx, di
		mov bp, di

		xchg bx, di
		movsw
		xchg bx, di

@@NoDiff:
		cmp cx, 0
		ja @@Compare
		dec dx
		ja @@GoToNextLine


		ret
		endp

include ./4.ASM
include ./STRING1.ASM

ProgEnd:


end 		start
