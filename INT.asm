; some int from DOS, some straight to mem
.model tiny
.186
.code
org 100h

locals @@

HotkeyPressed   equ '#'
HotkeyUnPressed equ '$'
F3Hotkey	equ 03Dh        ; F3 scan code
OneIntSize	equ 004h	; One interruption in interrupt table include segment register (cs) and offset (ip) = 4 byte
TableColor      equ 01Ah
TableWidth	equ 011h
TableHeight	equ 00Ah
TablePos	equ 140*2
ColumnPos	equ 81*2
AXPos		equ TablePos+ColumnPos + 8

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
		push cs
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

		;; pop  bx			     Get old value of BX
		push ax cx dx bp di si ds es ; Register that will be used

		;; push ax bx cx dx bp di si ds es ; Register that need to output their value

		push cx

		push cs		; To correct work
		pop ds		; DS and ES should be CS

		push cs
		pop es


		mov dl, TableWidth
		mov dh, TableHeight

		lea si, BoxFill
		lea cx, UselessString
		xor bp, bp	; Bp is UselessString position in videosegment
				; it could be any value
		mov di, TablePos

		;; call DrawBox

		mov cx, 02h
		lea si, Registers

		mov di, TablePos + ColumnPos

		call WriteInColumn

		mov dl, 10h	; 16-Radix

		pop bx
		xor cx, cx
		xor dh, dh
		lea di, RegValue

		call itoa

		mov ah, TableColor
		mov di, AXPos
		lea si, RegValue

		mov bx, VideoSeg
		mov es, bx


		call OutHex



		pop es ds si di bp dx cx ax

@@CallSys08h:
		pop bx
		db 0EAh			; jmp far
		SysHandler08h dd 0

		Registers      db 'AXBXCXDXSIDIESDS$'
                BoxFill        db ' ∫∫»Õº…Õª' ; Element used to draw the box
		UselessString  db '$'	      ; This string need only for correct work DrawBox procedure
		RegValue       db 5 dup(?)

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
		push bx

		lea bx, HotkeyPress
		cmp byte ptr cs:[bx], HotkeyPressed
		je @@SecondPress

	; 	If first press is detected
	;
		mov byte ptr cs:[bx], HotkeyPressed
		jmp @@InterruptEnd

@@SecondPress:
		mov byte ptr cs:[bx], HotkeyUnPressed

@@InterruptEnd:
		in al, 061h
		mov ah, al

		or al, 080h
		out 61h, al

		mov al, ah
		out 61h, al

		mov al, 020h
		out 020h, al

		pop bx ax
		iret

		HotkeyPress db HotkeyUnPressed

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

		ret
		sti
		endp


include ./4.ASM
include ./STRING1.ASM

ProgEnd:


end 		start
