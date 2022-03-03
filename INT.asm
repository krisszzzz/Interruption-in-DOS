; some int from DOS, some straight to mem
.model tiny
.186
.code
org 100h

locals @@

HotkeyPressed   equ '#'
HotkeyUnPressed equ '$'
F3Hotkey	equ 03Dh        ; F3 scan code
OneIntSize	equ 4h		; One interruption in interrupt table include segment register (cs) and offset (ip) = 4 byte

;-------------------------------------------------
; Macro to replace standart interruption and save it
; IntNamePos - position of interruption - for example 08h * 4
; ReplaceHandler - Name of procedure that will be replaced in interruption table
; SaveHandler    - Name of buffer, where will be saved info about system handler of interruption
; Note: You should write cli - sti by your own to correct work, and es should be zeroed!
;
ReplaceSaveInt  macro IntNamePos, ReplaceHandler, SaveHandler
		mov ax, es:[IntNamePos * 4]
		mov word ptr SaveHandler, ax

		mov es:[IntNamePos * 4], offset ReplaceHandler

		mov ax, es:[IntNamePos * 4 + 2]
		mov word ptr SaveHandler + 2, ax
		push cs
		pop ax
		mov es:[IntNamePos * 4 + 2], ax

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
		je @@CallSys08h

		push ax cx dx bp di si ds es

		mov ah, 01Eh
		mov dl, 0Ah
		mov dh, 0Ah

		lea si, BoxFill
		lea cx, String
		mov bp, 70*2 + 80*2 + 2

		mov bx, 0B800h
		mov es, bx

		mov di, 60*2
		push cs
		pop ds

		call DrawBox

		pop es ds si di bp dx cx ax

@@CallSys08h:
		pop bx
		db 0EAh			; jmp far
		SysHandler08h dd 0
		endp

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

		;; call Restore08h
		;; jmp @@Skip


@@SecondPress:
		mov byte ptr cs:[bx], HotkeyUnPressed

			;; xor bx, bx
      	        ;; xor bx, bx
		;; mov es, bx
		;; mov bx, 08h * 4

		;; call Overwrite08h

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

                BoxFill db '.12345678'
		String  db '1$'

		HotkeyPress db HotkeyUnPressed

NewHandler09h	endp


;-------------------------------------------------
; Save system interuption 08h and 09h in
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

	;; SaveSysHandler 09h
		;; mov ax, es:[bx]
		;; mov word ptr SysHandler08h, ax    ; Save System handler of 08h interruption (cs and ip)

		;; mov ax, es:[bx + 2]
		;; mov word ptr SysHandler08h+2, ax

		ReplaceSaveInt 09h, NewHandler09h, SysHandler09h

		ret
		sti
		endp

;-------------------------------------------------
; Overwrite 08h System interuption
; Entry: BX = 08h * 4
; 	 ES = 0000h
; Ret: None
; Destr: AX
;-------------------------------------------------


Overwrite08h	proc
		cli

		mov word ptr es:[bx], offset NewHandler08h
		push cs
		pop  ax
		mov  word ptr es:[bx+2], ax

		sti
		ret
Overwrite08h	endp

;-------------------------------------------------
; Overwrite 09h System interuption
; Entry: BX = 09h * 4
; 	 ES = 0000h
; Ret: None
; Destr: AX
;-------------------------------------------------

Overwrite09h	proc
		cli

		mov word ptr es:[bx], offset NewHandler09h
		push cs
		pop ax
		mov word ptr es:[bx+2], ax

		sti
		ret
Overwrite09h    endp


;; Restore08h	proc
;; 		cli

;; 		xor bx, bx
;; 		mov es, bx
;; 		mov bx, 08h * 4

;; 		mov ax, word ptr SysHandler08h
;; 		mov word ptr es:[bx], ax
;; 		mov ax, word ptr SysHandler08h + 2
;; 		mov word ptr es:[bx + 2], ax

;; 		sti
;; 		ret
;; 		endp

include .\4.ASM

ProgEnd:


end 		start
