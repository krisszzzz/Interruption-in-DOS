; some int from DOS, some straight to mem
.model tiny
.186
.code
org 100h

locals @@


start:

                call SaveSysHandler08h09h
                call Overwrite09h


                mov ax, 3100h	; Exit, but stay resident
                mov dx, offset ProgEnd
                shr dx, 4
                inc dx

                int 21h



NewHandler08h	proc
	;; iret

		db 0EAh			; jmp far
		SysHandler08h dd 0

		endp

NewHandler09h	proc
		push ax

		in al, 60h
		cmp al, 03Dh
		je @@WriteRegInfo

		pop ax

		db 0EAh
		SysHandler09h dd 0


@@WriteRegInfo:
		push bx cx dx bp si di ds es
			;; xor bx, bx
		;; xor bx, bx
		;; mov es, bx
		;; mov bx, 08h * 4

		;; call Overwrite08h

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

                BoxFill db '.12345678'
		String  db '1$'


NewHandler09h	endp


;-------------------------------------------------
; Save system interuption 08h and 09h in
; Entry: None
; Ret:   None
; Destr: BX, ES, AX
; Note:
;-------------------------------------------------


SaveSysHandler08h09h proc
		xor bx, bx
		mov es, bx
		mov bx, 08h * 4

		mov ax, es:[bx]
		mov word ptr SysHandler08h, ax

		mov ax, es:[bx + 2]
		mov word ptr SysHandler08h+2, ax

		add bx, 4

		mov ax, es:[bx]
		mov word ptr SysHandler09h, ax

		mov ax, es:[bx + 2]
		mov word ptr SysHandler09h+2, ax
		ret
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

include .\4.ASM

ProgEnd:


end 		start
