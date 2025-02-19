.model tiny
.data

FrameStyle              db 218, 196, 191, 179, 32, 179, 192, 196, 217
LenghtScreen            dw 160
SizeStrStyle            dw 3
Coordinates             dw 4301h
LenghtFrame             dw 11
HeightFrame             dw 12
IndentationFrame        dw 3
ColorFrame              db 31h
ColorRegister           db 30h

NameAx                  db 'ax'
NameBx                  db 'bx'
NameCX                  db 'cx'
NameDx                  db 'dx'
NameSi                  db 'si'
NameDi                  db 'di'
NameBp                  db 'bp'
NameSp                  db 'sp'
NameDs                  db 'ds'
NameEs                  db 'es'
NameSs                  db 'ss'
NameCs                  db 'cs'

ArrayNameRegister       dw 2, offset NameAx
						dw 2, offset NameBx
						dw 2, offset NameCx
						dw 2, offset NameDx
						dw 2, offset NameSi
						dw 2, offset NameDi
						dw 2, offset NameBp
						dw 2, offset NameSp
						dw 2, offset NameDs
						dw 2, offset NameEs
						dw 2, offset NameSs
						dw 2, offset NameCs

NumberRegister          dw 12
ShowIsOn                db 0
IsMemoryBack            db 0

BackBuffer              dw 182 dup (0)
DrawBuffer              dw 182 dup (0)

end_prog:

.code

org 100h
locals

start: jmp main

new_int_9h proc
	push ax

	in al, 60h
	push ax

	in al, 61h
	and al, 01111111b
	out 61h, al
	or al, 80h
	out 61h, al

	mov al, 20h
	in al, 20h

	pop ax
	mov ah, 80h
	cmp ah, al
	jbe @@end_int_9h

	cmp al, 02h
	jne @@check_off

		cmp cs:[IsMemoryBack], 0
		jne @@end_int_9h

		mov cs:[IsMemoryBack], 1
		mov cs:[ShowIsOn], 1


		push bx es cx dx si di ds

		call copy_in_back_buffer
		mov bx, 0b800h
		mov es, bx
		mov bx, cs
		mov ds, bx
		
		mov ax, [Coordinates]
		mov cx, [LenghtFrame]
		mov dx, [HeightFrame]
		mov bl, [ColorFrame]
		mov si, offset FrameStyle
		call print_frame
		pop ds di si dx cx es bx

		jmp @@end_int_9h

	@@check_off:
		cmp al, 03h
		jne @@end_int_9h

		cmp cs:[IsMemoryBack], 0
		je @@end_int_9h

		mov cs:[IsMemoryBack], 0
		mov cs:[ShowIsOn], 0

		push bx es cx dx si di ds
		call copy_in_video_memory
		pop ds di si dx cx es bx	
	
	@@end_int_9h:
	pop ax

	db 0eah
OldISR  dd 0bad0badh

	iret
	endp


new_int_8h proc
	push es ds ax bx cx dx si di bp

	push ax
	in al, 61h
	and al, 01111111b
	out 61h, al
	or al, 80h
	out 61h, al

	mov al, 20h
	out 20h, al
	pop ax

	cmp cs:[ShowIsOn], 0
	je @@end_int_8h

	push bx
	mov bx, cs
	mov ds, bx
	pop bx

	call print_cenral_frame

	@@end_int_8h:
	pop bp di si dx cx bx ax ds es


	db 0eah
OldISR8h  dd 0bad0badh

	iret
	endp

;_______________________________
;Entry: None
;Exit:  None
;Destr: cx, bx, dx, ax, ds, es, si, di
;_______________________________
copy_in_back_buffer proc
	mov bx, 0b800h
	mov ds, bx
	mov ax, cs:[Coordinates]
	call give_adress
	mov si, di

	mov bx, cs
	mov es, bx
	mov di, offset BackBuffer

	mov dx, cs:[HeightFrame]
	inc dx
	inc dx

	xor ax, ax

	jmp @@cond

	@@copy:
		mov cx, cs:[LenghtFrame]
		inc cx
		inc cx

		push si
		rep movsw
		pop si
		add si, cs:[LenghtScreen]

		dec dx
	
	@@cond:
		cmp ax, dx
		jb @@copy 
	
	ret
	endp

;_______________________________
;Entry: None
;Exit:  None
;Destr: cx, bx, dx, ax, ds, es, si, di
;_______________________________
copy_in_video_memory proc
	mov bx, 0b800h
	mov es, bx

	mov ax, cs:[Coordinates]
	call give_adress

	mov bx, cs
	mov ds, bx
	mov si, offset BackBuffer

	mov dx, cs:[HeightFrame]
	inc dx
	inc dx

	xor ax, ax

	jmp @@cond

	@@copy:
		mov cx, cs:[LenghtFrame]
		inc cx
		inc cx

		push di
		rep movsw
		pop di
		add di, cs:[LenghtScreen]

		dec dx
	
	@@cond:
		cmp ax, dx
		jb @@copy 
	
	ret 
	endp

;_______________________________
;Entry: value every register
;Exit:  None
;Destr: ax, bx, cx, dx, si, di, bp, es
;_______________________________
print_cenral_frame proc
	push cs ss es ds sp bp di si dx cx bx ax

	mov bx, 0b800h
	mov es, bx

	mov ax, cs:[Coordinates]
	inc al
	call give_adress

	mov ax, cs:[IndentationFrame]
	shl ax, 1
	add di, ax

	mov si, offset ArrayNameRegister

	xor dx, dx
	jmp .cond_L1

	.L1:
		pop bx
		lodsw
		mov cx, ax
		lodsw
		push si
		mov si, ax
		push dx di
		call print_register
		pop di dx si
		
		add di, cs:[LenghtScreen]
		inc dx
	
	.cond_L1:
		cmp dx, cs:[NumberRegister]
		jb .L1

	ret
	endp


;_______________________________
;Entry: bx = value register
;       si = adress name register
;       di = adress video print
;       cx = size name register
;Exit:  None
;Destr: ax, cx, si, di, dx, bp
;_______________________________
print_register proc
	mov ah, cs:[ColorRegister]
	.print_str:
		lodsb 
		stosw
		loop .print_str

	inc di
	inc di

	mov cl, 12
	xor ch, ch
	mov bp, 000fh;
	mov dx, bx

	jmp .cond_print_number
	
	.print_number:
		shr bx, cl
		and bx, bp

		cmp bx, 9
		ja .print_letter

			add bl, 30h
			mov al, bl
			jmp .end

		.print_letter:
			add bl, 37h
			mov al, bl
		
		.end:
		stosw
		sub cl, 4
		inc ch
		mov bx, dx
	
	.cond_print_number:
		cmp ch, 4
		jb .print_number
	
	ret
	endp

;_______________________________
;Entry: ah = horisontale offset
;       al = verticale offset
;       cx = horisontale size
;       dx = verticale size
;       bl = color
;       si = frame style
;Exit:  None
;Destr: ax, dx, cx, si, di
;_______________________________
print_frame proc
	push bx
	push dx
	call give_adress
	pop dx
	pop bx

	push di
	push cx
	call print_str
	pop cx
	pop di

	add di, cs:[LenghtScreen]

	jmp cond_print_frame1
	xor dx, dx
	
	cycle_print_frame1:
		push cx
		push di
		call print_str
		sub si, cs:[SizeStrStyle]
		pop di
		pop cx
		
		add di, cs:[LenghtScreen]
		dec dx
	
	cond_print_frame1:
		xor ax, ax
		cmp ax, dx
		jb cycle_print_frame1

	add si, cs:[SizeStrStyle]
	call print_str	
	ret
	endp

;_______________________________
;Entry: ah = horisontale offset
;       al = verticale offset                                                   
;Exit:  di = start_adress
;Destr: dx, ax, bx
;_______________________________
give_adress     proc
	mov bx, ax
	mov dx, cs:[LenghtScreen]
	mul dl
	mov di, ax

	mov ax, bx
	shr ax, 7
	add di, ax

	ret	
	endp

;_______________________________
;Entry: si = frame_style
;       di = adress_video_memory
;       cx = lenght
;       bl = color
;Exit:  None
;Destr: cx, si, di
;_______________________________ 
print_str	proc
	lodsb
	mov ah, bl
	stosw
	
	lodsb
	rep stosw
	
	lodsb 
	stosw
	ret
	endp
	  

main:
		call copy_in_back_buffer
		call copy_in_video_memory
		mov ax, 0
        mov ds, ax
        mov si, 09h*4
        mov ax, cs
        mov es, ax
        mov di, offset OldISR
    	movsw
		movsw

        mov ax, 0            
        mov ds, ax
        mov bx, 09h*4

    	mov ax, offset new_int_9h
        mov [bx], ax
        mov ax, es
        mov [bx+2], es

		mov ax, 0
		mov ds, ax
		mov si, 08h*4
		mov ax, cs
		mov es, ax
		mov di, offset OldISR8h
		movsw
		movsw

		mov ax, 0
		mov ds, ax
		mov bx, 08h*4

		mov ax, offset new_int_8h
		mov [bx], ax
        mov ax, es
        mov [bx+2], es	
				
		mov dx, offset end_prog 
        ;shr dx, 4
        ;inc dx
        mov ax, 3100h
        int 21h

end start