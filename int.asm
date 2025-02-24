.model tiny
.data

FrameStyle              db 0dah, 0c4h, 0bfh, 0b3h, 20h, 0b3h, 0c0h, 0c4h, 0d9h
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

ArrayNameRegister       dw 2, offset NameAx   ;{size_name_register, name_register}
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
SizeBuffer              dw 182

MaxBitShift             db 12
NumberDigitInRegister   db 4

end_prog:

.code

org 100h
locals

start: jmp main

main: 
		mov ax, 0
        mov ds, ax                   
        mov si, 09h*4            
        mov ax, cs
        mov es, ax
        mov di, offset OldISR
    	movsw
		movsw                         ;OldISR = {old segment int9h, old adress int9h}

        mov ax, 0            
        mov ds, ax
        mov bx, 09h*4                 ;ds:bx = adress old int9h

    	mov ax, offset new_int_9h
        mov [bx], ax
        mov ax, es
        mov [bx+2], es                ;int 9h = new_int_9h

		mov ax, 0
		mov ds, ax
		mov si, 08h*4
		mov ax, cs
		mov es, ax
		mov di, offset OldISR8h
		movsw
		movsw                         ;OldISR8h = {old segment int8h, old adress int8h}

		mov ax, 0
		mov ds, ax
		mov bx, 08h*4                 ;ds:bx = adress old int8h

		mov ax, offset new_int_8h
		mov [bx], ax
        mov ax, es
        mov [bx+2], es                ;int 8h = new_int_8h	
				
		mov dx, offset end_prog       ;dx = size programm in byte 
        shr dx, 4
        inc dx                        ;dx = size programm in paragraph (16 byte)
        mov ax, 3100h                 ;terminate the program with code 0 and leave it in the resident mode
        int 21h


new_int_9h proc
	push ax

	in al, 60h
	push ax

	in al, 61h
	and al, 01111111b
	out 61h, al                    ;changing the high-order bit in the interrupt controller port
	or al, 80h
	out 61h, al                    ;write old high-order bit in the interrupt controller port         

	mov al, 20h
	in al, 20h                     ;write number 20h in 20 port

	pop ax
	mov ah, 80h
	cmp ah, al                     
	jbe @@end_int_9h               ;checking the scancode of the pressed key with the scancode '1'

	cmp al, 02h
	jne @@check_off

		cmp cs:[IsMemoryBack], 0
		jne @@end_int_9h           ;checking whether the '1' key has been pressed

		mov cs:[IsMemoryBack], 1   ;remember that the '1' key is pressed
		mov cs:[ShowIsOn], 1       ;saving a value to a variable indicating that interrupting the timer should display a picture


		push bx es cx dx si di ds  

		mov bx, cs
		mov es, bx
		mov di, offset BackBuffer
		call copy_in_back_buffer   ;copying the background from where the frame should be

		pop ds di si dx cx es bx

		jmp @@end_int_9h

	@@check_off:
		cmp al, 03h
		jne @@end_int_9h            ;checking the scancode of the pressed key with the scancode '1'

		cmp cs:[IsMemoryBack], 0
		je @@end_int_9h             ;checking whether the '2' key has been pressed

		mov cs:[IsMemoryBack], 0    ;remember that the '2' key is pressed
		mov cs:[ShowIsOn], 0        ;saving in var value indicating that interrupting the timer don't should display a picture

		push bx es cx dx si di ds
		mov bx, cs
		mov ds, bx
		mov si, offset BackBuffer
		call copy_in_video_memory   ;copying the old background to the video memory
		pop ds di si dx cx es bx	
	
	@@end_int_9h:
	pop ax

	db 0eah
OldISR  dd 0bad0badh                ;call old interrupting the keyboard

	iret
	endp


new_int_8h proc
	push es ds ax bx cx dx si di bp

	push ax
	in al, 61h
	and al, 01111111b               ;changing the high-order bit in the interrupt controller port   
	out 61h, al                      
	or al, 80h
	out 61h, al                     ;write old high-order bit in the interrupt controller port 

	mov al, 20h
	out 20h, al                     ;write number 20h in 20 port
	pop ax

	cmp cs:[ShowIsOn], 0
	je @@end_int_8h                 ;checking if the frame needs to be printed

	push es ds ax bx cx dx si di bp
	call strcmp_buffers             ;checking whether other programs have changed the background. If you have changed, save the changes in BackBuffer

	mov cx, cs:[LenghtFrame]
	mov dx, cs:[HeightFrame]
	mov bl, cs:[ColorFrame]
	mov si, offset FrameStyle       
	call print_frame                ;print frame in DrawBuffer

	pop bp di si dx cx bx ax ds es
	call print_cenral_frame         ;print the value of the registers in the central part of the frame in DrawBuffer

	mov bx, cs
	mov ds, bx
	mov si, offset DrawBuffer
	call copy_in_video_memory       ;copying from DrawBuffer to video memory

	@@end_int_8h:
	pop bp di si dx cx bx ax ds es


	db 0eah
OldISR8h  dd 0bad0badh              ;call old interrupting the timer

	iret
	endp

;_______________________________
;Entry: None
;Exit:  None
;Destr: ax, bx, cx, dx, si, di, ds, es
;_______________________________
strcmp_buffers proc
	mov bx, cs	
	mov ds, bx
	mov si, offset DrawBuffer       ;ds:[si] = ptr on the DrawBuffer

	mov bx, 0b800h
	mov es, bx                      ;es indicates the beginning of video memory
	mov ax, cs:[Coordinates]

	call give_adress                ;di = the address of the beginning of the video memory, which our program uses

	mov bx, cs:[HeightFrame]
	inc bx
	inc bx                          ;bx = HeightFrame + 2

	xor dx, dx
	@@L1:
		mov cx, [LenghtFrame]
		inc cx
		inc cx
		inc cx                      ;cx = LenghtFrame + 2

		push di                   
		@@L2:
			repe cmpsw              ;compares ds:si and es:di lines until the lines run out or until a difference is found
			xor ax, ax
			cmp ax, cx
			jb @@mov_back_buffer    ;checking if the line has ended

			jmp @@end_L2

			@@mov_back_buffer:                                     ;if the line has not ended
				mov ax, es:[di - 2] 
				push si
				sub si, offset DrawBuffer - offset BackBuffer 
				mov [si - 2], ax                                   ;BackBuffer[si - 2] = DrawBuffer[di - 2]
				pop si
				jmp @@L2
			
	@@end_L2:
		dec si
		dec si
		
		pop di
		add di, cs:[LenghtScreen]
		inc dx
		cmp dx, bx
		jb @@L1                      ;repeat this until the area of our video memory runs out

	ret 
	endp 
				

;_______________________________
;Entry: es:[si] - adress buffer
;Exit:  None
;Destr: cx, bx, dx, ax, ds, es, si, di
;_______________________________
copy_in_back_buffer proc
	mov bx, 0b800h
	mov ds, bx                       ;ds = indicates the beginning of video memory
	mov ax, cs:[Coordinates] 
	push di
	call give_adress                 ;di = the address of the beginning of the video memory, which our program uses
	mov si, di
	pop di

	mov dx, cs:[HeightFrame]
	inc dx
	inc dx                           ;dx = HeightFrame + 2

	xor ax, ax

	jmp @@cond

	@@copy:
		mov cx, cs:[LenghtFrame]
		inc cx
		inc cx                       ;cx = LenghtFrame + 2

		push si
		rep movsw                    ;copying a line of video memory to es:[si]
		pop si
		add si, cs:[LenghtScreen]

		dec dx
	
	@@cond:
		cmp ax, dx
		jb @@copy                    ;repeat until we copy all the lines to es:[di]
	
	ret
	endp

;_______________________________
;Entry: ds:[si] - adress buffer
;Exit:  None
;Destr: cx, bx, dx, ax, ds, es, si, di
;_______________________________
copy_in_video_memory proc
	mov bx, 0b800h                   
	mov es, bx                       ;es = indicates the beginning of video memory

	mov ax, cs:[Coordinates]
	call give_adress

	mov dx, cs:[HeightFrame]
	inc dx
	inc dx                           ;dx = HeightFrame + 2

	xor ax, ax

	jmp @@cond

	@@copy:
		mov cx, cs:[LenghtFrame]
		inc cx
		inc cx                       ;cx = LenghtFrame + 2

		push di
		rep movsw                    ;copying a line of ds:[si] to video memory
		pop di
		add di, cs:[LenghtScreen]    ;di = adress video memory on new line

		dec dx
	
	@@cond:
		cmp ax, dx
		jb @@copy                    ;repeat until we copy all the lines to video memory                  
	
	ret 
	endp

;_______________________________
;Entry: value every register
;Exit:  None
;Destr: ax, bx, cx, dx, si, di, bp, es
;_______________________________
print_cenral_frame proc
	push cs ss es ds sp bp di si dx cx bx ax

	mov bx, cs
	mov es, bx
	mov ds, bx                       ;ds = cs; es = cs

	mov di, offset DrawBuffer
	mov cx, cs:[LenghtFrame]
	inc cx
	inc cx                           
	add cx, cs:[IndentationFrame]
	shl cx, 1                        ;cx = 2 * (LenghtFrame + 2 + IndentationFrame)

	add di, cx                       ;di = address where to write the string with the case value

	mov si, offset ArrayNameRegister 

	xor dx, dx
	jmp .cond_L1

	.L1:
		pop bx                       ;bx = value print register
		lodsw                         
		mov cx, ax                   ;cx = size_name_register
		lodsw
		push si                      
		mov si, ax                   ;si = char* name_print_register
		push dx di
		call print_register          ;copy in DrawBuffer str "name_register nember". Example: "ax 01AE"
		pop di dx si                
		
		mov ax, cs:[LenghtFrame]     
		inc ax
		inc ax
		shl ax, 1                    ;ax = 2 * (LenghtFrame + 2)
		add di, ax      
		inc dx                       ;di += ax + 1
	
	.cond_L1:
		cmp dx, cs:[NumberRegister]
		jb .L1                       ;repeat until we print all the registers.

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
	@@print_str:
		lodsb 
		stosw
		loop @@print_str              ;copy name register in es:[di]

	inc di
	inc di                            ;skip one symbol

	mov cl, cs:[MaxBitShift]          ;cl = max bit shift. cl = 8*sizeof(register) - 4
	xor ch, ch
	mov bp, 000fh;                    ;bp = mask to separate the lower 4 bits
	mov dx, bx                        ;dx = value register

	jmp @@cond_print_number
	
	@@print_number:
		shr bx, cl
		and bx, bp                    ;we select 4 bits of the number

		cmp bx, 9
		ja .print_letter              ;we check that when writing a character, you need to write it as a number or letter

			add bl, 30h
			mov al, bl                ;al = ASCI code bl
			jmp .end

		.print_letter:
			add bl, 37h
			mov al, bl                ;al = ASCI code bl
		
		.end:
		stosw                         ;copy ASCI code in DrawBuffer
		sub cl, 4                     ;changing the bit shift to highlight the higher digit
		inc ch
		mov bx, dx                    ;bx = the initial value of the register
	
	@@cond_print_number:
		cmp ch, cs:[NumberDigitInRegister]
		jb @@print_number              ;repeat until we have highlighted all the numbers in the register.
	
	ret
	endp

;_______________________________
;Entry: cx = horisontale size
;       dx = verticale size
;       bl = color
;       si = frame style
;Exit:  None
;Destr: ax, dx, cx, si, di
;_______________________________
print_frame proc
	mov ax, cs
	mov es, ax                        ;es = cs
	mov ds, ax                        ;ds = cs
	mov di, offset DrawBuffer

	push cx
	call print_str                    ;copying the top row of the frame. Si indicates the style of the center row of the frame
	pop cx

	jmp @@cond_print_frame1
	xor dx, dx
	
	@@cycle_print_frame1:
		push cx
		call print_str               ;copying the central line of the frame. Si indicates the style of the lower one row of the frame 
		sub si, cs:[SizeStrStyle]    ;Si indicates the style of the center row of the frame  
		pop cx
		
		dec dx
	
	@@cond_print_frame1:
		xor ax, ax
		cmp ax, dx
		jb @@cycle_print_frame1        ;repeat until all the central lines are printed

	add si, cs:[SizeStrStyle]
	call print_str	                 ;copying the lower one line of the frame
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
	mov di, ax                       ;di = al * LenghtScreen

	mov ax, bx
	shr ax, 7                        ;al = ah * 2;
	add di, ax                       ;di = al * LenghtScreen + ah * 2

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
	stosw                             ;copy first symbol
	
	lodsb
	rep stosw                         ;copy central symbol
	
	lodsb 
	stosw                             ;copy last symbol
	ret
	endp

end start