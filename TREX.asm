[org 0x0100]
jmp start
gameOver:db 'Game is over'
gameOverLen: dw 12
dinopositions: dw 1292,1290,1288,1286,1452,1450,1448,1446,1606,1608,1610,1764,1766,1768,1926,1928
dinoAttributes:dw 0x00DC,0x00F9,0x00DC,0x00DC,0x00DF,0x00DF,0x00DB,0x00DB,0x00DB,0x00DB,0x00AA,0x00F4,0x00DB,0x00DB,0x004C,0x004C
;;;;;;;;;;;hurdlles array;;
dipositions: dw 2076,2136,1916,2078,2196,1916,2078
;;;score;;;
scorecounter: dw 0 ;;;;;;;;;;;;;;;;
Scoredisplay: db 'Score:0'

flag: dw 0
tempUp:dw 6
counter: dw 0
counter2:dw 0

SCORE_DI: dw 142
NAMEOFGAME: db 'WELCOME TO T-REX GAME'
START: db 'Press Enter to Start!!'
GAMEOVER: db 'GAME OVER!!!'
Gameover_flag: db 0
     
sound: dw 1200,6449,4000,2200

printnum:
push bp
mov bp,sp
pusha
push 0xb800
pop es
mov bx,scorecounter

mov ax,[bx]
mov bx,10
mov cx,0


nextdigit1:
mov  dx, 0              ; zero upper half of dividend          
div  bx                 ; divide by 10         
add  dl, 0x30           ; convert digit into ascii value    
push dx                 ; save ascii value on stack    
inc  cx                 ; increment count of values      
cmp  ax, 0              ; is the quotient zero           
jnz  nextdigit1  
mov di,[SCORE_DI]
nextposition1:     
pop  dx                 ; remove a digit from the stack      
mov  dh, 0x07           ; use normal attribute          
mov [es:di], dx         ; print char on screen            
add  di, 2              ; move to next screen location      
loop nextposition1

popa
pop bp
ret


;;;;;;;;;;;;;;;;;;;;;;;SOUND;;;;;;;;;;;;;
SOUND:
	push bp
	mov bp,sp
	pusha
        mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
        mov     ax, [bp+4] ;1612       ;6449        ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 1          ; Pause for duration of note.
pause1:
        mov     cx, 65535
pause2:
        dec     cx
        jne     pause2
        dec     bx
        jne     pause1
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.
	
	popa
	pop bp
	ret 2			   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Scoring Subroutine;;;;;;;;;;;;;;;;;;
scoring:
push bp
mov bp,sp

push ax
push cx
push bx
push di
push dx

push 0xb800
pop es

mov bx,scorecounter
add word [bx],1
mov ax,[bx]
mov bx,10
mov cx,0
;;;;;;;;;;;;;;;;
nextdigit:
mov  dx, 0              ; zero upper half of dividend          
div  bx                 ; divide by 10         
add  dl, 0x30           ; convert digit into ascii value    
push dx                 ; save ascii value on stack    
inc  cx                 ; increment count of values      
cmp  ax, 0              ; is the quotient zero           
jnz  nextdigit  
mov di,[SCORE_DI]
nextposition:     
pop  dx                 ; remove a digit from the stack      
mov  dh, 0x07           ; use normal attribute          
mov [es:di], dx         ; print char on screen            
add  di, 2              ; move to next screen location      
loop nextposition


pop dx
pop di
pop bx
pop cx
pop ax
pop bp

ret
;;;;;;;;;;;;;;;;;;;;;clear screeen;;;;;;;;;
clearscreen:
push es
push ax
push di
mov ax,0xb800
mov es,ax
mov di,0
nextchar:
mov word [es:di],0x0720
add di,2
cmp di,4000
jne nextchar
pop  di
pop  ax               
pop  es               
ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;print START;;;;;;;;;;;;;;;;;;
printStart:
pusha

	
mov ax,0xb800
mov es,ax
mov ah,10001100b
mov si,START
mov di,1820
mov cx,22
PS:	 
	 lodsb
	 stosw
	 loop PS

mov ah,00001010b	 
mov si,NAMEOFGAME
mov di,1500
mov cx,21
PN:
	 lodsb
	 stosw
	 loop PN

	 
popa
ret
;;;;;;;;;;;;;;;;;;;;;;PRINT BACKGROUND;;;;;;;;;;;;;;;;;;;;
printBG:
push bp
mov bp,sp
pusha
mov ax,0xb800
mov es,ax
mov di,0
mov si,0
mov cx,80
mov di,2080

lop:
mov al,2dh
mov ah,0x07
stosw
loop lop

mov al,0xDC        ;Dino head
mov ah,07h
mov di,1292
stosw
mov al,0xDF
mov ah,07h
mov di,1452
stosw

mov al,0xF9        ;Dino head
mov ah,07h
mov di,1290
stosw

mov al,0xDF
mov ah,07h
mov di,1450
stosw

mov al,0xDC        ;Dino head
mov ah,07h
mov di,1288
stosw

mov al,0xDB
mov ah,07h
mov di,1448
stosw

mov al,0xDC        ;Dino head
mov ah,07h
mov di,1286
stosw

mov al,0xDB
mov ah,07h
mov di,1446
stosw

                  ;Dino belly
mov al,0xDB      
mov ah,07h
mov di,1606
stosw

mov al,0xDB
mov ah,07h
mov di,1608
stosw

mov al,0xAA
mov ah,07h
mov di,1610
stosw


mov al,0xDB      
mov ah,07h
mov di,1766
stosw

mov al,0xF4      
mov ah,07h
mov di,1764
stosw


mov al,0xDB
mov ah,07h
mov di,1768
stosw
			  
mov al,'L'      
mov ah,07h
mov di,1926
stosw
			  
mov al,'L'
mov ah,07h
mov di,1928
stosw

;;;;;;;scoreka display
push cx
mov si,Scoredisplay
mov ah,0x07
mov di,130
mov cx,7
DispScore:	 
lodsb
stosw
loop DispScore	 
pop cx	 


;;;;;;;;;;;;;;;;;;;;;clouds

mov cx,3
mov di,560
cloud1:
mov al,0xDB
stosw
loop cloud1

mov cx,3
mov di,720
cloud2:
mov al,0xDB
stosw
loop cloud2

mov cx,3
mov di,880
cloud3:
mov al,0xDB
stosw
loop cloud3

mov di,558
mov al,0xDC
stosw

mov di,718
mov al,0xDB
stosw

mov di,878
mov al,0xDB
stosw

mov di,716
mov al,0xDB
stosw

mov di,876
mov al,0xDB
stosw

mov di,714
mov al,0xDB
stosw

mov di,874
mov al,0xDF
stosw

mov di,712
mov al,0xDC
stosw

mov di,566
mov al,0xDC
stosw

mov di,726
mov al,0xDB
stosw

mov di,886
mov al,0xDB
stosw

mov di,728
mov al,0xDB
stosw

mov di,888
mov al,0xDB
stosw

mov di,730
mov al,0xDB
stosw

mov di,890
mov al,0xDF
stosw

;;;;;;;;;;;;;;;;;;;;;;;;
mov di,2240
mov cx,880
grass:
mov ah,0x02
mov al,0xB0
stosw
loop grass
;;;;;;;;;;;;;;;;;;;;;moon
mov di,470
mov ah,0x07
mov al,0xDC
stosw
mov ah,0x07
mov al,0xDC
stosw

mov di,630
mov al,0xDF
stosw

mov al,0xDF
stosw
;mov di,2000
;mov al,0xdb;
;mov ah,0x07
;stosw
popa
pop bp
ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MOVE BACKGROUND;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
movbg:
push bp
mov bp,sp
push ax
push cx
push es

push cs
pop ds
;;;;;;;;
;;;;;
;;;;;;;;
mov ax,0xb800
mov es,ax
mov si,0
mov cx,3
mov bx,[bp+4]
mainwork:

mov di,[bx+si]
cmp di,2078 ;;;;;;;;;;
jae onlydec
;;;;;;;;;
cmp cx,3
jne otherhurdles
mov al,0x20
mov ah,0x07
stosw
sub di,2
jmp compares

otherhurdles:
push si ;;;;;;;;
mov al,0x20
mov ah,0x07
stosw
sub di,2
add si,2
mov di,[bx+si]
stosw
add si,2
mov di,[bx+si]
stosw
pop si

mov di,[bx+si]

;;;;;;;;;;
compares:
cmp cx,3
je moving1
cmp cx,2
je moving2
cmp cx,1
je moving3sj ;;;shortjump error
;;;;;;;;;;

nextpos:
add si,2
loop mainwork
jmp pops
;;;
onlydec:
sub di,2
mov [bx+si],di ;;;;;decremented value
add si,4 ;;;;;;;;;;
jmp nextpos

;;;;;;;; 
moving1:
sub di,2
cmp di,1920
jbe l4
ja l5
l4:
;;;;;;;;;;;;;;;score increment
call scoring
;;;;;;;;;;;;;;;;;;;;;
mov di,2076;;;;;2078      ;;;;;;

l5:
mov al,0xdb
mov ah,0x07
stosw
sub di,2
mov [bx+si],di

jmp nextpos
;;;;;;
moving3sj:
jmp moving3;;;;;;;;;
;;;;;;;
moving2:
sub di,2
cmp di,1920
jbe l6
ja l7
l6:
;;;;;;;;;;;;;;;;;;scoreinc

call scoring
;;;;;;;;;;;;;;;;;;;;;
mov di,2076
push si
add si,2
mov dx,1918;1916
mov [bx+si],dx
add si,2
mov dx,2080;2078
mov [bx+si],dx
pop si
l7:
mov al,0xdb
mov ah,0x07
stosw
sub di,2
mov [bx+si],di ;;;;value of first part of hurdle in array
add si,2
mov di,[bx+si]
sub di,2;;;;;;
mov al,0xdb
stosw
sub di,2
mov [bx+si],di ;;;value of 2nd part of hu

add si,2
mov di,[bx+si]
sub di,2;;;;;;;;;;;
mov al,0xdf
stosw
sub di,2
mov [bx+si],di

jmp nextpos
;;;;;;
moving3:
sub di,2
cmp di,1920
jbe l8
ja l9
l8:
;;;;;;;;;;;;;;;;;;scoreinc
call scoring

;;;;;;;;;;;;;;;;;;;;;

mov di,2076
push si
add si,2
mov dx,1918;1916
mov [bx+si],dx
add si,2
mov dx,2080;2078
mov [bx+si],dx
pop si

l9:
mov al,0xdb
mov ah,0x07
stosw
sub di,2
mov [bx+si],di ;;;;value of hurdle in array
add si,2
mov di,[bx+si]
sub di,2;;;;;;
mov al,0xdb
stosw
sub di,2
mov [bx+si],di ;;;value of 2nd part of hu

add si,2
mov di,[bx+si]
sub di,2;;;;;
mov al,0xdb
stosw
sub di,2
mov [bx+si],di

jmp nextpos

pops:

pop es
pop cx
pop ax
pop bp
ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;movement of player;;;;;;;;;;;
movPlayer:
push bp
mov bp,sp
pusha

push 0xb800
pop es
mov bx,dinopositions;;;;;;;;;;
;;;mov bx,[bp+4]   ;moving player pos in bx
mov si,0
mov cx,6
jumpingUp:
mov di,[bx+si]
push bx
mov bx,dinoAttributes
mov dx,[bx+si]
mov ax,dx
mov ah,0x07
sub di,160
stosw
sub di,2
pop bx
mov [bx+si],di
add di,160
mov ax,0x0720
stosw
sub di,2
add si,2
cmp si,32
jnz jumpingUp
mov si,0

loop jumpingUp

popa 
pop bp
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;my isr for int 8;;;;;;;;;;;;;;;
jumpDown:
push bp
mov bp,sp
pusha
push 0xb800
pop es

mov bx,dinopositions;;;;;;;;;;
;;;mov bx,[bp+4]   ;moving player pos in bx
mov si,30
mov cx,6
jumpingDown:
mov di,[bx+si]
push bx
mov bx,dinoAttributes
mov dx,[bx+si]
mov ax,dx
mov ah,0x07
add di,160
stosw
sub di,2
pop bx
mov [bx+si],di
sub di,160
mov ax,0x0720
stosw
sub di,2
sub si,2
cmp si,-2
jnz jumpingDown
mov si,30
loop jumpingDown

popa 
pop bp
ret
;;;;;;;;;;;;overlapping
overlapping:
push bp
mov bp,sp
pusha
mov si,0
mov bx,dipositions
mov dx,[dinopositions+30] ;right feet
mov cx,7
carryOn:
mov dx,[dinopositions+30]
mov ax,[bx+si]
sub ax,dx
cmp ax,2
jz flagOne

mov ax,[bx+si]
sub dx,ax
cmp dx,2
jz flagOne

add si,2
loop carryOn

mov si,0
mov dx,[dinopositions+28]  ;left feet
mov cx,7
carryOn2:
mov dx,[dinopositions+28]
mov ax,[bx+si]
sub ax,dx
cmp ax,2
jz flagOne
mov ax,[bx+si]
sub dx,ax
cmp dx,2
jz flagOne

add si,2
loop carryOn2


mov si,0
mov dx,[dinopositions+22]   ;tail
mov cx,7
carryOn3:
mov dx,[dinopositions+22]
mov ax,[bx+si]
sub ax,dx
cmp ax,2
jz flagOne
mov ax,[bx+si]
sub dx,ax
cmp dx,2
jz flagOne
add si,2
loop carryOn3

jmp popping
flagOne:
mov word[flag],1
popping:
popa
pop bp
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

myisr:                   ;time isr
pusha 
mov ax,dipositions
push ax
cmp word[flag],1       ;if flag is one stop
jz stop

cmp word[dinopositions],332   ;;cmp if player at that position to jump down
jz Func2 
jnz notdown
Func2:
inc word[counter2]
cmp word[counter2],12         ;;pass 55ms 8 times then com down
jz down
jnz notdown
down:
call jumpDown
mov word [counter2],0
notdown:
inc word[counter]              ;incrementing counter
cmp word[counter],1
jz callfunction
jnz continue
callfunction:
call movbg
mov word[counter],0
;;;;;;;;;;;;;;;;;;;overlapping
call overlapping

continue:
mov ax,0
mov al,0x20
out 0x20,al

stop:
cmp word[flag],1
jz cout
jnz notCout
cout:

push word [sound+4]
call SOUND
push 0xb800         ;Printing Game over
pop es
mov si,GAMEOVER
mov cx,12
mov ah,10001100b
mov di,1820
gameEnd:
lodsb
stosw
loop gameEnd

push cx
mov si,Scoredisplay
mov ah,00001001b
mov di,1506
mov cx,7
DisplayScore:	 
	 lodsb
	 stosw
	 loop DisplayScore
mov di,1518
mov [SCORE_DI],di 
call printnum
pop cx	 
mov byte[Gameover_flag],1

notCout:
pop ax
popa
iret
;;;;;;;;;;;;;;;;;;;;kbisr;;;;;;;;;;;;;

kbisr:        ;keyboard
push ax
pusha
push ds
push cs
pop ds
in al,0x60
cmp al,57
jz callFunc
jnz popskbsir
callFunc:
;;;;;push plpositions
cmp word[dinopositions],1292
jz calling
jnz notCalling

calling:
call movPlayer
push word[sound];;;;;;;;;;;;;;;;;;;;;;;;;;;;
call SOUND


notCalling:
popskbsir:
pop ds
popa
mov al,0x20
out 0x20,al 
pop ax
iret

start:
call clearscreen
againPS:
call printStart ;;;;;;;prints start
mov ah,0
int 0x16
cmp ah,0x1c
jne againPS



call clearscreen
call printBG

xor ax,ax
mov es,ax
mov cx,0
cli
mov word[es: 0x9*4],kbisr
mov [es:0x9*4+2],cs

mov word[es: 0x8*4],myisr
mov [es:0x8*4+2],cs
sti

llll:
cmp byte[Gameover_flag],1
jne llll

termination:
mov  ax, 0x4c00         ; terminate and stay resident 
int  0x21