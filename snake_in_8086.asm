                            .Model Small
.Stack 100h


draw_row Macro x,y,size
    Local l1
; draws a line in row x from col 10 to col 300
    MOV AH, 0CH
    ;MOV AL, 1
    MOV CX, y
    MOV DX, x
   
    MOV BX,Y
    ADD BX,SIZE
    INC BX

L1: INT 10h
    INC CX
    CMP CX, BX
    JL L1
    
    EndM

draw_col Macro y,X,SIZE
    Local l2
; draws a line col y from row 10 to row 189
    MOV AH, 0CH
    ;MOV AL, 1
    MOV CX, y
    MOV DX, X
    
    MOV BX,X
    ADD BX,SIZE
    INC BX
       
L2: INT 10h
    INC DX
    CMP DX, BX
    JL L2
    EndM



    






;; USE RECTANGLE METHOD AS : RECTANGLE(X , Y  , X_LENGTH , Y_LENGTH , COLOR)
HORIZONTAL_LINE MACRO BEGIN_COL,END_COL,BEGIN_ROW,COLOR 
    LOCAL L1HOR
    MOV AH, 0CH
    MOV AL, COLOR
    MOV CX, BEGIN_COL; beginning col
    MOV DX, BEGIN_ROW ; beginning row
    L1HOR: INT 10h
        INC CX
        CMP CX , END_COL
        JLE L1HOR
    ENDM
    
HORIZONTAL_THICK MACRO BEGIN_COL,END_COL,BEGIN_ROW, THICK , COLOR
    LOCAL PRINTHOR
    MOV LINE_THICKNESS , THICK
    MOV BAL , BEGIN_ROW
    PRINTHOR:
        DEC LINE_THICKNESS
        HORIZONTAL_LINE BEGIN_COL , END_COL , BAL , COLOR
        INC BAL
        CMP LINE_THICKNESS ,0
        JGE PRINTHOR
    ENDM
    
RECTANGLE MACRO BEGIN_COL,BEGIN_ROW, END_COL , THICK , COLOR
MOV RLEN , END_COL
ADD RLEN , BEGIN_COL
HORIZONTAL_THICK BEGIN_COL, RLEN , BEGIN_ROW , THICK , COLOR
    ENDM
    
    



    
TEXT_MACRO MACRO PAGEB , ROWB , COLUMNB , MESSAGE
MOV AH,02           ;set cursor
MOV BH,0            ;page 0  
MOV DH , ROWB       ;row 0
MOV DL , COLUMNB    ;col 30
INT 10H

MOV AH,9H 
LEA DX, MESSAGE
INT 21H

ENDM











    
.DATA




;LAPPA   DB  '000000000$'
LINE_THICKNESS DW ?   
BAL DW ?
RLEN DW ?

SCORE_ARRAY      DB   '0000$' 
MESSAGE1  DB 'NEW GAME$'
MESSAGE2  DB 'HIGH SCORE$'
MESSAGE3  DB 'QUIT$'
SURE_MSG  DB 'ARE YOU SURE ?$'
YES_MSG   DB 'YES(Y)$'
NO_MSG   DB 'NO(N)$'
YOUR_SCORE_MSG DB 'YOUR SCORE$'
NUM DB ?  
HIGH_SCORE_ARRAY  DW  9 DUP(0) 
TEMPO DW ?
TEMP_SCORE_ARRAY_  DB  '000$'
FILENAME DB 'INPUT.txt',0
WRITE_FILE DB 'INPUT.txt',0
BUFFER DB 512 DUP (0) 
NAMEFLD DB 512 DUP(0) 


bufferlen dW ?
HS_LIST1   DB  '000$'
HS_LIST2   DB  '000$'
HS_LIST3   DB  '000$'
HIGH_SCORES_TEXT  DB 'HIGH SCORES$'


HANDLE1 DW ?
HANDLE DW ?
OPENERR DB 0DH,0AH 
ERRCODE DB 30H,'$'
WRITERR DB 0DH,0AH
TEMP DB ?

ARE_YOU_SURE_FLAG DB ?

;;; ARNAB'S SEGMENT
;;_________________________


CURRENT_SCORE    DW   0

SNAKE DW 100,110
      DW 100,100
      DW 100,90
      DW 100,80
      DW 100,70
      DW 100,60
      DW 100,50
      DW 100,40
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      DW ?,?
      

SNAKE_BOXSIZE DW 10

VAR1 DW ?
VAR2 DW ?
VAR3 DW ?
VAR4 DW ?


SNAKE_H_ROW DW ?
SNAKE_H_COL DW ?        
         
r dw ?
c dw ?
S dw ?


new_timer_vec   dw  ?,?
old_timer_vec   dw  ?,?
NEW_KEY_VEC DW  ?,?
OLD_KEY_VEC DW  ?,?
SCAN_CODE   DB  0
KEY_FLAG    DB  0
timer_flag  db  0

;CURRENT_SCORE DW ?

;scan codes
UP_ARROW = 72
DOWN_ARROW = 80
LEFT_ARROW = 75
RIGHT_ARROW = 77
ESC_KEY = 1
P_KEY = 25
N_KEY = 49
;

FLAG DW 0
BOUNDARY_FLAG DW ?
SNAKE_L DW 5

TIMER DW ?;;;;;;;;
TIMER_CON DW ? ;;;;;

BALL_ROW DW ?
BALL_COL DW ?

BALL_ARR DW 7,7,11,9,5,18,6,14,3,15,9,5,14,23,15,6,3,5

;BALL_ARR DW 7,7,11,9,11,12,11,10,5,18,6,14,3,15,9,5,14,23,15,6,3,5


BALL_ARR_SIZE DW 18
BALL_ARR_INDEX DW 0

SNAKE_L_CONST=25

SNAKE_H_BALL_FLAG DW ?
SNAKE_H_BODY_FLAG DW ?
BALL_BODY_FLAG DW ?





;__________________________

















;; WHOLE SCREEN SIZE  500 X 200

.CODE


HIGH_SCORE_UPDATE PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX

 LEA DX,FILENAME 
    MOV AL,0        
    CALL OPEN       
    JC OPEN_ERROR   
    
    
    MOV HANDLE1,AX   
   
    
    
    READ_LOOP:
    LEA DX, BUFFER 
    MOV BX,HANDLE1 
    CALL READ    
    OR AX,AX 
    JE EXIT
    MOV CX,AX 
    JMP READ_LOOP
    
    OPEN_ERROR:
    LEA DX,OPENERR 
    ADD ERRCODE,AL 
    MOV AH,9
    INT 21H 
    EXIT:    
    xor bx , bx
  

    LEA SI , BUFFER
    LEA DI , HIGH_SCORE_ARRAY 
    MOV CX , 3
   
    PRINT:
    MOV TEMPO , 0   
    MOV NUM , 100
    MOV AX , [SI + BX] 
    SUB AX , '0'
    MUL NUM 
    ADD TEMPO , AX
    MOV NUM , 10
    MOV AX , [SI + BX+1] 
    SUB AX , '0'
    MUL NUM 
    ADD TEMPO , AX
    MOV NUM , 1
    MOV AX , [SI + BX + 2] 
    SUB AX , '0'
    MUL NUM 
    ADD TEMPO , AX
    MOV DX , TEMPO
    MOV HIGH_SCORE_ARRAY[BX] , DX 
    
    ADD BX , 3
    LOOP PRINT  
    
    
    
    READ_LABEL_END:
    
    MOV DX , HIGH_SCORE_ARRAY[0]
    CMP DX , HIGH_SCORE_ARRAY[3]
    JGE NEXT1
    XCHG DX  , HIGH_SCORE_ARRAY[3]
    MOV HIGH_SCORE_ARRAY[0] , DX
    
    NEXT1:
    
    MOV DX , HIGH_SCORE_ARRAY[6]
    CMP DX , CURRENT_SCORE
    JGE NEXT2
    XCHG DX  , CURRENT_SCORE
    MOV HIGH_SCORE_ARRAY[6] , DX
    
    NEXT2:
    
    MOV DX , HIGH_SCORE_ARRAY[0]
    CMP DX , HIGH_SCORE_ARRAY[6]
    JGE NEXT3
    XCHG DX  , HIGH_SCORE_ARRAY[6]
    MOV HIGH_SCORE_ARRAY[0] , DX
    NEXT3:
    
    MOV DX , HIGH_SCORE_ARRAY[6]
    CMP DX , CURRENT_SCORE
    JGE NEXT4
    XCHG DX  , CURRENT_SCORE
    MOV HIGH_SCORE_ARRAY[6] , DX
    NEXT4: 
    
    MOV DX , HIGH_SCORE_ARRAY[3]
    CMP DX , HIGH_SCORE_ARRAY[6]
    JGE NEXT5
    XCHG DX , HIGH_SCORE_ARRAY[6]
    MOV HIGH_SCORE_ARRAY[3] , DX
    NEXT5 :
    
    MOV DX , HIGH_SCORE_ARRAY[6]
    CMP DX , CURRENT_SCORE
    JGE NEXT6
    XCHG DX  , CURRENT_SCORE
    MOV HIGH_SCORE_ARRAY[6] , DX
    NEXT6: 
    
    MOV AX ,HIGH_SCORE_ARRAY[0] 
    CALL TEMP_SCORE_ARRAY
    LEA SI , NAMEFLD 
    MOV DL , TEMP_SCORE_ARRAY_[0]
    MOV [SI + 0] , DL
    MOV DL , TEMP_SCORE_ARRAY_[1]
    MOV [SI + 1] , DL 
    MOV DL , TEMP_SCORE_ARRAY_[2]
    MOV [SI + 2] , DL
    MOV DL , TEMP_SCORE_ARRAY_[0]
    MOV HS_LIST1[0] , DL
     MOV DL , TEMP_SCORE_ARRAY_[1]
    MOV HS_LIST1[1] , DL
     MOV DL , TEMP_SCORE_ARRAY_[2]
    MOV HS_LIST1[2] , DL  
    
    
    
    MOV AX ,HIGH_SCORE_ARRAY[3] 
    CALL TEMP_SCORE_ARRAY
    LEA SI , NAMEFLD
    MOV DL , TEMP_SCORE_ARRAY_[0]
    MOV [SI + 3] , DL
    MOV DL , TEMP_SCORE_ARRAY_[1]
    MOV [SI + 4] , DL
    MOV DL , TEMP_SCORE_ARRAY_[2]
    MOV [SI + 5] , DL 
     MOV DL , TEMP_SCORE_ARRAY_[0]
    MOV HS_LIST2[0] , DL
     MOV DL , TEMP_SCORE_ARRAY_[1]
    MOV HS_LIST2[1] , DL
     MOV DL , TEMP_SCORE_ARRAY_[2]
    MOV HS_LIST2[2] , DL
    
    
    MOV AX ,HIGH_SCORE_ARRAY[6] 
    CALL TEMP_SCORE_ARRAY
    LEA SI , NAMEFLD
    MOV DL , TEMP_SCORE_ARRAY_[0]
    MOV [SI + 6] , DL
    MOV DL , TEMP_SCORE_ARRAY_[1]
    MOV [SI + 7] , DL 
    MOV DL , TEMP_SCORE_ARRAY_[2]
    MOV [SI + 8] , DL
     MOV DL , TEMP_SCORE_ARRAY_[0]
    MOV HS_LIST3[0] , DL
     MOV DL , TEMP_SCORE_ARRAY_[1]
    MOV HS_LIST3[1] , DL
     MOV DL , TEMP_SCORE_ARRAY_[2]
    MOV HS_LIST3[2] , DL
    
    MOV [SI + 9] , 0

     ;TEXT_MACRO  0 , 5 , 15 , HS_LIST1
     ;TEXT_MACRO  0 , 9 , 15 , HS_LIST2
     ;TEXT_MACRO  0 , 13 , 15 , HS_LIST3
    MOV BX,HANDLE1
    CALL CLOSE
    
    
   ; TEXT_MACRO  0 , 5 , 15 , HS_LIST1
    ; TEXT_MACRO  0 , 9 , 15 , HS_LIST2
     ;TEXT_MACRO  0 , 13 , 15 , HS_LIST3
     
     
     POP DX
    POP CX
    POP BX
    POP AX
    RET
     
    HIGH_SCORE_UPDATE ENDP  







 

WRITE_PROC PROC
    PUSH AX
  PUSH BX
    PUSH CX
    PUSH DX
    

 ;WRITING FILE
    LEA DX, WRITE_FILE 
    CALL OPEN_WRITE_FILE  
    JC OPEN_ERROR1
    MOV HANDLE,AX 
    
    
    MOV BX, HANDLE
    CALL MOV_PTER 
    
    LEA DI,NAMEFLD
    MOV BX , 0
   
    PRINT_NAMEFLD:
        CMP NAMEFLD[BX] , 0
        JE HERE
        INC BX  
        inc cx
        jmp PRINT_NAMEFLD
        
        
        
    HERE:    

    
    
    MOV BX, HANDLE 
    LEA DX,NAMEFLD 
    CALL WRITE     
    JC WRITE_ERROR
    
    JMP EXIT2
   
    
    OPEN_ERROR1:
    LEA DX, OPENERR
    MOV AH,9
    INT 21H
    JMP EXIT2
    
    WRITE_ERROR:
    LEA DX, WRITERR
    MOV AH,9
    INT 21H
    
    
    EXIT2:
    MOV BX, HANDLE
    CALL CLOSE
    POP DX
    POP CX
    POP BX
    POP AX
    RET
    WRITE_PROC ENDP









SHOW_HIGH_SCORES PROC

    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    MOV AH, 0
    MOV AL, 4h; 320x200 4 color
    INT 10h
; select palette    
    MOV AH, 0BH
    MOV BH, 1
    MOV BL, 1
    INT 10h
; set bgd color
    MOV BH, 0
    MOV BL, 4       ; cyan
    INT 10h
    
    RECTANGLE 0, 0 , 500 , 200 , 4
    
    ;CALL HIGH_SCORE_UPDATE
    TEXT_MACRO  0 , 2 , 15 , HIGH_SCORES_TEXT
    TEXT_MACRO  0 , 5 , 18 , HS_LIST1 
    TEXT_MACRO  0 , 8 , 18 , HS_LIST2
    TEXT_MACRO  0 , 11 , 18 , HS_LIST3
    

    
    POP DX
    POP CX
    POP BX
    POP AX
    RET

SHOW_HIGH_SCORES ENDP



;;_______________----------------------------------------------
    
TEMP_SCORE_ARRAY PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
       
    OR AX , AX
    JGE END_IF1
    PUSH AX
    MOV DL , '-'   
    MOV AH , 2
    INT 21H
    POP AX
    NEG AX
END_IF1:
    XOR CX , CX
    MOV BX , 10
REPEAT1:
    XOR DX , DX
    DIV BX
    PUSH DX
    INC CX
    
    OR AX , AX
    JNE REPEAT1
    
    MOV AH , 2
    

MOV BX , 3
SUB BX , CX 
PRINT_LOOP1:
    POP DX 
    OR DL , 30H
    MOV TEMP_SCORE_ARRAY_[BX] , DL
    INC BX
    LOOP PRINT_LOOP1
    POP DX
     POP CX
      POP BX
       POP AX 
      ; POP DI
      
      
    ;TEXT_MACRO 0 , 1 , 35 , SCORE_ARRAY
       RET
TEMP_SCORE_ARRAY ENDP

    
    
    
    
    
OUTDEC PROC  NEAR
   ; PUSH DI
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
   
    OR AX , AX
    JGE END_IF2
    PUSH AX
    MOV DL , '-'   
    MOV AH , 2
    INT 21H
    POP AX
    NEG AX
END_IF2:
    XOR CX , CX
    MOV BX , 10
REPEAT:
    XOR DX , DX
    DIV BX
    PUSH DX
    INC CX
    
    OR AX , AX
    JNE REPEAT
    
    MOV AH , 2
    

MOV BX , 4
SUB BX , CX 
PRINT_LOOP:
    POP DX 
    OR DL , 30H
   ; MOV SCORE_ARRAY[BX] , DL
    INT 21H
    INC BX
    LOOP PRINT_LOOP
    POP DX
     POP CX
      POP BX
       POP AX 
      ; POP DI
       RET
    OUTDEC ENDP
    
    MAX PROC
    PUSH CX
    PUSH DX 
    
    CMP AX , BX
    JGE END_
    XCHG AX , BX
    
    
    END_:
    
    POP DX
    POP CX 
    RET
    MAX ENDP    
    
    
    
    
    


OPEN PROC NEAR
  
    
    MOV AH,3DH
    MOV AL,0
    INT 21H
    RET 
    OPEN ENDP


OPEN_WRITE_FILE PROC NEAR
    
    MOV AH,3DH
    MOV AL,1
    INT 21H
    RET
    OPEN_WRITE_FILE ENDP

READ PROC NEAR
    
    PUSH CX
    MOV AH,3FH
    MOV CX,512
    INT 21H
    POP CX
    RET
    READ ENDP

WRITE PROC NEAR
 
    MOV AH,40H
    INT 21H
    RET
    WRITE ENDP



 

CLOSE PROC NEAR
    MOV AH,3EH
    INT 21H
    RET
    CLOSE ENDP


MOV_PTER PROC NEAR
    MOV AH,42H
    XOR CX,CX
    xor dx  ,dx
    MOV AL,0
    INT 21H
    RET 
    MOV_PTER ENDP



;;;; PROCEDURES


KEYBOARD_INT    PROC
;keyboard interrupt routine
;save registers
    PUSH    DS
    PUSH    AX
;set up DS
    MOV AX,SEG SCAN_CODE
    MOV DS,AX
;input scan code
    IN  AL,60H      ;read scan code
    PUSH    AX      ;save it
    IN  AL,61H      ;control port
    MOV AH,AL       ;save in AH
    OR  AL,80H      ;set bit for keyboard
    OUT 61H,AL      ;write back
    XCHG    AH,AL       ;get back control value
    OUT 61H,AL      ;reset control port
    POP AX      ;recover scan code
    MOV AH,AL       ;save scan code in AH
    TEST    AL,80H      ;test for break code
    JNE KEY_0       ;yes, clear flags, goto KEY_0
;make code
    MOV SCAN_CODE,AL    ;save in variable
    MOV KEY_FLAG,1  ;set key flag
KEY_0:  MOV AL,20H      ;reset interrupt
    OUT 20H,AL
;restore registers
    POP AX
    POP DS
    IRET
KEYBOARD_INT    ENDP        ;end KEYBOARD routine




;sound generating procedure
BEEP    PROC
;generate beeping sound
    PUSH    CX      ;save CX
    PUSH    AX
;initialize timer
    MOV AL,0B6H     ;specify mode of operation
    OUT 43H,AL      ;write to port 43h
;load count
    MOV AX,1193     ;count for 1000 Hz
    OUT 42H,AL      ;low byte
    MOV AL,AH       ;high byte
    OUT 42H,AL
;activate speaker
    IN  AL,61H      ;read control port
    MOV AH,AL       ;save value in AH
    OR  AL,11B      ;set control bits
    OUT 61H,AL      ;activate speaker
;500 ms delay loop
    MOV CX,9        ;do 9 times
B_1:    CMP TIMER_FLAG,1    ;check timer flag
    JNE B_1     ;not set, loop back
    MOV TIMER_FLAG,0    ;flag set, clear it
    LOOP    B_1     ;repeat for next tick
;turn off tone
    MOV AL,AH       ;return old control value to AL
    OUT 61H,AL      ;restore control value
;
    POP AX
    POP CX      ;restore CX
    RET
BEEP    ENDP


timer_tick Proc
    PUSH DS
    PUSH AX
    
    MOV AX, Seg timer_flag
    MOV DS, AX
    MOV timer_flag, 1
    
    
    POP AX
    POP DS
    
    IRET
timer_tick EndP

setup_int Proc
; save old vector and set up new vector
; input: al = interrupt number
;    di = address of buffer for old vector
;    si = address of buffer containing new vector
; save old interrupt vector
    PUSH AX
    
    MOV AH, 35h ; get vector
    INT 21h
    MOV [DI], BX    ; save offset
    MOV [DI+2], ES  ; save segment
; setup new vector
    MOV DX, [SI]    ; dx has offset
    PUSH DS     ; save ds
    MOV DS, [SI+2]  ; ds has the segment number
    MOV AH, 25h ; set vector
    INT 21h
    POP DS
    
    POP AX
    RET
setup_int EndP

set_display_mode Proc
; sets display mode and draws boundary
    PUSH AX
    PUSH BX
    
    MOV AH, 0
    MOV AL, 4h; 320x200 4 color
    INT 10h
; select palette    
    MOV AH, 0BH
    MOV BH, 1
    MOV BL, 1
    INT 10h
; set bgd color
    MOV BH, 0
    MOV BL, 0; cyan
    INT 10h
    
    POP BX
    POP AX
    
    RET
set_display_mode ENDP



;FUNC FOR DRAWING A BOX
DRAW_BOX PROC
    
    PUSH BX
    
    draw_row r,c,s
    MOV BX,R
    ADD BX,S
    MOV R,BX
    draw_row R,c,s
    MOV BX,R
    SUB BX,S
    MOV R,BX
    draw_col c,r,s
    MOV BX,C
    ADD BX,S
    MOV C,BX
    draw_col C,r,s
    
    POP BX
    
    RET
DRAW_BOX ENDP


DRAW_SCORE PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    MOV AX , CURRENT_SCORE   
    OR AX , AX
    JGE END_IFD
    PUSH AX
    MOV DL , '-'   
    MOV AH , 2
    INT 21H
    POP AX
    NEG AX
    END_IFD:
    XOR CX , CX
    MOV BX , 10
    REPEATx:
    XOR DX , DX
    DIV BX
    PUSH DX
    INC CX
    
    OR AX , AX
    JNE REPEATx
    
    MOV AH , 2
    

    MOV BX , 4
SUB BX , CX 
PRINT_LOOPD:
    POP DX 
    OR DL , 30H
    MOV SCORE_ARRAY[BX] , DL
    INC BX
    LOOP PRINT_LOOPD
    POP DX
     POP CX
      POP BX
       POP AX 
      ; POP DI
      
      
    TEXT_MACRO 0 , 1 , 35 , SCORE_ARRAY
       RET
DRAW_SCORE ENDP



DRAW_B PROC

; draw boundary
    draw_row 20,10,300
    draw_row 190,10,300
    draw_col 10,20,170
    draw_col 310,20,170
    
    RET
DRAW_B ENDP 



DRAW_SNAKE PROC    
;DRAWING SNAKE
    PUSH BX
    PUSH DX

    MOV BX,0
    SUB BX,2
    
    MOV VAR2,0
    LOOOP:    
    
    ADD BX,2
    MOV DX,[SNAKE+BX]
    MOV R,DX    
    
    ADD BX,2
    MOV DX,[SNAKE+BX]
    MOV C,DX
    
    MOV DX,SNAKE_BOXSIZE
    MOV S,DX
    
    CALL DRAW_BOX
    
    INC VAR2
    MOV DX,VAR2
    CMP SNAKE_L,DX
    JNE LOOOP 
    
    POP DX
    POP BX
    RET
    
DRAW_SNAKE ENDP



UPDATE_SNAKE PROC
;MOVE SNAKE
    PUSH AX    

    CMP FLAG,0
    JE RET_MOVE_S

    MOV AL,0
    CALL DRAW_SNAKE

    MOV BX,SNAKE_L
    ADD BX,SNAKE_L
    ADD BX,SNAKE_L
    ADD BX,SNAKE_L
    SUB BX,8
    
    MOV AX,1
    
    ADD BX,10
    LOOOP2:
    SUB BX,10
    
    MOV DX,[SNAKE+BX]
    ADD BX,4
    MOV [SNAKE+BX],DX
    
    SUB BX,2
    
    MOV DX,[SNAKE+BX]
    ADD BX,4
    MOV [SNAKE+BX],DX

    INC AX
    
    CMP AX,SNAKE_L
    JNE LOOOP2
    
    POP AX
    
    CALL UPDATE_SNAKE_H
RET_MOVE_S:    
    RET
UPDATE_SNAKE ENDP

UPDATE_SNAKE_H PROC
    
    PUSH DX    
    
    FLAG_UP:
    CMP FLAG,1
    JNE FLAG_DOWN
    SUB SNAKE_H_ROW,10
    JMP U
    
    FLAG_DOWN:
    CMP FLAG,2
    JNE FLAG_LEFT
    ADD SNAKE_H_ROW,10
    JMP U
    
    FLAG_LEFT:
    CMP FLAG,3
    JNE FLAG_RIGHT
    SUB SNAKE_H_COL,10
    JMP U
    
    FLAG_RIGHT:
    CMP FLAG,4
    JNE U
    ADD SNAKE_H_COL,10

    
    U:
    PUSH BX
        
    MOV DX,SNAKE_H_ROW
    MOV BX,0
    MOV [SNAKE+BX],DX
    
    MOV DX,SNAKE_H_COL
    MOV BX,2
    MOV [SNAKE+BX],DX
    
    POP BX
    
    MOV AL,1
    CALL DRAW_SNAKE
    
    POP DX
    
    RET
UPDATE_SNAKE_H ENDP
    



    
CHECK_BOUNDARY PROC

    CMP SNAKE_H_ROW,20
    JLE YUY
    
    CMP SNAKE_H_ROW,180
    JGE YUY
    
    CMP SNAKE_H_COL,10
    JLE YUY
    
    CMP SNAKE_H_COL,300
    JGE YUY
    
    JMP ZUZ
                
    YUY:
    MOV BOUNDARY_FLAG,1
    
    ZUZ:    
    RET
CHECK_BOUNDARY ENDP 

DRAW_BALL PROC   
    
    PUSH DX
    
    MOV DX,BALL_ROW
    MOV R,DX    
    MOV DX,BALL_COL
    MOV C,DX
    MOV DX,5
    MOV S,DX
    
    CALL DRAW_BOX
    
    POP DX
    RET
DRAW_BALL ENDP  


UPDATE_BALL PROC

;BALL_ROW DW ?
;BALL_COL DW ?
;BALL_ARR DW 3,5,7,9,6,2,5,2
;BALL_ARR_SIZE DW 8
;BALL_ARR_INDEX DW 0  
    PUSH AX
    PUSH BX
    PUSH DX
    
    MOV AL,0
    CALL DRAW_BALL
    
    QPQO:
    
    MOV BALL_BODY_FLAG,0
    
    MOV BX,BALL_ARR_INDEX
    ;ADD BX,BALL_ARR_INDEX
    MOV DX,[BALL_ARR+BX]
    
    AA:    
    CMP DX,18
    JLE BB
    SUB DX,18
    JMP AA
    BB:
    CMP DX,3
    JGE CC
    ADD DX,5
    CC:
    MOV VAR1,10
    MOV AX,DX
    MUL VAR1
    MOV DX,AX
    
    MOV BALL_ROW,DX
    ADD BX,2
    ;INC BALL_ARR_INDEX
    ;MOV BX,BALL_ARR_INDEX
    ;ADD BX,BALL_ARR_INDEX
    MOV DX,[BALL_ARR+BX]
    JMP EE
    
    RTE:
    JMP QPQO
    
    EE:    
    CMP DX,30
    JLE FF
    SUB DX,30
    JMP EE
    FF:
    CMP DX,2
    JGE GG
    ADD DX,5
    GG:
    MOV VAR1,10
    MOV AX,DX
    MUL VAR1
    MOV DX,AX
    
    
    MOV BALL_COL,DX
    ADD BX,2
    
    MOV BALL_ARR_INDEX,BX
    ;INC BALL_ARR_INDEX
    ;MOV BX,BALL_ARR_INDEX
    ;ADD BX,BALL_ARR_INDEX
    CMP BX,BALL_ARR_SIZE
    JL  QLQ_RET
    MOV BALL_ARR_INDEX,0
    
    
    QLQ_RET:    
    CALL CHECK_BALL_BODY
    CMP BALL_BODY_FLAG,1
    JE RTE
    
    ADD BALL_ROW,2
    ADD BALL_COL,2
    
    MOV AL,1
    CALL DRAW_BALL
    
    
    POP DX
    POP BX
    POP AX
    RET
    
UPDATE_BALL ENDP

CHECK_SNAKE_H_BALL PROC

    PUSH BX

    SUB BALL_ROW,2
    SUB BALL_COL,2
    
    MOV BX,SNAKE_H_ROW
    CMP BALL_ROW,BX
    JNE QWE_RET
    
    MOV BX,SNAKE_H_COL
    CMP BALL_COL,BX
    JNE QWE_RET
    
    
    WER:
    MOV SNAKE_H_BALL_FLAG,1
    ;INC SNAKE_L
    
    
    QWE_RET:
    
    ADD BALL_ROW,2
    ADD BALL_COL,2
    
    POP BX
    RET
CHECK_SNAKE_H_BALL ENDP

CHECK_SNAKE_H_BODY PROC
    
    PUSH AX
    PUSH BX
    PUSH DX    
    
    
    ;MOV DX,1
    MOV VAR4,1
    ;JMP CHK_RET
    
    STRT:
    MOV DX,VAR4
    ;JMP CHK_RET
    CMP DX,SNAKE_L
    JE CHK_RET
    
    
    MOV BX,VAR4
    ADD BX,VAR4
    ADD BX,VAR4
    ADD BX,VAR4
    
    MOV DX,SNAKE[BX]
    CMP DX,SNAKE_H_ROW
    JNE BQX_RET
    
    ADD BX,2
    
    MOV DX,SNAKE[BX]
    CMP DX,SNAKE_H_COL
    JE FQLAG
    
    
    BQX_RET:
    ADD BX,2
    INC VAR4
    JMP STRT
    
    FQLAG:
    MOV SNAKE_H_BODY_FLAG,1
    
    
    CHK_RET:
    POP DX
    POP BX
    POP AX    

    RET
CHECK_SNAKE_H_BODY ENDP

CHECK_BALL_BODY PROC
    
    PUSH AX
    PUSH BX
    PUSH DX    
    
    
    ;MOV DX,1
    MOV VAR4,0
    ;JMP CHK_RET
    
    STRT2:
    MOV DX,VAR4
    ;JMP CHK_RET
    CMP DX,SNAKE_L
    JE CHK_RET2
    
    
    MOV BX,VAR4
    ADD BX,VAR4
    ADD BX,VAR4
    ADD BX,VAR4
    
    MOV DX,SNAKE[BX]
    CMP DX,BALL_ROW
    JNE BQX_RET2
    
    ADD BX,2
    
    MOV DX,SNAKE[BX]
    CMP DX,BALL_COL
    JE FQLAG2
    
    
    BQX_RET2:
    ADD BX,2
    INC VAR4
    JMP STRT
    
    FQLAG2:
    MOV BALL_BODY_FLAG,1
    
    
    CHK_RET2:
    POP DX
    POP BX
    POP AX    

    RET
CHECK_BALL_BODY ENDP







INTRO PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX

    ; set CGA 640x200 high res mode
    MOV AH,0
 ;   MOV AL, 4H
    INT 10H

    ; select palette
     MOV AH, 0BH
     MOV BH, 1
     MOV BL, 1
        INT 10h

    ; set bgd color
        MOV BH, 0
        MOV BL, 4
        INT 10h   
    
    RECTANGLE 105 , 50, 110 , 20, 9
    RECTANGLE 105 , 90 , 110 , 20 , 9
    RECTANGLE 105 , 130 , 110 , 20 , 9
    TEXT_MACRO 0 , 7 , 16 , MESSAGE1
    TEXT_MACRO 0 , 12 , 15 , MESSAGE2
    TEXT_MACRO 0 , 17 , 18 , MESSAGE3
    
    
    POP DX
    POP CX
    POP BX
    POP AX
    
    
    RET
INTRO ENDP




NEW_GAME_PROC PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX

        ;INITIALIZING VALUES
        
    MOV SNAKE_L,5    
    
    MOV SNAKE[0],100
    MOV SNAKE[2],110
    MOV SNAKE[4],100
    MOV SNAKE[6],100
    MOV SNAKE[8],100
    MOV SNAKE[10],90
    MOV SNAKE[12],100
    MOV SNAKE[14],80
    MOV SNAKE[16],100
    MOV SNAKE[18],70
    
    
    MOV TIMER_CON,2


    
    MOV BX,0
    MOV DX,[SNAKE+BX]
    MOV SNAKE_H_ROW,DX
    MOV BX,2
    MOV DX,[SNAKE+BX]
    MOV SNAKE_H_COL,DX
    
    ;INITIALIZING VALUES
    MOV BALL_ROW,122
    MOV BALL_COL,272
    
    ;INITIALIZING VALUES
    MOV CURRENT_SCORE,0
    MOV BOUNDARY_FLAG,0
    MOV SNAKE_H_BALL_FLAG,0
    MOV SNAKE_H_BODY_FLAG,0
    MOV BALL_BODY_FLAG,0
    MOV TIMER,0
    MOV FLAG,4
    
    CALL SET_DISPLAY_MODE
    
    MOV AL,1
    CALL DRAW_B 
    CALL DRAW_SCORE
    CALL DRAW_BALL
    CALL DRAW_SNAKE
    
    ;CALL UPDATE_BALL
    
    
    
    ; set up timer interrupt vector
    MOV new_timer_vec, offset timer_tick
    MOV new_timer_vec+2, CS
    MOV AL, 1CH; interrupt type
    LEA DI, old_timer_vec
    LEA SI, new_timer_vec
    CALL setup_int

;set up keyboard interrupt vector
    MOV NEW_KEY_VEC,OFFSET KEYBOARD_INT ;offset
    MOV NEW_KEY_VEC+2,CS        ;segment
    MOV AL,9H       ;interrupt number
    LEA DI,OLD_KEY_VEC
    LEA SI,NEW_KEY_VEC
    CALL    SETUP_INT
    
    JMP TEST_KEY
    
    
TEST_KEY:
    CMP KEY_FLAG,1          ;check key flag
    JNE TEST_TIMER          ;not set, go check timer flag
    MOV KEY_FLAG,0          ;flag set, clear it and check
    
    
    ;CMP SCAN_CODE,'P'
    ;JE TK_7
    
    CMP SCAN_CODE,ESC_KEY
    JE DO2
TK_1:   
    CMP SCAN_CODE,UP_ARROW  ;up arrow?
    JNE TK_2                ;no, check down arrow
    CMP FLAG,2
    JE  TEST_TIMER
    MOV FLAG,1
    JMP TEST_TIMER          ;go check timer flag
TK_2:   
    CMP SCAN_CODE,DOWN_ARROW    ;up arrow?
    JNE TK_3                ;no, check down arrow
    CMP FLAG,1
    JE  TEST_TIMER
    MOV FLAG,2
    JMP TEST_TIMER          ;go check timer flag
TK_3:   
    CMP SCAN_CODE,LEFT_ARROW    ;up arrow?
    JNE TK_4                ;no, check down arrow    
    CMP FLAG,4
    JE  TEST_TIMER
    MOV FLAG,3
    JMP TEST_TIMER
TK_4: 
    CMP SCAN_CODE,RIGHT_ARROW  
    JNE TEST_TIMER
    CMP FLAG,3
    JE  TEST_TIMER
    MOV FLAG,4
    JMP TEST_TIMER
    ;go check timer flag
    
;TK_7:
    ;MOV AH,0
    ; INT 16H
    
    ;CMP AL,'P'
    ;JE TEST_TIMER
    ;JMP TK_7 
    
    DO1:
    JMP TEST_KEY
    DO2:
    JMP DONE2
    
TEST_TIMER:
    CMP TIMER_FLAG,1    ;flag set?
    JNE DO1        ;no, check key flag
    INC TIMER
    MOV TIMER_FLAG,0    ;yes, clear it
    MOV BX,TIMER 
    CMP BX,TIMER_CON
    JNE DO1
    MOV TIMER,0
    
    
    ;THE IMP SEGMENT
        ;MOVING SNAKE
    MOV AL,0
    CALL DRAW_SNAKE
    CALL UPDATE_SNAKE
    MOV AL,1
    CALL DRAW_SNAKE    
        ;CHECKING SNAKE HEAD
    CALL CHECK_BOUNDARY
    CALL CHECK_SNAKE_H_BALL
    CALL CHECK_SNAKE_H_BODY
    ;JMP DONE2
    
    CMP BOUNDARY_FLAG,1
    JE DONE2
    
    CMP SNAKE_H_BODY_FLAG,1
    JE DONE2
    
    CMP SNAKE_H_BALL_FLAG,0
    JE DO1
        
        
        ;SNAKE ATE A BALL,NOW MOVING SNAKE
    MOV AL,0
    CALL DRAW_SNAKE
    
        ;;;;;;;;;;;CALL BEEP
    
    CMP SNAKE_L,SNAKE_L_CONST
    JNL INCSNAKE_L 
    INC SNAKE_L
    
    INCSNAKE_L:
    CALL UPDATE_SNAKE
    MOV AL,1
    CALL DRAW_SNAKE
    
        ;UPDATING BALL,SCORE
        
    MOV SNAKE_H_BALL_FLAG,0
    CALL UPDATE_BALL
    INC CURRENT_SCORE
    
    CMP CURRENT_SCORE,6
    JE SAW
    
    JMP UIYI
    SAW:
    DEC TIMER_CON
    
    UIYI:
    
    CALL DRAW_SCORE
    ;;;;;;;
    JMP DO1
    
DONE2:
    MOV BOUNDARY_FLAG,0
    MOV SNAKE_H_BODY_FLAG,0
    
    
    LEA DI,NEW_TIMER_VEC
    LEA SI,OLD_TIMER_VEC
    MOV AL,1CH
    CALL SETUP_INT

    LEA DI,NEW_KEY_VEC
    LEA SI,OLD_KEY_VEC
    MOV AL,9H
    CALL SETUP_INT


    MOV AH,0
    INT 16H
    
    ;MOV AH,0
    ;MOV AL,3
    ;INT 10H
    
    POP DX
    POP CX
    POP BX
    POP AX
    
    RET

NEW_GAME_PROC ENDP



ARE_YOU_SURE_PROC PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
   
    MOV AH, 0
    MOV AL, 4h; 320x200 4 color
    INT 10h
; select palette    
    MOV AH, 0BH
    MOV BH, 1
    MOV BL, 1
    INT 10h
; set bgd color
    MOV BH, 0
    MOV BL, 1; cyan
    INT 10h
    
    RECTANGLE 0, 0 , 500 , 200 , 9
    
    
    RECTANGLE 65 , 35 , 200 , 20 , 4
    RECTANGLE 105 , 90 , 110 , 20 , 4
    RECTANGLE 105 , 130 , 110 , 20 , 4
    TEXT_MACRO 0 , 5 , 14 , SURE_MSG
    TEXT_MACRO 0 , 12 , 18 , YES_MSG
    TEXT_MACRO 0 , 17 , 18 , NO_MSG 
    
    LKJ:
    MOV AH , 0
    INT 16H
    
    CMP AL , 'Y'
    JE URT
    
    CMP AL , 'N'
    JE URT
   
    
    JMP LKJ
    
    URT:
    MOV ARE_YOU_SURE_FLAG,AL
    
    POP DX
    POP CX
    POP BX
    POP AX
    RET


ARE_YOU_SURE_PROC ENDP




;;;;;;;;;;; YOUR SCORE PROCEDURE HERE;;;;;;;;;;;;;;;;;;

YOUR_SCORE_PROC PROC

    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
   
    
    MOV AH, 0
    MOV AL, 4h; 320x200 4 color
    INT 10h
; select palette    
    MOV AH, 0BH
    MOV BH, 1
    MOV BL, 1
    INT 10h
; set bgd color
    MOV BH, 0
    MOV BL, 4       ; cyan
    INT 10h
    
    RECTANGLE 0, 0 , 500 , 200 , 4
   
    RECTANGLE 65 , 35 , 200 , 40 , 4
    TEXT_MACRO 0 , 5 , 15 , YOUR_SCORE_MSG
    TEXT_MACRO 0 , 8 , 18 , SCORE_ARRAY
    
    MOV AH , 0
    INT 16H
    
    POP DX
    POP CX
    POP BX
    POP AX
    RET

YOUR_SCORE_PROC ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;  HIGH SCORE SHOW PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









;;;;;GAME END PROCEDURE HERE \;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GAME_END_PROC PROC

    PUSH AX 
    PUSH BX 
    
    
    
    ; select palette
    MOV AH, 0BH
    MOV BH, 1
    MOV BL, 2
    INT 10h

; set bgd color
    MOV BH, 1
    MOV BL, 0
    INT 10h
    RECTANGLE 0, 0 , 500 , 200 , 0
    
    POP BX 
    POP AX
    RET
    GAME_END_PROC ENDP


    


    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    
    
    ;____________________________
    ;MAIN PROCEDURE HERE
    
    
    

MAIN PROC

    ;MAIN FUNCTION
    MOV AX, @data
    MOV DS, AX
    MOV ES , AX
    
    
    
    
    
INTRODUCTION:
    MOV AL , 4H
    ;CALL SHOW_HIGH_SCORES
   
    CALL INTRO
    
    
ITYE:
    ;call YOUR_SCORE_PROC
    
    
    ;call HIGH_SCORE_UPDATE
    ;CALL WRITE_PROC
      
    MOV AH , 0
    INT 16H
    
    CMP AL,'H'
    JE HIGH_SCORE    
    
    CMP AL , 'N'
    JE NEW_GAME
    JMP ITYE
    

NEW_GAME:


        
    CALL NEW_GAME_PROC
    
ARE_YOU_SURE: 
    CALL ARE_YOU_SURE_PROC
    JMP YOUR_SCORE
    
    
    
    
YOUR_SCORE:
    CALL YOUR_SCORE_PROC
    CALL HIGH_SCORE_UPDATE
    CALL WRITE_PROC
    
    XOR BX,BX
    MOV BL,ARE_YOU_SURE_FLAG
    
    CMP BL,'Y'
    JE GAME_END

    
    CMP BL,'N'
    JE INTRODUCTION

HIGH_SCORE:
    CALL HIGH_SCORE_UPDATE
    CALL SHOW_HIGH_SCORES
    

OIOQ:    
    MOV AH , 0
    INT 16H
    
    ;CMP AL,ESC_KEY
    ;JE INTRODUCTION
    JMP INTRODUCTION
        
    ;JMP OIOQ 
    
    
GAME_END:
    
    ;CALL GAME_END_PROC
    
    

    MOV AH, 4CH
    INT 21h
main EndP



     End main
