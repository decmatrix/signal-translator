.i386


.data
VAR_A DW 0h


.code
@TEST1TRUE:
@1:
JMP @2
@2:
XOR AX, AX
XOR VAR_A, VAR_A
IN AX, 1h
MOV VAR_A, AX


MOV AX, 4c00h
INT 21h

;;OK. No errors