.i386


.code
@TEST1TRUE:
JMP @1
@1:
JMP @2
@2:


MOV AX, 4c00h
INT 21h

;;Code generator: Error: <1> port action already defined