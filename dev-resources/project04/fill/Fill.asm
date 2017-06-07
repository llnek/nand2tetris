(MAIN)
@color
M=0
@size
M=0

// test keyed or not ?
@24576
D=M
@WHITE
D;JEQ	// if no key => white
@color
M=1		// keyed => black
@DRAW
0;JMP


(WHITE)
@color
M=0
@DRAW
0;JMP

(DRAW)
@SCREEN
D=A
@SCN
M=D
//
@10
D=A
@count
M=D
//
(LOOP)
@color
D=M
@SCN
A=M
M=D
// down count
@count
M=M-1
D=M
@DONE_LOOP
D;JLT
//  incr screen ptr
@SCN
D=M
M=D+1
@LOOP
0;JMP

(DONE_LOOP)
@MAIN
0;JMP

