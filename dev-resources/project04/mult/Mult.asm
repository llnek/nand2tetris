@total
M=0		// set total=0
@R2	// M= R2
M=0	// R2=0
@R0
D=M
@END
D;JEQ	// end if R0 = zero
@R1
D=M
@END
D;JEQ	// end if R1 = zero
@counter
M=D	// counter = R1

(LOOP)
@counter
M=M-1
D=M
@END
D;JLT	// if counter < 0, end it
@R0
D=M		// set D= R0
@total	
M=D+M	// total= total + R0
@LOOP
0;JMP	// goto LOOP

(END)
@total
D=M
@R2
M=D+M
@FINZ
0;JMP

(FINZ)
@FINZ
0;JMP


