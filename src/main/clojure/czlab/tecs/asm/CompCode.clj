package com.tecs.asm;

/*
 * Given the TECS compute-instruction code, return the code in its binary form based on definitions from the book.
 */
class CompCode {

	/**/
	def convertToBits(String code) {
		def flag=0, w= new Bit16Word();
		code = ( code ?: "" ).trim().toUpperCase();
		if (code.indexOf("M") >=0) {
			// if the actual register affected is M, then we flag it, but
			// fake it into A register for comparison below
			flag=1;
			code=code.replace("M", "A");
		}
		// bits 11,10,9,8,7,6
		switch (code) {
			case "0": w.setBits( [ 11:1, 10:0, 9:1, 8:0, 7:1, 6:0 ]); break;
			case "1": w.setBits( [ 11:1, 10:1, 9:1, 8:1, 7:1, 6:1 ]); break;
			case "-1": w.setBits( [ 11:1, 10:1, 9:1, 8:0, 7:1, 6:0 ]); break;
			case "D": w.setBits( [ 11:0, 10:0, 9:1, 8:1, 7:0, 6:0 ]); break;
			case "A": w.setBits( [ 11:1, 10:1, 9:0, 8:0, 7:0, 6:0 ]); break;
			case "!D": w.setBits( [ 11:0, 10:0, 9:1, 8:1, 7:0, 6:1 ]); break;
			case "!A": w.setBits( [ 11:1, 10:1, 9:0, 8:0, 7:0, 6:1 ]); break;
			case "-D": w.setBits( [ 11:0, 10:0, 9:1, 8:1, 7:1, 6:1 ]); break;
			case "-A": w.setBits( [ 11:1, 10:1, 9:0, 8:0, 7:1, 6:1 ]); break;
			case "D+1": w.setBits( [ 11:0, 10:1, 9:1, 8:1, 7:1, 6:1 ]); break;
			case "A+1": w.setBits( [ 11:1, 10:1, 9:0, 8:1, 7:1, 6:1 ]); break;
			case "D-1": w.setBits( [ 11:0, 10:0, 9:1, 8:1, 7:1, 6:0 ]); break;
			case "A-1": w.setBits( [ 11:1, 10:1, 9:0, 8:0, 7:1, 6:0 ]); break;
			case "D+A": w.setBits( [ 11:0, 10:0, 9:0, 8:0, 7:1, 6:0 ]); break;
			case "D-A": w.setBits( [ 11:0, 10:1, 9:0, 8:0, 7:1, 6:1 ]); break;
			case "A-D": w.setBits( [ 11:0, 10:0, 9:0, 8:1, 7:1, 6:1 ]); break;
			case "D&A": w.setBits( [ 11:0, 10:0, 9:0, 8:0, 7:0, 6:0 ]); break;
			case "D|A":	 w.setBits( [ 11:0, 10:1, 9:0, 8:1, 7:0, 6:1 ]); break; 		
		}
		
		// see if we need to toggle bit-12 (for M-register)
		if (flag > 0) {
			// it was M not A, so make sure a flag that!
			w.setBit(12,1);
		}
		
		return w;
	}
	
	/**/
	def CompCode()
	{}
	

}
