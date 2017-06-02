package com.tecs.asm;

/*
 * Given the TECS destination code, return the code in its binary form based on definitions from the book.
 */
class DestOut {

	/**/
	def convertToBits(String code) {
		def w= new Bit16Word();
		code=( code ?: "" ).trim().toUpperCase();
		// set bits 5,4,3
		if (code.indexOf("A") >=0) {
			w.setBit(5,1);
		}
		if (code.indexOf("M") >=0) {
			w.setBit(3,1);
		}
		if (code.indexOf("D") >=0) {
			w.setBit(4,1);
		}
		return w;
	}

	/**/
	def DestOut()
	{}
	
}
