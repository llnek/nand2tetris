package com.tecs.asm;

/*
 * Given the TECS jump code, return the code in its binary form based on definitions from the book.
 */
class JumpCode {

  /**/
  def convertToBits(String code) {
    def w= new Bit16Word();
    code=( code  ?: "" ).trim().toUpperCase();
    // bit= 2, 1, 0
    switch (code) {
      case "JGT": w.setBits( [ 0:1, 1:0, 2:0 ] ); break;
      case "JEQ": w.setBits( [ 0:0, 1:1, 2:0 ] ); break;
      case "JGE": w.setBits( [ 0:1, 1:1, 2:0 ] ); break;
      case "JLT": w.setBits( [ 0:0, 1:0, 2:1 ] ); break;
      case "JNE": w.setBits( [ 0:1, 1:0, 2:1 ] ); break;
      case "JLE": w.setBits( [ 0:0, 1:1, 2:1 ] ); break;
      case "JMP": w.setBits( [ 0:1, 1:1, 2:1 ] ); break;
    }
    return w;
  }

  /**/
  def JumpCode()
  {}


}
