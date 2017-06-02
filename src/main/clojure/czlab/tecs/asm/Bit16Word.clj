package com.tecs.asm;

/*
 * Abstraction for a 16-bit word.
 */
class Bit16Word {

	private def _word= new int[16];
	
	/**/
	def setBit(int pos, int value) {
		if (pos < 0 || pos > 15) { throw new Exception("Index out of range for word: " + pos); }
		if ( !( value==0 || value==1)) { throw new Exception("Bad binary value: " + value); }
		_word[pos]=value;
	}
	
	/**/
	def getBit(int pos) {
		if (pos < 0 || pos > 15) { throw new Exception("Index out of range for word: " + pos); }
		return _word[pos];
	}
	
	/**/
	def setBits(Map m) {
		def me=this; m.each { k, v ->
			me.setBit(k,v);
		}
	}
	
	/**/
	def or(Bit16Word w) {
		Bit16Word r= new Bit16Word();
		for (int i=0; i < 16; ++i) {
			if (_word[i] ==1 || w._word[i]==1) {
				r.setBit(i, 1);
			}
		}
		return r;
	}
	
	/**/
	public String toString() {
		// need to reverse it - MSB to appear first
		def b= new StringBuilder();
		for (int i=15; i >=0; --i) {
			b.append(_word[i]);
		}
		return b.toString();
	}
	
	/**/
	def Bit16Word() {
	}
	
}
