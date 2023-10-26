public class Test {
	public boolean z = true ;
	public byte b = -10 ;
	public char c = 'a' ;
	public short s = 123 ;
	public int i = 2004 ;
	public long j = 1999999999 ;
	public float f = (float)123.456 ;
	public double d = 123.456 ;
	public double[] ad = {123.456, 34, 55.55} ;
	public String[] as = {"ola", "ole"} ;
	public Object ob = null ;
	public String str = "olaole" ;

	public void ola(String arg) {
        System.out.println("OLA " + arg) ;
	}
	public void ola(int i) {
		int a = 0 ;
        System.out.println("OLA " + i/a) ;
	}


	public boolean toBool(int i) {
		return i != 0 ? true : false ;
	}
	public byte toByte(int i) {
		return (byte)i ;
	}
	public char toChar(int i) {
		return (char)i ;
	}
	public short toShort(int i) {
		return (short)i ;
	}
	public int toInt(int i) {
		return i ;
	}
	public long toLong(int i) {
		return i ;
	}
	public float toFloat(int i) {
		return i ;
	}
	public double toDouble(int i) {
		return i ;
	}
	public String toString(int i) {
		return (new Integer(i)).toString() ;
	}

	public String doubleString(String s) {
		return s + s ;
	}
	public String arrString(String[] s, int i) {
		return s[i] ;
	}
	public String arrString2(String[][] s, int i, int j) {
		return s[i][j] ;
	}
	public int arrInt(int[][][] s, int i, int j, int k) {
		return s[i][j][k] ;
	}

	public Class intClass() {
		return int.class ;
	}


	public void over() {
        System.out.println(b) ;
		b *= 100 ;
        System.out.println(b) ;		
	}
    public static void main(String[] args) {
        System.out.println("Hello World " + args[0]) ;
    }
}
