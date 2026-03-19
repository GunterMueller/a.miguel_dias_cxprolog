class JavaCallCxProlog {
	static {
		try {
			System.loadLibrary("JavaCallCxProlog");
		} catch (UnsatisfiedLinkError e) {
			System.err.println("Native code library failed to load.\n" + e);
			System.exit(1);
    	}
	}

	public static native void startProlog();
	public static native void callProlog(String str);
	public static native void stopProlog();

	public static void main(String[] args) {
		startProlog();
		callProlog("writeln('ola ole')");
		callProlog("statistics");
		stopProlog();
	}
}
