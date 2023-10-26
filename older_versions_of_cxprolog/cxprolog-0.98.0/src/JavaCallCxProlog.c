#include <stdio.h>
#include "CxProlog.h"
#include <jni.h>

JNIEXPORT void JNICALL Java_JavaCallCxProlog_startProlog(JNIEnv *env, jclass class) {
	StartProlog(0, nil, nil);
}

JNIEXPORT void JNICALL Java_JavaCallCxProlog_callProlog(JNIEnv *env, jclass class, jstring str) {
	Str s = cStr((*env)->GetStringUTFChars(env, str, nil));
    CallPrologStr(s) ;
}

JNIEXPORT void JNICALL Java_JavaCallCxProlog_stopProlog(JNIEnv *env, jclass class) {
	StopProlog() ;
}
