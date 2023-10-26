/*
 *   This file is part of the CxProlog system

 *   Java.c
 *   by A.Miguel Dias - 2004/07/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

 *   it under the terms of the GNU General Public License as published by
 *   CxProlog is free software; you can redistribute it and/or modify
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"

#if USE_JAVA
#include "jni.h"

/* JOBJ */

static ExtraTypePt jobjType ;

typedef struct JObj {
	ExtraDef(JObj) ;
	jobject value  ;
} JObj, *JObjPt ;

#define cJObjPt(x)				((JObjPt)(x))

#define JObjValue(j)			(cJObjPt(j)->value)
#define MakeJObj(obj)			((obj)==nil ? tNullAtom : TagExtra(JObjNew(obj)))

static Pt tNullAtom, tNullJObj ;
static JObjPt nullJObj ;

static void CreateNullStuff(void)
{
	tNullAtom = MakeAtom("null") ;
	nullJObj = ExtraNew(jobjType) ;
	JObjValue(nullJObj) = nil ;
	tNullJObj = TagExtra(nullJObj) ;	
}

static JObjPt JObjNew(jobject obj)
{
	JObjPt j ;
	if( obj == nil ) return nullJObj ;
	j = ExtraNew(jobjType) ;
	JObjValue(j) = obj ;
	return j ;
}

#if unused
static JObjPt XTestJObj(register Pt t)
{
	VarValue(t) ;
	if( t == tNullAtom ) return nullJObj ;
	if( IsThisExtra(jobjType, t) )
		return cJObjPt(XExtra(t)) ;
	return TypeError2("JOBJ", t) ;
}
#endif

static jobject XTestJObjValue(register Pt t)
{
	VarValue(t) ;
	if( t == tNullAtom ) return nil ;
	if( IsThisExtra(jobjType, t) )
		return JObjValue(XExtra(t)) ;
	return TypeError2("JOBJ", t) ;
}

static CharPt JObjNamingFun(VoidPt x)
{
	return x == nullJObj ? "null object" : nil ;
}


/* Global variables */

static JNIEnv *env ;
static JavaVM *jvm ;
static jclass classClass, systemClass, stringClass ;
static jclass booleanClass, byteClass, charClass, shortClass ;
static jclass intClass, longClass, floatClass, doubleClass, voidClass ;
static jclass constructorClass, methodClass, fieldClass ;
static jmethodID classClass_getName, classClass_isArray ;
static jmethodID classClass_isPrimitive, classClass_getComponentType  ;
static jmethodID classClass_getConstructors, classClass_getMethods ;
static jmethodID classClass_getFields  ;
static CharPt ns, nskind ; /* current call signature and its kind, for error messages */


/* JNI macros */

#define JIsInstanceOf(o,c)				(*env)->IsInstanceOf(env, o, c)
#define JFindClass(n)					(*env)->FindClass(env, n)
#define JGetObjectClass(o)				(*env)->GetObjectClass(env, o)
#define JIsSameObject(o1,o2)			(*env)->IsSameObject(env, o1, o2)
#define JNewObjectA(c,id,args)			(*env)->NewObjectA(env, c, id, args)

#define JExceptionOccurred()			(*env)->ExceptionOccurred(env)
#define JExceptionClear()				(*env)->ExceptionClear(env)
#define JExceptionDescribe()			(*env)->ExceptionDescribe(env)


#define JGetMethodID(c,n,s)				(*env)->GetMethodID(env, c, n, s)
#define JGetStaticMethodID(c,n,s)		(*env)->GetStaticMethodID(env, c, n, s)
#define JCallMethod(k,c,id)				(*env)->Call##k##Method(env, c, id)
#define JCallMethodA(k,w,o,id,args) 	((w)?(*env)->CallStatic##k##MethodA(env, o, id, args) \
											:(*env)->Call##k##MethodA(env, o, id, args))

#define JGetFieldID(c,n,s)				(*env)->GetFieldID(env, c, n, s)
#define JGetStaticFieldID(c,n,s)		(*env)->GetStaticFieldID(env, c, n, s)
#define JGetField(k,w,o,id) 			((w)?(*env)->GetStatic##k##Field(env, o, id) \
											:(*env)->Get##k##Field(env, o, id))
#define JSetField(k,w,o,id,args) 		((w)?(*env)->SetStatic##k##Field(env, o, id, args) \
											:(*env)->Set##k##Field(env, o, id, args))

#define JGetStringUTFChars(o)			(*env)->GetStringUTFChars(env, o, nil)
#define JReleaseStringUTFChars(o,str)	(*env)->ReleaseStringUTFChars(env, o, str)
#define JNewStringUTF(n)				(*env)->NewStringUTF(env, n)

#define JNewArray(k,size)				(*env)->New##k##Array(env, size)
#define JNewObjectArray(size,c)			(*env)->NewObjectArray(env, size, c, nil)
#define JArrayGet(k,a,i,b)				(*env)->Get##k##ArrayRegion(env, a, i, 1, cVoidPt(b))
#define JObjectArrayGet(a,i,b)			((*b) = (*env)->GetObjectArrayElement(env, a, i))
#define JArraySet(k,a,i,vo,vn)			if( vo == nil || *(vn) != *(vo) ) \
											(*env)->Set##k##ArrayRegion(env, a, i, 1, cVoidPt(vn))
#define JObjectArraySet(a,i,vo,vn)		if( vo == nil || !JIsSameObject(*(vn), *(vo)) ) \
											(*env)->SetObjectArrayElement(env, a, i, *(vn))
#define JGetArrayLength(a)				(*env)->GetArrayLength(env, a)


/* General functions */

static VoidPt JError(CharPt fmt, ...) ; /* forward */

static jclass FindNormalClassChecked(CharPt name)
{
	jclass cls ;
	if( (cls = JFindClass(name)) != nil )
		return cls ;
	return JError("Could not access java class '%s'", name) ;
}

static jclass FindPrimitiveClassChecked(CharPt name)
{
	jclass cls ;
	if( (cls = JFindClass(name)) != nil ) {
		jfieldID id = JGetStaticFieldID(cls, "TYPE", "Ljava/lang/Class;") ;
		if( id != nil && (cls = JGetField(Object, true, cls, id)) != nil )
			return cls ;
	}
	return JError("Could not access primitive java class '%s'", name) ;
}

static jmethodID GetMethodIDChecked(jclass cls, CharPt name, CharPt sig)
{
	jmethodID id ;
	if( (id = JGetMethodID(cls, name, sig)) == nil )
		JError("Could not obtain method %s:%s", name, sig) ;
	return id ;
}

static CharPt GetJavaStringText(jstring obj) /* pre: obj != nil */
{
	if( JIsInstanceOf(obj, stringClass) ) {
		CharPt jstr, str ;
		if( (jstr = cCharPt(JGetStringUTFChars(obj))) == nil )
			JError("Couldn't access the contents of a java string") ;
		str = GStrMake(jstr) ;
		JReleaseStringUTFChars(obj, jstr) ;
		return str ;
	}
	else return nil ;
}

static jclass FindClass(CharPt name)
{
	jclass cls ;
	if( (cls = JFindClass(name)) != nil )
		return cls ;
	JExceptionClear() ;
	if( name[1] == '\0' ) { /* handle primitive types */
		switch( *name ) {
			case 'Z': return booleanClass ;
			case 'B': return byteClass ;
			case 'C': return charClass ;
			case 'S': return shortClass ;
			case 'I': return intClass ;
			case 'J': return longClass ;
			case 'F': return floatClass ;
			case 'D': return doubleClass ;
			case 'V': return voidClass ;
			default:  /* nothing */ break ;
		}
	}
	return JError("Could not access java class '%s'", name) ;
}

static CharPt ClassName(jclass cls) /* pre: cls != nil */
{
	if( JCallMethod(Boolean, cls, classClass_isPrimitive) ) {
		if( JIsSameObject(cls, intClass) ) return "I" ;		
		if( JIsSameObject(cls, doubleClass) ) return "D" ;		
		if( JIsSameObject(cls, booleanClass) ) return "Z" ;		
		if( JIsSameObject(cls, charClass) ) return "C" ;		
		if( JIsSameObject(cls, floatClass) ) return "F" ;		
		if( JIsSameObject(cls, voidClass) ) return "V" ;		
		if( JIsSameObject(cls, longClass) ) return "J" ;		
		if( JIsSameObject(cls, shortClass) ) return "S" ;		
		if( JIsSameObject(cls, byteClass) ) return "B" ;		
		return InternalError("ClassName (1)") ;
	}
	else {
		jobject obj ;
		CharPt name ;
		if( (obj = JCallMethod(Object, cls, classClass_getName)) == nil )
			JError("Could not find class name (2)") ;
		if( (name = GetJavaStringText(obj)) == nil )
			JError("Could not find class name (3)") ;
		return name ;
	}
}

static void CheckIfInstanceOf(jobject obj, jclass cls) /* pre: cls != nil */
{
	if( !JIsInstanceOf(obj, cls) ) {
		jclass actualClass = JGetObjectClass(obj) ;
		JError("Incompatible object of class '%s'",
											ClassName(actualClass)) ;
	}
}


/* Errors & exceptions */

static void PrepareJErrors(CharPt s, CharPt k)
{
	ns = s ;
	nskind = k ;
}

static VoidPt JError(CharPt fmt, ...)
{
	CharPt fmt2 ;
	va_list p ;	
	JExceptionClear() ;
	fmt2 = GStrFormat("<%s '%s'> %s", nskind, ns, fmt) ;
	va_start(p, fmt) ;
	return ErrorV(fmt2, p) ;
}

static VoidPt JErrorSig(void)
{
	return JError("Invalid %s signature", nskind) ;
}

static void CheckException(void)
{
	jthrowable exc ;
	CharPt s ;
	if( (exc = JExceptionOccurred()) ) {
		jclass cls ;
		Str256 excName ;
		jmethodID id ;
		jobject obj ;
		JExceptionClear() ;
		cls = JGetObjectClass(exc) ;
		strcpy(excName, ClassName(cls)) ;
		id = JGetMethodID(cls, "getMessage", "()Ljava/lang/String;") ;
		if( id == nil ) JError("Could not find 'Throwable.getMessage' method") ;
		obj = JCallMethod(Object, exc, id) ;
		if( obj == nil || (s = GetJavaStringText(obj)) == nil ) 	
			JError("%s", excName) ;
		else JError("%s: %s", excName, s) ;
	}
}


/* Initial signature handling */

static jobject GetTarget(register Pt t, Bool *isClass)
{
	VarValue(t) ;
	if( IsAtom(t) ) {
		if( t == tNullAtom )
			return JError("Accessing %s of the null object", nskind) ;
		else {
			*isClass = true ;
			return FindClass(XTestAtomName(t)) ;
		}
	}
	else {
		jobject obj = XTestJObjValue(t) ;
		*isClass = JIsInstanceOf(obj, classClass) ;
		return obj ;
	}
}

static void	GetNameSig(CharPt n, CharPt name, CharPt *sig, int maxName)
{
/* Pre-validate sig */
	if( (*sig = strchr(n, ':')) == nil )
		JErrorSig() ;
	if( n == *sig )
		JError("Missing name in %s signature", nskind) ;	
/* Get name */
	if( *sig - n > maxName )
		JError("Too long a name in %s signature", nskind) ;	
	strncpy(name, n, *sig - n) ;
	name[*sig - n] = '\0' ;
	(*sig)++ ;
}


/* Validation of signatures */

static CharPt dummySig ;

static jclass SigToClass(CharPt sig, CharPt *outSig)
{
	CharPt stop ;
	Char save ;
	jclass cls ;
	switch( *sig ) {
		case 'Z': case 'B': case 'C': case 'S':
		case 'I': case 'J': case 'F': case 'D': {
			stop = sig + 1 ;
			*outSig = stop ;
			break ;
		}
		case 'L': {
			sig++ ;
			if( (stop = strchr(sig, ';')) == nil ) JErrorSig() ;
			*outSig = stop + 1 ;
			break ;
		}
		case '[': {
			for( stop = sig ;  *stop == '[' ; stop++ ) ;
			switch( *stop ) {
				case 'Z': case 'B': case 'C': case 'S':
				case 'I': case 'J': case 'F': case 'D':
					*outSig = ++stop ;
					break ;
				case 'L':
					if( (stop = strchr(sig, ';')) == nil ) JErrorSig() ;
					*outSig = ++stop ;
					break ;
				default:
					JErrorSig() ;
			}
			break ;
		}
		default:
			return JErrorSig() ;
	}
	save = *stop ;
	*stop = '\0' ;
	cls = FindClass(sig) ;
	*stop = save ;
	return cls ;
}

static void ValidateFieldSig(CharPt sig, CharPt *outSig, Bool last)
{
	switch( *sig ) {
		case 'Z': case 'B': case 'C': case 'S':
		case 'I': case 'J': case 'F': case 'D':
			*outSig = sig + 1 ;
			break ;
		case 'L': case '[':
			SigToClass(sig, outSig) ;
			break ;
		default:
			JErrorSig() ;
	}
	if( last && **outSig != '\0' ) JErrorSig() ;
}

static void ValidateMethodSig(CharPt sig)
{
	if( *sig++ != '(' ) JErrorSig() ;
	while( *sig != '\0' && *sig != ')' )
		ValidateFieldSig(sig, &sig, false) ;
	if( *sig++ != ')' ) JErrorSig() ;
	if( *sig == 'V' ) {
		if( sig[1] != '\0' ) JErrorSig() ;
	}
	else ValidateFieldSig(sig, &dummySig, true) ;
}


/* Arrays */

static jstring NewJString(CharPt name)
{
	jstring jstr ;
	if( (jstr = JNewStringUTF(name)) == nil )
		JError("Could not create java string '%s'", name) ;
	return jstr ;
}

static jarray NewJArray(CharPt elemSig, int size)
{
	jarray arr ;
	switch( *elemSig ) {
		case 'Z': arr = JNewArray(Boolean, size) ; break ;
		case 'B': arr = JNewArray(Byte, size) ; break ;
		case 'C': arr = JNewArray(Char, size) ; break ;
		case 'S': arr = JNewArray(Short, size) ; break ;
		case 'I': arr = JNewArray(Int, size) ; break ;
		case 'J': arr = JNewArray(Long, size) ; break ;
		case 'F': arr = JNewArray(Float, size) ; break ;
		case 'D': arr = JNewArray(Double, size) ; break ;
		case 'L': case '[': {
			jclass elemClass = SigToClass(elemSig, &dummySig) ;
			arr = JNewObjectArray(size, elemClass) ;
			break ;
		}
		default: return InternalError("NewJArray (default)") ;
	}
	if( arr == nil ) JError("Could not create java array") ;
	return arr ;
}

static jarray NewJArrayC(jclass elemClass, int size)
{
	jarray arr ;
	if( JCallMethod(Boolean, elemClass, classClass_isPrimitive) ) {
		if( JIsSameObject(elemClass, intClass) ) return JNewArray(Int, size) ;		
		if( JIsSameObject(elemClass, doubleClass) ) return JNewArray(Double, size) ;		
		if( JIsSameObject(elemClass, booleanClass) ) return JNewArray(Boolean, size) ;		
		if( JIsSameObject(elemClass, charClass) ) return JNewArray(Char, size) ;		
		if( JIsSameObject(elemClass, floatClass) ) return JNewArray(Float, size) ;		
		if( JIsSameObject(elemClass, longClass) ) return JNewArray(Long, size) ;		
		if( JIsSameObject(elemClass, shortClass) ) return JNewArray(Short, size) ;		
		if( JIsSameObject(elemClass, byteClass) ) return JNewArray(Byte, size) ;		
		return InternalError("NewJArrayC") ;
	}
	else
		arr = JNewObjectArray(size, elemClass) ;
	if( arr == nil ) JError("Could not create java array") ;
	return arr ;
}

static jvalue GetJArray(CharPt elemSig, jarray arr, int idx)
{
  jvalue val ;
  if( arr == nil ) JError("Array is the null object") ;
  switch( *elemSig ) {
	case 'Z': JArrayGet(Boolean, arr, idx, &(val.z)) ; break ;
	case 'B': JArrayGet(Byte, arr, idx, &(val.b)) ; break ;
	case 'C': JArrayGet(Char, arr, idx, &(val.c)) ; break ;
	case 'S': JArrayGet(Short, arr, idx, &(val.s)) ; break ;
	case 'I': JArrayGet(Int, arr, idx, &(val.i)) ; break ;
	case 'J': JArrayGet(Long, arr, idx, &(val.j)) ; break ;
	case 'F': JArrayGet(Float, arr, idx, &(val.f)) ; break ;
	case 'D': JArrayGet(Double, arr, idx, &(val.d)) ; break ;
	case 'L': /* fall through */
	case '[': JObjectArrayGet(arr, idx, &(val.l)) ; break ;
	default: JErrorSig() ;		
  }
  return val ;
}

static void SetJArray(CharPt elemSig, jarray arr, int idx, jvalue *old, jvalue val)
{
  if( arr == nil ) JError("Array is the null object") ;
  switch( *elemSig ) {
	case 'Z': JArraySet(Boolean, arr, idx, &(old->z), &(val.z)) ; break ;
	case 'B': JArraySet(Byte, arr, idx, &(old->b), &(val.b)) ; break ;
	case 'C': JArraySet(Char, arr, idx, &(old->c), &(val.c)) ; break ;
	case 'S': JArraySet(Short, arr, idx, &(old->s), &(val.s)) ; break ;
	case 'I': JArraySet(Int, arr, idx, &(old->i), &(val.i)) ; break ;
	case 'J': JArraySet(Long, arr, idx, &(old->j), &(val.j)) ; break ;
	case 'F': JArraySet(Float, arr, idx, &(old->f), &(val.f)) ; break ;
	case 'D': JArraySet(Double, arr, idx, &(old->d), &(val.d)) ; break ;
	case 'L': /* fall through */
	case '[': JObjectArraySet(arr, idx, &(old->l), &(val.l)) ; break ;	
	default: JErrorSig() ;		
  }
}


/* Conversions */

static jvalue PrologToJava(CharPt sig, CharPt *outSig, Pt t, Bool convMore)
{											/* pre: valid signature */
	jvalue val ;
	VarValue(t) ;
	if( t == tNullAtom ) t = tNullJObj ;
	*outSig = sig + 1 ;
	switch( *sig ) {
		case 'Z': val.z = XTestBool(t) ; break ;
		case 'B': val.b = XTestIntRange(t, SCHAR_MIN, SCHAR_MAX) ; break ;
		case 'C': val.c = XTestChar(t) ; break ;
		case 'S': val.s = XTestIntRange(t, SHRT_MIN, SHRT_MAX) ; break ;
		case 'I': val.i = XTestInt(t) ; break ;
		case 'J': val.j = XTestInt64(t) ; break ;
		case 'F': val.f = XTestFloat(t) ; break ;
		case 'D': val.i = XTestFloat(t) ; break ;
		case 'L': {
			jclass expectedClass = SigToClass(sig, outSig) ;
			if( convMore && IsAtom(t) ) { /* Convert prolog atom to java string or class */
				if( JIsSameObject(expectedClass, stringClass) )
					val.l = NewJString(XAtomName(t)) ;
				elif( JIsSameObject(expectedClass, classClass) )
					val.l = FindClass(XAtomName(t)) ;
				else  JError("Invalid prolog->java conversion") ;
			}
			else {
				val.l = XTestJObjValue(t) ;
				CheckIfInstanceOf(val.l, expectedClass) ;
			}
			break ;
		}
		case '[': {
			jclass expectedClass = SigToClass(sig, outSig) ;
			CharPt elemSig = sig + 1 ;
			if( convMore && t == tNilAtom ) /* Convert prolog list to java array */	
				val.l = NewJArray(elemSig, 0) ;
			elif( convMore && IsList(t) ) { /* Convert prolog list to java array */
				int i, len = ListLength(t) ;
				val.l = NewJArray(elemSig, len) ;
				for( i = 0 ; i < len ; t = Drf(XListTail(t)), i++ ) {
					jvalue elem = PrologToJava(elemSig, &dummySig, XListHead(t), convMore) ;
					SetJArray(elemSig, val.l, i, nil, elem) ;
				}
			}
			else {
				val.l = XTestJObjValue(t) ;
				CheckIfInstanceOf(val.l, expectedClass) ;
			}
			break ;
		}
		default: InternalError("PrologToJava (default)") ;
	}
	return val ;
}

static Pt JavaToProlog(CharPt sig, jvalue val, Bool convMore)
{											/* pre: valid signature */
	switch( *sig ) {
		case 'Z': return MakeBool(val.z) ; break ;
		case 'B': return MakeInt(val.b) ; break ;
		case 'C': return MakeChar(val.c) ; break ;
		case 'S': return MakeInt(val.s) ; break ;
		case 'I': return MakeInt(val.i) ; break ;
		case 'J': return MakeInt64(val.j) ; break ;
		case 'F': return MakeFloat(val.f) ; break ;
		case 'D': return MakeFloat(val.d) ; break ;
		case 'L': {
			jclass expectedClass ;
			if( val.l == nil ) return tNullAtom ;
			expectedClass = SigToClass(sig, &dummySig) ;
			CheckIfInstanceOf(val.l, expectedClass) ;
			if( convMore && JIsSameObject(expectedClass, stringClass) )
			{   /* Convert java string to prolog atom */
				CharPt str = GetJavaStringText(val.l) ;
				if( str == nil ) return InternalError("JavaToProlog (L)") ;
				else return MakeTempAtom(str) ;
			}
			elif( convMore && JIsSameObject(expectedClass, classClass) )
			{   /* Convert java class to prolog atom */
				return MakeTempAtom(ClassName(val.l)) ;
			}
			else return MakeJObj(val.l) ;
			break ;
		}
		case '[': {
			jclass expectedClass ;
			if( val.l == nil ) return tNullAtom ;
			expectedClass = SigToClass(sig, &dummySig) ;
			CheckIfInstanceOf(val.l, expectedClass) ;
			if( convMore ) { /* Convert java array to prolog list */
				CharPt elemSig = sig + 1 ;
				int len = JGetArrayLength(val.l) ;
				if( len == 0 ) return tNilAtom ;
				else {
					int i ;
					Pt list = tNilAtom ;
					Hdl h = &list + 1 ;	
					for( i = 0 ; i < len ; i++ ) {
						jvalue elem = GetJArray(elemSig, val.l, i) ;
						h[-1] = MakeList(JavaToProlog(elemSig, elem, convMore), tNilAtom) ;
						h = H ;
					}
					return list ;
				}
			}
			else return MakeJObj(val.l) ;
			break ;
		}
		default: return InternalError("JavaToProlog (default)") ;
	}
}

static void PrologToJavaMultiple(CharPt sig, register Pt t, jvalue *vals, Bool convMore)
{											/* pre: valid signature */
	int i ;
	if( *sig++ != '(' ) InternalError("PrologToJavaMultiple (1)") ;
	for( i = 0, t = Drf(t) ; IsList(t) ; i++, t = Drf(XListTail(t)) ) {
		if( *sig == ')' ) JError("Too many arguments") ;
		else vals[i] = PrologToJava(sig, &sig, XListHead(t), convMore) ;
	}
	if( t != tNilAtom ) JError("Malformed argument list") ;
	if( *sig++ != ')' ) JError("Too few arguments") ;
}


/* Java start & stop */

static void JavaStart(void)
{
	JavaVMInitArgs vm_args ;
	JavaVMOption options[1] ;
	JavaVM *j ;
	Str2K path ;
	CharPt s ;
	if( jvm != nil )
		JError("Java interface already running") ;

	strcpy(path, "-Djava.class.path=") ;
	if( (s = OSGetEnv("CLASSPATH")) != nil )
		strcat(path, s) ;	
	options[0].optionString = path ;

	vm_args.version = JNI_VERSION_1_2 ;
	vm_args.options = options;
	vm_args.nOptions = 1;
	vm_args.ignoreUnrecognized = JNI_TRUE;
	if( JNI_CreateJavaVM(&j, (void**)&env, &vm_args) != 0 )
		JError("Could not create Java Virtual Machine") ;

	classClass = FindNormalClassChecked("java/lang/Class") ;
	systemClass = FindNormalClassChecked("java/lang/System") ;
	stringClass = FindNormalClassChecked("java/lang/String") ;

	booleanClass = FindPrimitiveClassChecked("java/lang/Boolean") ;
	byteClass = FindPrimitiveClassChecked("java/lang/Byte") ;
	charClass = FindPrimitiveClassChecked("java/lang/Character") ;
	shortClass = FindPrimitiveClassChecked("java/lang/Short") ;
	intClass = FindPrimitiveClassChecked("java/lang/Integer") ;
	longClass = FindPrimitiveClassChecked("java/lang/Long") ;
	floatClass = FindPrimitiveClassChecked("java/lang/Float") ;
	doubleClass = FindPrimitiveClassChecked("java/lang/Double") ;
	voidClass = FindPrimitiveClassChecked("java/lang/Void") ;

	constructorClass = FindNormalClassChecked("java/lang/reflect/Constructor") ;
	methodClass = FindNormalClassChecked("java/lang/reflect/Method") ;
	fieldClass = FindNormalClassChecked("java/lang/reflect/Field") ;

	classClass_getName =
		GetMethodIDChecked(classClass, "getName", "()Ljava/lang/String;") ;
	classClass_isArray =
		GetMethodIDChecked(classClass, "isArray", "()Z") ;
	classClass_isPrimitive =
		GetMethodIDChecked(classClass, "isPrimitive", "()Z") ;
	classClass_getComponentType =
		GetMethodIDChecked(classClass, "getComponentType", "()Ljava/lang/Class;") ;
	classClass_getConstructors =
		GetMethodIDChecked(classClass, "getConstructors", "()[Ljava/lang/reflect/Constructor;") ;
	classClass_getMethods =
		GetMethodIDChecked(classClass, "getMethods", "()[Ljava/lang/reflect/Method;") ;
	classClass_getFields =
		GetMethodIDChecked(classClass, "getFields", "()[Ljava/lang/reflect/Field;") ;

	jvm = j ;	/* Now is oficial: the jvm is active */
}

#if unused
static void JavaStop()
{
	if( jvm ==  nil )
			JError("Java interface is not running") ;
	if( JExceptionOccurred() )
		JExceptionDescribe() ;
	(*jvm)->DestroyJavaVM(jvm) ;
	jvm = nil ;
}
#endif


/* CXPROLOG C'BUILTINS */

static void PJavaCall()
{
	jobject target ;
	Bool targetIsClass ;
	CharPt sig ;
	Str256 name ;
	jvalue args[10] ;
	jmethodID id ;
	jclass cls ;
	jobject obj ;
	
	if( jvm == nil ) JavaStart() ;
/* Pre-validate sig and get method name */
	PrepareJErrors(XTestAtomName(X1), "method") ;
	GetNameSig(ns, name, &sig, 250) ;
/* Get target */
	target = GetTarget(X0, &targetIsClass) ;
/*** Is it a constructor? */
	if( StrEqual(name, "<init>") ) {
/* Yes, it is a constructor */
		PrepareJErrors(ns, "constructor") ;
		if( !targetIsClass )
			JError("The target of a constructor must be a class") ;
/* Get the constructor ID */
		id = JGetMethodID(target, name, sig) ;
		if( id == nil ) { /* No matching constructor? */
			JExceptionClear() ;
/*** Is it the array constructor? */
			if( JCallMethod(Boolean, target, classClass_isArray) )
			{
/* Yes, it is the array constructor */
				jclass elemClass ;
				jarray arr ;
				PrepareJErrors(ns, "array constructor") ;
				if( !StrEqual(sig, "(I)V") )
					JError("The only array constructor is <init>:(I)V") ;
				elemClass = JCallMethod(Object, target, classClass_getComponentType) ;
				PrologToJavaMultiple(sig, X2, args, true) ;
				arr = NewJArrayC(elemClass, args[0].i) ;
				MustBe( UnifyWithAtomic(X3, MakeJObj(arr)) ) ;
			}
	/* Give up: issue the apropriate error message */
			ValidateMethodSig(sig) ; /* If signature invalid, report now */
			if( sig[strlen(sig)-1] != 'V' )
				JError("Constructors must return void") ;
									/* Otherwise report missing constructor */ 
			JError("Inexistent constructor in class '%s'", ClassName(target)) ;
		}
/* Create the new object and return it */
	/* Assertion: at this point the field signature is valid */
		PrologToJavaMultiple(sig, X2, args, true) ;
		obj = JNewObjectA(target, id, args) ;
		if( obj == nil ) CheckException() ;
		MustBe( UnifyWithAtomic(X3, MakeJObj(obj)) ) ;
	}
/*** No, it is not a constructor */
/* Get the method ID */
	if( targetIsClass ) {
		id = JGetStaticMethodID(target, name, sig) ;
		if( id == nil ) { /* No static method available, search the method in classClass */
			JExceptionClear() ;
			id = JGetMethodID(classClass, name, sig) ;
			targetIsClass = false ; /* The method to call is not static */
			cls = target ; /* For error messages */
		}
		else cls = nil ; /* avoids warning */
	}
	else {
		cls = JGetObjectClass(target) ;
		id = JGetMethodID(cls, name, sig) ;
	}
	if( id == nil ) { /* No matching method? */
		JExceptionClear() ;
		ValidateMethodSig(sig) ; /* If signature invalid, report now */
		JError("Inexistent method in class '%s'", ClassName(cls)) ; /* Otherwise report missing method */
	}
/* Do the call and handle result */
	/* Assertion: at this point the field signature is valid */
	PrologToJavaMultiple(sig, X2, args, true) ;
	switch( sig[strlen(sig)-1] ) {
		case 'Z': {
			jboolean z = JCallMethodA(Boolean, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeBool(z)) ) ;
		}
		case 'B': {
			jbyte b = JCallMethodA(Byte, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeInt(b)) ) ;
		}
		case 'C': {
			jchar c = JCallMethodA(Char, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeChar(c)) ) ;
		}
		case 'S': {
			jshort s = JCallMethodA(Short, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeInt(s)) ) ;
		}
		case 'I': {
			jint i = JCallMethodA(Int, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeInt(i)) ) ;
		}
		case 'J': {
			jlong j = JCallMethodA(Long, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeInt64(j)) ) ;
		}
		case 'F': {
			jfloat f = JCallMethodA(Float, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeFloat(f)) ) ;
		}
		case 'D': {
			jdouble d = JCallMethodA(Double, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeFloat(d)) ) ;
		}
		case 'V': {
			JCallMethodA(Void, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, tVoidAtom) ) ;
		}
		case ';': { /* cases 'L' & '[' */
			jobject obj = JCallMethodA(Object, targetIsClass, target, id, args) ;
			CheckException() ;
			MustBe( UnifyWithAtomic(X3, MakeJObj(obj)) ) ;
		}
		default:
			InternalError("PJavaCall") ;
	}
}

static void PJavaField()
{
	jobject target ;
	Bool targetIsClass ;
	CharPt sig ;
	Str256 name ;
	jvalue val ;
	jfieldID id ;
	jclass cls ;

	if( jvm == nil ) JavaStart() ;
/* Pre-validate sig and get field name */
	PrepareJErrors(XTestAtomName(X1), "field") ;
	GetNameSig(ns, name, &sig, 250) ;
/* Get target */
	target = GetTarget(X0, &targetIsClass) ;
/* Get the field ID */
	if( targetIsClass ) {
		id = JGetStaticFieldID(target, name, sig) ;
		if( id == nil ) { /* No static field available, find a field in classClass */
			JExceptionClear() ;
			id = JGetFieldID(classClass, name, sig) ;
			targetIsClass = false ; /* The field to access is not static */
			cls = target ; /* For error messages */
		}
		else cls = nil ; /* avoids warning */
	}
	else {
		cls = JGetObjectClass(target) ;
		id = JGetFieldID(cls, name, sig) ;
	}
	if( id == nil ) { /* No matching field? */
		JExceptionClear() ;
		ValidateFieldSig(sig, &dummySig, true) ; /* If signature invalid, report now */
		JError("Inexistent field in class '%s'", ClassName(cls)) ; /* Otherwise report missing field */ 
	}
/* Get/Set the field */
	/* Assertion: at this point the field signature is valid */
	switch( *sig ) {
		case 'Z': {
			jboolean old = JGetField(Boolean, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeBool(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( val.z == old ) JumpNext() ;
			JSetField(Boolean, targetIsClass, target, id, val.z) ;
			JumpNext() ;
		}
		case 'B': {
			jbyte old = JGetField(Byte, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeByte(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( val.b == old ) JumpNext() ;
			JSetField(Byte, targetIsClass, target, id, val.b) ;
			JumpNext() ;
		}
		case 'C': {
			jchar old = JGetField(Char, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeChar(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( val.c == old ) JumpNext() ;
			JSetField(Char, targetIsClass, target, id, val.c) ;
			JumpNext() ;
		}
		case 'S': {
			jshort old = JGetField(Short, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeInt(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( val.s == old ) JumpNext() ;
			JSetField(Short, targetIsClass, target, id, val.s) ;
			JumpNext() ;
		}
		case 'I': {
			jint old = JGetField(Int, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeInt(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( val.i == old ) JumpNext() ;
			JSetField(Int, targetIsClass, target, id, val.i) ;
			JumpNext() ;
		}
		case 'J': {
			jlong old = JGetField(Long, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeInt64(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( val.j == old ) JumpNext() ;
			JSetField(Long, targetIsClass, target, id, val.j) ;
			JumpNext() ;
		}
		case 'F': {
			jfloat old = JGetField(Float, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeFloat(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( val.f == old ) JumpNext() ;
			JSetField(Float, targetIsClass, target, id, val.f) ;
			JumpNext() ;
		}
		case 'D': {
			jdouble old = JGetField(Double, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeFloat(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( val.d == old ) JumpNext() ;
			JSetField(Double, targetIsClass, target, id, val.d) ;
			JumpNext() ;
		}
		case 'L': case '[': {
			jobject old = JGetField(Object, targetIsClass, target, id) ;
			Ensure( UnifyWithAtomic(X2, MakeJObj(old)) ) ;
			val = PrologToJava(sig, &dummySig, X3, true) ;
			if( JIsSameObject(old, val.l) ) JumpNext() ;
			JSetField(Object, targetIsClass, target, id, val.l) ;
			JumpNext() ;
		}
		default:
			InternalError("PJavaField") ;
	}
}

static void PJavaArray()
{
	CharPt sig, elemSig ;
	int idx ;
	jvalue old, val ;
	jclass expectedClass ;
	jarray arr ;

	if( jvm == nil ) JavaStart() ;

	PrepareJErrors(XTestAtomName(X0), "array") ;
	sig = ns ;
	elemSig = sig + 1 ;
	ValidateFieldSig(sig, &dummySig, true) ; /* If signature invalid, report now */	

	expectedClass = SigToClass(sig, &dummySig) ;
	arr = XTestJObjValue(X1) ;
	CheckIfInstanceOf(arr, expectedClass) ;
	
	idx = XTestNat(X2) ;
	
	old = GetJArray(elemSig, arr, idx) ;
	Ensure( UnifyWithAtomic(X3, JavaToProlog(elemSig, old, false)) ) ;

	val = PrologToJava(elemSig, &dummySig, X4, true) ;
	SetJArray(elemSig, arr, idx, &old, val) ;
	JumpNext() ;
}

static void PJavaConvert() /* full conversion java (X1) <->  prolog (X2) */
{
	CharPt sig ;
	jvalue val ;
	Pt t1 ;
	if( jvm == nil ) JavaStart() ;
	PrepareJErrors(XTestAtomName(X0), "conversion") ;
	sig = ns ;
	ValidateFieldSig(sig, &dummySig, true) ; /* If signature invalid, report now */	

	t1 = Drf(X1) ;
	if( *sig == 'L' || *sig == '[' ) {
		if( IsVar(t1) ) { /* convert prolog (X2) -> java (X1) */
			val = PrologToJava(sig, &dummySig, X2, true) ;
			MustBe( UnifyWithAtomic(t1, MakeJObj(val.l)) ) ;
		}
		else { /* convert java (X1) -> prolog (X2) */
			val.l = XTestJObjValue(t1) ;
			MustBe( Unify(X2, JavaToProlog(sig, val, true)) ) ;
		}
	}
	else {
		if( IsVar(t1) ) { /* convert prolog (X2) -> java (X1) */
			PrologToJava(sig, &dummySig, X2, true) ; /* only for validation */
			MustBe( UnifyWithAtomic(t1, X2) ) ;
		}
		else { /* convert java (X1) ->  prolog (X2) */
			PrologToJava(sig, &dummySig, t1, true) ; /* only for validation */
			MustBe( UnifyWithAtomic(X2, t1) ) ;
		}
	}
}

static void PJavaSameObject()
{
	jobject obj0, obj1 ;
	if( jvm == nil ) JavaStart() ;
	obj0 = XTestJObjValue(X0) ;
	obj1 = XTestJObjValue(X1) ;
	MustBe( JIsSameObject(obj0, obj1) )	 ;
}

void JavaInit()
{
	jobjType = ExtraTypeNew("JOBJ", WordsOf(JObj), JObjNamingFun) ;
	PrepareJErrors("", "") ;
	CreateNullStuff() ;
	jvm = nil ;	/* jvm is inactive at startup */
	InstallCBuiltinPred("java_call", 4, PJavaCall) ;
	InstallCBuiltinPred("java_field", 4, PJavaField) ;
	InstallCBuiltinPred("java_array", 5, PJavaArray) ;
	InstallCBuiltinPred("java_convert", 3, PJavaConvert) ;
	InstallCBuiltinPred("java_same", 2, PJavaSameObject) ;
}

#else

void JavaInit()
{
	/* nothing */
}

#endif /* USE_JAVA */

/*
gcc  -I/usr/java/j2/include:/usr/java/j2/include/linux -L/usr/java/j2/jre/lib/i386/client/ -ljvm Java.c -o Java
LD_LIBRARY_PATH=/usr/java/j2/jre/lib/i386/client/:/usr/java/j2/jre/lib/i386/ Java
*/
