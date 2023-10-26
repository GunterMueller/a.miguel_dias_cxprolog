	.file	"String.c"
	.text
.globl StrHash
	.type	StrHash,@function
StrHash:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$0, -8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L2
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L2:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L4
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L4:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L6
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L6:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L8
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L8:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L10
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L10:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L12
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L12:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L14
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L14:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L16
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L16:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L18
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L18:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L20
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L20:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L22
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L22:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L24
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L24:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L26
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L26:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L28
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L28:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L30
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L30:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	incl	8(%ebp)
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L32
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L1
.L32:
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	addl	%eax, -8(%ebp)
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
.L1:
	movl	-4(%ebp), %eax
	leave
	ret
.Lfe1:
	.size	StrHash,.Lfe1-StrHash
.globl StrSimilar
	.type	StrSimilar,@function
StrSimilar:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$20, %esp
	movl	20(%ebp), %eax
	movw	%ax, -6(%ebp)
	cmpw	$0, -6(%ebp)
	je	.L35
.L36:
	leal	16(%ebp), %eax
	decl	(%eax)
	cmpl	$-1, 16(%ebp)
	jne	.L38
	jmp	.L42
.L38:
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	je	.L40
	movl	12(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L39
.L40:
	movl	8(%ebp), %eax
	movl	12(%ebp), %edx
	movb	(%eax), %al
	cmpb	(%edx), %al
	sete	%al
	movzbl	%al, %eax
	movl	%eax, -12(%ebp)
	jmp	.L34
.L39:
	subl	$12, %esp
	movl	8(%ebp), %eax
	movzbl	(%eax), %eax
	pushl	%eax
	call	toupper
	addl	$16, %esp
	movl	%eax, %ebx
	subl	$12, %esp
	movl	12(%ebp), %eax
	movzbl	(%eax), %eax
	pushl	%eax
	call	toupper
	addl	$16, %esp
	cmpl	%eax, %ebx
	je	.L41
	movl	$0, -12(%ebp)
	jmp	.L34
.L41:
	incl	8(%ebp)
	leal	12(%ebp), %eax
	incl	(%eax)
	jmp	.L36
.L35:
	nop
.L43:
	leal	16(%ebp), %eax
	decl	(%eax)
	cmpl	$-1, 16(%ebp)
	jne	.L45
	jmp	.L42
.L45:
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	je	.L47
	movl	12(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L46
.L47:
	movl	8(%ebp), %eax
	movl	12(%ebp), %edx
	movb	(%eax), %al
	cmpb	(%edx), %al
	sete	%al
	movzbl	%al, %eax
	movl	%eax, -12(%ebp)
	jmp	.L34
.L46:
	movl	8(%ebp), %eax
	movl	%eax, %ecx
	movl	12(%ebp), %eax
	movl	%eax, %edx
	leal	12(%ebp), %eax
	incl	(%eax)
	incl	8(%ebp)
	movb	(%ecx), %al
	cmpb	(%edx), %al
	je	.L43
	movl	$0, -12(%ebp)
	jmp	.L34
.L42:
	movl	$1, -12(%ebp)
.L34:
	movl	-12(%ebp), %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe2:
	.size	StrSimilar,.Lfe2-StrSimilar
.globl StrPerm
	.type	StrPerm,@function
StrPerm:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	subl	$8, %esp
	pushl	8(%ebp)
	pushl	8(%ebp)
	call	strlen
	addl	$4, %esp
	addl	$4, %eax
	shrl	$2, %eax
	pushl	%eax
	call	PermAllocate
	addl	$4, %esp
	pushl	%eax
	call	strcpy
	addl	$16, %esp
	leave
	ret
.Lfe3:
	.size	StrPerm,.Lfe3-StrPerm
	.type	GStrInit,@function
GStrInit:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$4, %esp
	movl	$0, -8(%ebp)
.L51:
	cmpl	$5, -8(%ebp)
	jle	.L54
	jmp	.L52
.L54:
	movl	-8(%ebp), %ebx
	subl	$12, %esp
	pushl	$16
	call	TempAllocate
	addl	$16, %esp
	movl	%eax, gStrs(,%ebx,4)
	movl	-8(%ebp), %eax
	movl	$64, gSizes(,%eax,4)
	leal	-8(%ebp), %eax
	incl	(%eax)
	jmp	.L51
.L52:
	movl	$0, gCurr
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe4:
	.size	GStrInit,.Lfe4-GStrInit
.globl GStrSize
	.type	GStrSize,@function
GStrSize:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$0, -4(%ebp)
	movl	$0, -8(%ebp)
.L56:
	cmpl	$5, -8(%ebp)
	jle	.L59
	jmp	.L57
.L59:
	movl	-8(%ebp), %eax
	movl	gSizes(,%eax,4), %eax
	addl	$3, %eax
	movl	%eax, %edx
	shrl	$2, %edx
	leal	-4(%ebp), %eax
	addl	%edx, (%eax)
	leal	-8(%ebp), %eax
	incl	(%eax)
	jmp	.L56
.L57:
	movl	-4(%ebp), %eax
	leave
	ret
.Lfe5:
	.size	GStrSize,.Lfe5-GStrSize
.globl GStrExpand
	.type	GStrExpand,@function
GStrExpand:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$4, %esp
	movl	gCurr, %eax
	movl	gSizes(,%eax,4), %eax
	addl	$3, %eax
	shrl	$2, %eax
	movl	%eax, -8(%ebp)
	movl	gCurr, %ebx
	subl	$8, %esp
	movl	-8(%ebp), %eax
	sall	$1, %eax
	pushl	%eax
	pushl	gStrBegin
	call	Reallocate
	addl	$16, %esp
	movl	%eax, gStrs(,%ebx,4)
	movl	gCurr, %edx
	movl	-8(%ebp), %eax
	sall	$3, %eax
	movl	%eax, gSizes(,%edx,4)
	movl	gCurr, %eax
	movl	gStrBegin, %edx
	movl	gStrs(,%eax,4), %eax
	subl	%edx, %eax
	addl	%eax, gStrPt
	movl	gCurr, %eax
	movl	gStrs(,%eax,4), %eax
	movl	%eax, gStrBegin
	movl	gCurr, %eax
	movl	gSizes(,%eax,4), %eax
	addl	gStrBegin, %eax
	subl	$8, %eax
	movl	%eax, gStrEnd
	movl	$0, %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe6:
	.size	GStrExpand,.Lfe6-GStrExpand
.globl GStrOpen
	.type	GStrOpen,@function
GStrOpen:
	pushl	%ebp
	movl	%esp, %ebp
	incl	gCurr
	cmpl	$6, gCurr
	jne	.L62
	movl	$0, gCurr
.L62:
	movl	gCurr, %eax
	movl	gStrs(,%eax,4), %eax
	movl	%eax, gStrBegin
	movl	%eax, gStrPt
	movl	gCurr, %eax
	movl	gSizes(,%eax,4), %eax
	addl	gStrBegin, %eax
	subl	$8, %eax
	movl	%eax, gStrEnd
	leave
	ret
.Lfe7:
	.size	GStrOpen,.Lfe7-GStrOpen
.globl GStrClose
	.type	GStrClose,@function
GStrClose:
	pushl	%ebp
	movl	%esp, %ebp
	movl	gStrPt, %eax
	movb	$0, (%eax)
	movl	gStrBegin, %eax
	leave
	ret
.Lfe8:
	.size	GStrClose,.Lfe8-GStrClose
.globl GStrAddStr
	.type	GStrAddStr,@function
GStrAddStr:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	nop
.L65:
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L67
	jmp	.L64
.L67:
	movl	gStrPt, %eax
	cmpl	gStrEnd, %eax
	jb	.L69
	call	GStrExpand
.L69:
	movl	gStrPt, %eax
	movl	%eax, %edx
	movl	8(%ebp), %eax
	movb	(%eax), %al
	movb	%al, (%edx)
	incl	8(%ebp)
	incl	gStrPt
	jmp	.L65
.L64:
	leave
	ret
.Lfe9:
	.size	GStrAddStr,.Lfe9-GStrAddStr
	.section	.rodata
.LC0:
	.string	"GStrFormatV"
	.text
.globl GStrFormatV
	.type	GStrFormatV,@function
GStrFormatV:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$4, %esp
	incl	gCurr
	cmpl	$6, gCurr
	jne	.L71
	movl	$0, gCurr
.L71:
	pushl	12(%ebp)
	pushl	8(%ebp)
	movl	gCurr, %eax
	pushl	gSizes(,%eax,4)
	movl	gCurr, %eax
	pushl	gStrs(,%eax,4)
	call	vsnprintf
	addl	$16, %esp
	incl	%eax
	movl	%eax, -8(%ebp)
	movl	gCurr, %edx
	movl	-8(%ebp), %eax
	cmpl	gSizes(,%edx,4), %eax
	jle	.L72
	subl	$12, %esp
	movl	gCurr, %eax
	pushl	gStrs(,%eax,4)
	call	Release
	addl	$16, %esp
	movl	gCurr, %ebx
	subl	$12, %esp
	movl	-8(%ebp), %eax
	addl	$3, %eax
	shrl	$2, %eax
	pushl	%eax
	call	TempAllocate
	addl	$16, %esp
	movl	%eax, gStrs(,%ebx,4)
	movl	gCurr, %edx
	movl	-8(%ebp), %eax
	addl	$3, %eax
	shrl	$2, %eax
	sall	$2, %eax
	movl	%eax, gSizes(,%edx,4)
	pushl	12(%ebp)
	pushl	8(%ebp)
	movl	gCurr, %eax
	pushl	gSizes(,%eax,4)
	movl	gCurr, %eax
	pushl	gStrs(,%eax,4)
	call	vsnprintf
	addl	$16, %esp
	incl	%eax
	movl	%eax, -8(%ebp)
	movl	gCurr, %edx
	movl	-8(%ebp), %eax
	cmpl	gSizes(,%edx,4), %eax
	jle	.L72
	subl	$12, %esp
	pushl	$.LC0
	call	InternalError
	addl	$16, %esp
.L72:
	movl	gCurr, %eax
	movl	gStrs(,%eax,4), %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe10:
	.size	GStrFormatV,.Lfe10-GStrFormatV
.globl GStrFormat
	.type	GStrFormat,@function
GStrFormat:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	leal	12(%ebp), %eax
	movl	%eax, -4(%ebp)
	subl	$8, %esp
	pushl	-4(%ebp)
	pushl	8(%ebp)
	call	GStrFormatV
	addl	$16, %esp
	leave
	ret
.Lfe11:
	.size	GStrFormat,.Lfe11-GStrFormat
	.section	.rodata
.LC1:
	.string	"%s"
	.text
.globl GStrMake
	.type	GStrMake,@function
GStrMake:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	subl	$8, %esp
	pushl	8(%ebp)
	pushl	$.LC1
	call	GStrFormat
	addl	$16, %esp
	leave
	ret
.Lfe12:
	.size	GStrMake,.Lfe12-GStrMake
.globl BigStrSize
	.type	BigStrSize,@function
BigStrSize:
	pushl	%ebp
	movl	%esp, %ebp
	movl	bigStrSize, %eax
	addl	$3, %eax
	shrl	$2, %eax
	leave
	ret
.Lfe13:
	.size	BigStrSize,.Lfe13-BigStrSize
	.section	.rodata
.LC2:
	.string	"bigstr"
	.text
.globl BigStrExpand
	.type	BigStrExpand,@function
BigStrExpand:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	bigStrSize, %eax
	addl	$3, %eax
	shrl	$2, %eax
	movl	%eax, -4(%ebp)
	pushl	$0
	movl	-4(%ebp), %eax
	sall	$1, %eax
	pushl	%eax
	pushl	-4(%ebp)
	pushl	$.LC2
	call	MemoryGrowWarning
	addl	$16, %esp
	subl	$8, %esp
	movl	-4(%ebp), %eax
	sall	$1, %eax
	pushl	%eax
	pushl	bigStrBegin
	call	Reallocate
	addl	$16, %esp
	movl	%eax, -8(%ebp)
	movl	-4(%ebp), %eax
	sall	$3, %eax
	movl	%eax, bigStrSize
	movl	bigStrBegin, %edx
	movl	-8(%ebp), %eax
	subl	%edx, %eax
	addl	%eax, bigStrPt
	movl	bigStrBegin, %edx
	movl	-8(%ebp), %eax
	subl	%edx, %eax
	addl	%eax, bigStrAPt
	movl	bigStrBegin, %edx
	movl	-8(%ebp), %eax
	subl	%edx, %eax
	addl	%eax, bigStrBPt
	movl	-8(%ebp), %eax
	movl	%eax, bigStrBegin
	movl	bigStrSize, %eax
	addl	bigStrBegin, %eax
	subl	$32, %eax
	movl	%eax, bigStrEnd
	movl	$0, %eax
	leave
	ret
.Lfe14:
	.size	BigStrExpand,.Lfe14-BigStrExpand
.globl BigStrOpen
	.type	BigStrOpen,@function
BigStrOpen:
	pushl	%ebp
	movl	%esp, %ebp
	movl	bigStrBegin, %eax
	movl	%eax, bigStrPt
	leave
	ret
.Lfe15:
	.size	BigStrOpen,.Lfe15-BigStrOpen
.globl BigStrClose
	.type	BigStrClose,@function
BigStrClose:
	pushl	%ebp
	movl	%esp, %ebp
	movl	bigStrPt, %eax
	movb	$0, (%eax)
	movl	bigStrBegin, %eax
	leave
	ret
.Lfe16:
	.size	BigStrClose,.Lfe16-BigStrClose
.globl BigStrInit
	.type	BigStrInit,@function
BigStrInit:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	subl	$12, %esp
	pushl	$1024
	call	TempAllocate
	addl	$16, %esp
	movl	%eax, bigStrBegin
	movl	$4096, bigStrSize
	movl	bigStrSize, %eax
	addl	bigStrBegin, %eax
	subl	$32, %eax
	movl	%eax, bigStrEnd
	leave
	ret
.Lfe17:
	.size	BigStrInit,.Lfe17-BigStrInit
.globl BigStrAddStr
	.type	BigStrAddStr,@function
BigStrAddStr:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
.L82:
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L84
	jmp	.L81
.L84:
	movl	bigStrPt, %eax
	cmpl	bigStrEnd, %eax
	jb	.L86
	call	BigStrExpand
.L86:
	movl	bigStrPt, %eax
	movl	%eax, %edx
	movl	8(%ebp), %eax
	movb	(%eax), %al
	movb	%al, (%edx)
	incl	8(%ebp)
	incl	bigStrPt
	jmp	.L82
.L81:
	leave
	ret
.Lfe18:
	.size	BigStrAddStr,.Lfe18-BigStrAddStr
.globl BigStrAddStrSlice
	.type	BigStrAddStrSlice,@function
BigStrAddStrSlice:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	12(%ebp), %edx
	movl	16(%ebp), %eax
	subl	%edx, %eax
	incl	%eax
	movl	%eax, -4(%ebp)
	subl	$8, %esp
	pushl	12(%ebp)
	pushl	8(%ebp)
	call	Utf8Pos
	addl	$16, %esp
	movl	%eax, 8(%ebp)
.L88:
	movl	-4(%ebp), %edx
	leal	-4(%ebp), %eax
	decl	(%eax)
	testl	%edx, %edx
	jle	.L87
	movl	8(%ebp), %eax
	cmpb	$0, (%eax)
	jne	.L90
	jmp	.L87
.L90:
	movl	bigStrPt, %eax
	cmpl	bigStrEnd, %eax
	jb	.L93
	call	BigStrExpand
.L93:
	subl	$8, %esp
	leal	8(%ebp), %eax
	pushl	%eax
	pushl	$bigStrPt
	call	Utf8Copy
	addl	$16, %esp
	jmp	.L88
.L87:
	leave
	ret
.Lfe19:
	.size	BigStrAddStrSlice,.Lfe19-BigStrAddStrSlice
.globl StringInit
	.type	StringInit,@function
StringInit:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	call	GStrInit
	call	BigStrInit
	leave
	ret
.Lfe20:
	.size	StringInit,.Lfe20-StringInit
	.comm	gStrBegin,4,4
	.comm	gStrEnd,4,4
	.comm	gStrPt,4,4
	.comm	bigStrBegin,4,4
	.comm	bigStrEnd,4,4
	.comm	bigStrPt,4,4
	.comm	bigStrAPt,4,4
	.comm	bigStrBPt,4,4
	.local	gStrs
	.comm	gStrs,24,4
	.local	gSizes
	.comm	gSizes,24,4
	.local	gCurr
	.comm	gCurr,4,4
	.local	bigStrSize
	.comm	bigStrSize,4,4
	.ident	"GCC: (GNU) 3.2.2 20030222 (Red Hat Linux 3.2.2-5)"
