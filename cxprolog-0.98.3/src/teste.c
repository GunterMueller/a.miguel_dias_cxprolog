#include <stdio.h>
#include <stdint.h>

#if defined(__LP64__)
typedef uint64_t Word ;
typedef int64_t PInt ;
#else
typedef uint32_t Word ;
typedef int32_t PInt ;
#endif

typedef Word *Pt ;


#if __GLIBC__
#  include <malloc.h>
#elif __APPLE__
#  include <malloc/malloc.h>
#endif


/* Use OWN_MEMORY_MANAGER or MALLOC ? */
#define USE_OWN_MEMORY_MANAGER		1

#if defined(USE_VALGRIND)
#  undef USE_OWN_MEMORY_MANAGER
#endif

#if USE_OWN_MEMORY_MANAGER && defined(M_MMAP_THRESHOLD)
#  define USE_MMAP		1
#  include <sys/mman.h>
#endif





int main(void)
{
	Word w;
	Pt p;
	void *mem ;

	printf("%zd %zd\n\n", sizeof(w), sizeof(p)) ;
#if USE_MMAP
	printf("USE_MMAP\n") ;
	mem = mmap(NULL, 128 * 1024 * 8, PROT_READ|PROT_WRITE,
                                        MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) ;
#else
	printf("NO USE_MMAP\n") ;
	mem = malloc(128 * 1024 * 8) ;
#endif
	printf("mem = %p\n\n", mem) ;

	system("lsb_release -a");
	printf("\n") ;

	system("cat /etc/issue.net");
	printf("\n") ;

	system("uname -a");
	printf("\n") ;

	system("free");
	printf("\n") ;


	return 0;
}
