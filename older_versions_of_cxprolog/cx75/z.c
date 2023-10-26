main()
{
	long z[40] ;
	static long n = 0 ;
	if( ++n % 1000 == 0 )
		printf("%ld\n", n) ;
	main() ;
}
