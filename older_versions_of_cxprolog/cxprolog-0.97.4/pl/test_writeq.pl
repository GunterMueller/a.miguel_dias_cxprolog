:- op(100, xfx,'+a+').
:- op(100, xfx,'a+a').
:- op(100, xfx,'AA').
:- op(100, xfx,'  ').
:- op(100, xfx,'123').
:- op(100, xfx,'+.+').
:- op(100, fy,'ooo').
:- op(100, yf,!).
:- op(100, yf,#).

:- write('+a++' '+a+' 1), nl.
:- writeq('+a++' '+a+' 1), nl.
:- nl.

:- write('+a++' 'AA' 1), nl.
:- writeq('+a++' 'AA' 1), nl.
:- nl.

:- write('+a++' '  ' 1), nl.
:- writeq('+a++' '  ' 1), nl.
:- nl.

:- write('+a++' '123' 1), nl.
:- writeq('+a++' '123' 1), nl.
:- nl.

:- write('+a++' 'a+a' 1), nl.
:- writeq('+a++' 'a+a' 1), nl.
:- nl.

:- write(a is b), nl.
:- writeq(a is b), nl.
:- nl.

:- write(a is 1+1), nl.
:- writeq(a is 1+1), nl.
:- nl.

:- write(a+1), nl.
:- writeq(a+1), nl.
:- nl.

:- write(a+'*'), nl.
:- writeq(a+'*'), nl.
:- nl.

:- write('*' '+.+' '*'), nl.
:- writeq('*' '+.+' '*'), nl.
:- nl.

:- write(a '+.+' b), nl.
:- writeq(a '+.+' b), nl.
:- nl.

:- write(a>(=(1,2,3,4))), nl.
:- writeq(a>(=(1,2,3,4))), nl.
:- nl.

:- write((a-> -b)), nl.
:- writeq((a-> -b)), nl.
:- nl.

:- write((a->ooo ooo b)), nl.
:- writeq((a->ooo ooo b)), nl.
:- nl.

:- write((a!! ->b)), nl.
:- writeq((a!! ->b)), nl.
:- nl.

:- write((a# ->b)), nl.
:- writeq((a# ->b)), nl.
:- nl.

:- write((a->[])), nl.
:- writeq((a->[])), nl.
:- nl.

:- write((*,*)), nl.
:- writeq((*,*)), nl.
:- nl.

:- write((',',',')), nl.
:- writeq((',',',')), nl.
:- nl.
