% $Id: tokens.pl,v 1.6 90/09/27 21:25:09 sources Exp $

% =============================================================================
%
% $Log: tokens.pl,v $
% Revision 1.6  90/09/27  21:25:09  sources
% `prolog_type' directive should not fail... Took care of that.
% 
% Revision 1.5  90/09/27  21:18:01  sources
% Fixed typo.
% 
% Revision 1.4  90/09/27  21:17:00  sources
% Put stuff in so that this may be used with Quintus again.
% 
% Revision 1.3  90/09/27  21:12:23  sources
% Fixed some floating point troubles (dealing with integers getting
% large and Prolog getting more and more confused).
% 
% Revision 1.2  90/09/27  20:57:29  sources
% Converted to read input from a string, and also to be able to parse
% floating point numbers.
% 
% Revision 1.1  90/09/27  16:59:42  sources
% Initial revision
% 
%
%   Package: tokens
%   Author : Richard A. O'Keefe
%   Updated: 11/2/88
%   Defines: a public-domain tokeniser in reasonably standard Prolog.

:- prolog_type(quintus) ->
   module(tokens, [
    read_token_list/3           % string -> token list
   ]) ; true.

/*  This tokeniser is meant to complement the library READ routine.
    It recognises Dec-10 Prolog with the following exceptions:

    %( is not accepted as an alternative to {

    %) is not accepted as an alternative to }

    NOLC convention is not supported (read_name could be made to do it)

    ,.. is not accepted as an alternative to | (hooray!)

    large integers are not read in as xwd(Top18Bits,Bottom18Bits)

    After a comma, "(" is read as ' (' rather than '('.  This does the
    parser no harm at all, and the Dec-10 tokeniser's behaviour here
    doesn't actually buy you anything.  This tokeniser guarantees never
    to return '(' except immediately after an atom, yielding ' (' every
    other where.

    Some (KL-10, DEC-10 Prolog v3.53) times might be of interest.
    Applied to an earlier version of this file:
    this code took          1.66 seconds
    the Dec-10 tokeniser took   1.28 seconds
    A Pascal version took       0.96 seconds
    The Dec-10 tokeniser was called via the old RDTOK interface, with
    which this file is compatible.  One reason for the difference in
    speed is the way variables are looked up: this code uses a linear
    list, while the Dec-10 tokeniser uses some sort of tree.  The Pascal
    version is the program WLIST which lists "words" and their frequencies.
    It uses a hash table.  Another difference is the way characters are
    classified: the Dec-10 tokeniser and WLIST have a table which maps
    ASCII codes to character classes, and don't do all this comparison
    and and memberchking.  We could do that without leaving standard Prolog,
    but what do you want from one evening's work?

    Changes:
    integers can have underscores in them.
    characters in 0'x, "x", and 'x' can be escaped as in C or PopLog.
    So in 'foo\t%t\n' the \t and \n are now single characters.
    Radix notation is no longer exactly like DEC-10 Prolog v3.53.
    The radix may be any number of digits, e.g. 0016'deed.

    BEWARE: this file does _not_ recognise floating-point numbers.
    In order to make this file independent of whether character-escapes
    are enabled or not, the magic numbers
    9   (TAB)   and
    92  ( \ )
    are used.
*/



%   read_tokens(String, TokenList, Dictionary)
%   returns a list of tokens.  It is needed to "prime" read_tokens/2
%   with the initial blank, and to check for end of file.  The
%   Dictionary is a list of AtomName=Variable pairs in no particular order.
%   The way end of file is handled is that everything else FAILS when it
%   hits character -1, sometimes printing a warning.  It might have been
%   an idea to return the atom 'end_of_file' instead of the same token list
%   that you'd have got from reading "end_of_file. ", but (1) this file is
%   for compatibility, and (b) there are good practical reasons for wanting
%   this behaviour.
%
%   This version gets the characters from a string instead of reading them.

read_token_list([C|String], TokenList, Dictionary) :-
    read_tokens(C, String, Dict, ListOfTokens),
    terminate_list(Dict),       %  fill in the "hole" at the end.
    !,              %  we have to unify explicitly so
    Dictionary = Dict,      %  that we'll read and then check
    TokenList = ListOfTokens.   %  even with filled in arguments.
read_token_list([], [atom(end_of_file)], []).   %  End Of File is only problem.

terminate_list([]).
terminate_list([_|Tail]) :-
    terminate_list(Tail).



read_tokens(-1, _, _, _) :- !,
    fail.

read_tokens(Ch, [NextCh|String], Dict, Tokens) :-
    Ch =< " ",
    !,
    read_tokens(NextCh, String, Dict, Tokens).
read_tokens(0'%, [Ch|String], Dict, Tokens) :-
    Ch < " ", Ch =\= 9, !, read_tokens(Ch, String, Dict, Tokens).
read_tokens(0'%, [_Ch|String], Dict, Tokens) :- !,
    read_tokens(0'%, String, Dict, Tokens).
read_tokens(0'/, [NextCh|String], Dict, Tokens) :- !,
    read_solidus(NextCh, String, Dict, Tokens).
read_tokens(0'!, [NextCh|String], Dict, [atom(!)|Tokens]) :- !,
    read_after_atom(NextCh, String, Dict, Tokens).
read_tokens(0'(, [NextCh|String], Dict, [' ('|Tokens]) :- !,
    read_tokens(NextCh, String, Dict, Tokens).
read_tokens(0'), [NextCh|String], Dict, [')'|Tokens]) :- !,
    read_tokens(NextCh, String, Dict, Tokens).
read_tokens(0',, [NextCh|String], Dict, [','|Tokens]) :- !,
    read_tokens(NextCh, String, Dict, Tokens).
read_tokens(0';, [NextCh|String], Dict, [atom(;)|Tokens]) :- !,
    read_after_atom(NextCh, String, Dict, Tokens).
read_tokens(0'[, [NextCh|String], Dict, ['['|Tokens]) :- !,
    read_tokens(NextCh, String, Dict, Tokens).
read_tokens(0'], [NextCh|String], Dict, [']'|Tokens]) :- !,
    read_after_atom(NextCh, String, Dict, Tokens).
read_tokens(0'{, [NextCh|String], Dict, ['{'|Tokens]) :- !,
    read_tokens(NextCh, String, Dict, Tokens).
read_tokens(0'|, [NextCh|String], Dict, ['|'|Tokens]) :- !,
    read_tokens(NextCh, String, Dict, Tokens).
read_tokens(0'}, [NextCh|String], Dict, ['}'|Tokens]) :- !,
    read_after_atom(NextCh, String, Dict, Tokens).
read_tokens(0'., [NextCh|String], Dict, Tokens) :- !,
    read_fullstop(NextCh, String, Dict, Tokens).
read_tokens(0'", S0, Dict, [string(S)|Tokens]) :- !,
    read_string(S, 0'", NextCh, S0, S1),
    read_tokens(NextCh, S1, Dict, Tokens).
read_tokens(0'', S0, Dict, [atom(A)|Tokens]) :- !,
    read_string(S, 0'', NextCh, S0, S1),
    name(A, S),
    read_after_atom(NextCh, S1, Dict, Tokens).
read_tokens(Ch, S0, Dict, [var(Var,Name)|Tokens]) :-
    (   Ch =< "Z", Ch >= "A" -> true
    ;   Ch =:= "_"
    ),
    !,
    read_name(Ch, S, NextCh, S0, S1),
    (   S = "_" -> Name = '_'
    ;   name(Name, S),
        read_lookup(Dict, Name=Var), true
    ),
    read_after_atom(NextCh, S1, Dict, Tokens).
read_tokens(Ch, S0, Dict, [number(I)|Tokens]) :-
    Ch >= "0", Ch =< "9",   
    !,
    read_digits(Ch, 0, 10, I, NextCh, S0, S1),
    read_tokens(NextCh, S1, Dict, Tokens).
read_tokens(Ch, S0, Dict, [atom(A)|Tokens]) :-
    (   Ch =< "z", Ch >= "a" -> true
    ;   Ch =:= "$"
    ),
    !,
    read_name(Ch, S, NextCh, S0, S1),
    name(A, S),
    read_after_atom(NextCh, S1, Dict, Tokens).
read_tokens(Ch, [AnotherCh|S0], Dict, [atom(A)|Tokens]) :-
    read_symbol(AnotherCh, Chars, NextCh, S0, S1),
    name(A, [Ch|Chars]),
    read_after_atom(NextCh, S1, Dict, Tokens).



%   The only difference between read_after_atom(Ch, Dict, Tokens) and
%   read_tokens/3 is what they do when Ch is "(".  read_after_atom
%   finds the token to be '(', while read_tokens finds the token to be
%   ' ('.  This is how the parser can tell whether <atom> <paren> must
%   be an operator application or an ordinary function symbol application.
%   See the public-domain library file READ.PL for details.

read_after_atom(0'(, [NextCh|String], Dict, ['('|Tokens]) :- !,
    read_tokens(NextCh, String, Dict, Tokens).
read_after_atom(Ch, String, Dict, Tokens) :-
    read_tokens(Ch, String, Dict, Tokens).




%   read_string(Chars, Quote, NextCh, String)
%   reads the body of a string delimited by Quote characters.
%   The result is a list of ASCII codes.  There are two complications.
%   If we hit the end of the file inside the string this predicate FAILS.
%   It does not return any special structure.  That is the only reason
%   it can ever fail.  The other complication is that when we find a Quote
%   we have to look ahead one character in case it is doubled.  Note that
%   if we find an end-of-file after the quote we *don't* fail, we return
%   a normal string and the end of file character is returned as NextCh.
%   If we were going to accept C-like escape characters, as I think we
%   should, this would need changing (as would the code for 0'x).  But
%   the purpose of this module is not to present my ideal syntax but to
%   present something which will read present-day Prolog programs.
%
%   it reads from the list "String"

read_string(Chars, Quote, NextCh, [Ch|S0], S2) :-
    read_char(Ch, Quote, Char, Next, S0, S1),
    rest_string(Char, Next, Chars, Quote, NextCh, S1, S2).


rest_string(-1,   NextCh, [],     _, NextCh, S, S) :- !. % string ended
%rest_string(Char, NextCh, [Char], _, NextCh, S, S) :- !. % end of string
rest_string(Char, Next, [Char|Chars], Quote, NextCh, S0, S2) :-
    read_char(Next, Quote, Char2, Next2, S0, S1),
    rest_string(Char2, Next2, Chars, Quote, NextCh, S1, S2).


%   read_char(C1, Quote, Char, C2, Sin, Sout)
%   reads a single `character' from a string, quoted atom, or character
%   constant.  C1 is the first character it is to look at, and has been
%   read already.  Quote is the surrounding quotation mark, which is "
%   for strings, ' for quoted atoms, and the radix character (also ')
%   for character constants.  A Dec-10 Prolog incompatibility is that
%   it does not allow newline characters in strings unless they are
%   preceded by an escape character.  As reading an extended character
%   would sometimes read one character too many, it is made to do so
%   always, and to return the first character which does not belong in
%   the character as C2.  When we have hit the end of the string, we
%   return Char = -1 (which does not necessarily mean that we have hit
%   the end of the source file, look at C2 for that).
%
%   note that 92 == '\'
%         94 == '^'

read_char(92, Quote, -1, -1, [], []) :- !,
    format(user_error, '~N** end of file in ~cquoted~c~n', [Quote,Quote]).
read_char(92, Quote, Result, Next, [C1|S0], Sn) :-
    C1 =< " ", !,
    S0 = [C2|S1],
    read_char(C2, Quote, Result, Next, S1, Sn).
read_char(92, _Quote, Result, Next, [C1,C2,C3,Next|S], S) :-
    C3 =< "7", C3 >= "0", C2 =< "7", C2 >= "0", C1 =< "7", C1 >= "0", !,
    Result is (C1*8+C2)*8+C3 - 73*"0".
read_char(92, _Quote, Result, Next, [C1,C2,Next|S], S) :-
    C2 =< "7", C2 >= "0", C1 =< "7", C1 >= "0", !,
    Result is C1*8+C2 - 9*"0".
read_char(92, _Quote, Result, Next, [C1,Next|S], S) :-
    C1 =< "7", C1 >= "0", !,
    Result is C1 - "0".
read_char(92, _Quote, Result, Next, [94,X,Next|S], S) :- !,
%???    Result is X/\31.
    Result is X mod 32.
read_char(92, _Quote, Result, Next, [C1,Next|S], S) :-
    escape_char(C1, Result), !.
read_char(92, _Quote, C1, Next, [C1,Next|S], S) :- !.
read_char(Quote, Quote, Quote, Next, [Quote,Next|S], S) :- !.
read_char(Quote, Quote, -1, Next, [Next|S], S) :- !.
read_char(Char, Quote, -1, Char, S, S) :-
    Char < " ", Char =\= 9 /*TAB*/, !,
    format(user_error,
        '~N** Strange character ~d ends ~ctoken~c~n',
        [Char, Quote, Quote]).
read_char(Char, _Quote, Char, Next, [Next|S], S).



%  This table is for ASCII.  On Xerox Lisp systems, \n maps to
%  13 (CR).  The whole table needs replacing in EBCDIC systems,
%  in which the assumption that A..Z and a..z are contiguous
%  blocks also needs correcting.

escape_char(0'n, 10).       % \n = NewLine
escape_char(0'N, 10).       % \N = NewLine
escape_char(0't,  9).       % \t = Tab
escape_char(0'T,  9).       % \T = Tab
escape_char(0'r, 13).       % \r = Return
escape_char(0'R, 13).       % \R = Return
escape_char(0'v, 11).       % \v = Vertical tab
escape_char(0'V, 11).       % \V = Vertical tab
escape_char(0'b,  8).       % \b = Backspace
escape_char(0'B,  8).       % \B = Backspace
escape_char(0'f, 12).       % \f = FormFeed
escape_char(0'F, 12).       % \F = FormFeed
escape_char(0'e, 27).       % \e = Escape
escape_char(0'E, 27).       % \E = Escape
escape_char(0'd,127).       % \d = Delete
escape_char(0'D,127).       % \D = Delete
escape_char(0's, 32).       % \s = visible Space
escape_char(0'S, 32).       % \S = visible Space
escape_char(0'z, -1).       % \z = end of file
escape_char(0'Z, -1).       % \Z = end of file



%   read_solidus(Ch, String, Dict, Tokens)
%   checks to see whether /Ch is a /* comment or a symbol.  If the
%   former, it skips the comment.  If the latter it just calls read_symbol.
%   We have to take great care with /* comments to handle end of file
%   inside a comment, which is why read_solidus/2 passes back an end of
%   file character or a (forged) blank that we can give to read_tokens.

read_solidus(0'*, [Ch|S0], Dict, Tokens) :- !,
    read_solidus(Ch, NextCh, S0, S1),
    read_tokens(NextCh, S1, Dict, Tokens).
read_solidus(Ch, S0, Dict, [atom(A)|Tokens]) :-
    read_symbol(Ch, Chars, NextCh, S0, S1), % might read 0 chars
    name(A, [0'/|Chars]),
    read_after_atom(NextCh, S1, Dict, Tokens).

read_solidus(-1, -1, [], []) :- !,
    format(user_error, '~N** end of file in /*comment~n', []).
read_solidus(0'*, LastCh, [NextCh|S0], S1) :- !,
    (   NextCh =:= 0'/ ->   %  end of comment*/ found
        LastCh is " "   % /*comment*/s act like spaces
    ;   read_solidus(NextCh, LastCh, S0, S1)
    ).
read_solidus(_, LastCh, [NextCh|S0], S1) :-
    read_solidus(NextCh, LastCh, S0, S1).


%   read_name(Char, String, LastCh)
%   reads a sequence of letters, digits, and underscores, and returns
%   them as String.  The first character which cannot join this sequence
%   is returned as LastCh.

read_name(Char, [Char|Chars], LastCh, [NextCh|S0], S1) :-
    (   Char >= "a", Char =< "z"
    ;   Char >= "A", Char =< "Z"
    ;   Char >= "0", Char =< "9"
    ;   Char=:= "_"         % _
    ;   Char=:= "$"         % _
    ), !,
    read_name(NextCh, Chars, LastCh, S0, S1).
read_name(LastCh, [], LastCh, S, S).


%   read_symbol(Ch, String, NextCh)
%   reads the other kind of atom which needs no quoting: one which is
%   a string of "symbol" characters.  Note that it may accept 0
%   characters, this happens when called from read_fullstop.

read_symbol(Char, [Char|Chars], LastCh, [NextCh|S0], S1) :-
    symbol_char(Char),
    !,
    read_symbol(NextCh, Chars, LastCh, S0, S1).
read_symbol(LastCh, [], LastCh, S, S).

symbol_char(0'#).
symbol_char(0'$).
symbol_char(0'&).
symbol_char(0'*).
symbol_char(0'+).
symbol_char(0'-).
symbol_char(0'.).   % yes, +./* is a legal atom
symbol_char(0'/).
symbol_char(0':).
symbol_char(0'<).
symbol_char(0'=).
symbol_char(0'>).
symbol_char(0'?).
symbol_char(0'@).
symbol_char(92 /* \ */).
symbol_char(0'^).
symbol_char(0'`).   % CHAT-80 uses `` as an atom.
symbol_char(0'~).


%   read_fullstop(Char, Dict, Tokens)
%   looks at the next character after a full stop.  There are
%   three cases:
%   (a) the next character is an end of file.  We treat this
%       as an unexpected end of file.  The reason for this is
%       that we HAVE to handle end of file characters in this
%       module or they are gone forever; if we failed to check
%       for end of file here and just accepted .<EOF> like .<NL>
%       the caller would have no way of detecting an end of file
%       and the next call would abort.
%   (b) the next character is a layout character.  This is a
%       clause terminator.
%   (c) the next character is anything else.  This is just an
%       ordinary symbol and we call read_symbol to process it.

read_fullstop(-1, [], _, _) :- !,
    format(user_error, '~N** end of file just after full stop~n', []),
    fail.
read_fullstop(Ch, _, _, []) :-
    Ch =< " ", !.       % END OF CLAUSE
read_fullstop(Ch, S0, Dict, [atom(A)|Tokens]) :-
    read_symbol(Ch, S, NextCh, S0, S1),
    name(A, [0'.|S]),
    read_after_atom(NextCh, S1, Dict, Tokens).



%   read_digits is complicated by having to understand radix notation.
%   There are three forms of integer:
%   0 ' <any character> - the ASCII code for that character
%   <digit> ' <digits>  - the digits, read in that base
%   <digits>        - the digits, read in base 10.
%   The X =\= -1 tests are to make sure we don't miss an end of file
%   character.  If we hit the end of the file inside an integer,
%   read_digits/4 will quietly fail, leaving read_tokens to return
%   end of file.  In such cases we ought to report an error.
%
%   2nd argument now indicates the state we're in wrt floating point
%   number parsing.
%   nothing     - haven't seen anything to indicate this is a float
%             but it's still allowed
%   none        - no floating stuff allowed
%   float(N)    - have seen the fraction, N is negative power of 10
%   exp(N)      - have seen the exponent, N is negative power of 10

read_digits(Ch, SoFar, Base, Value, NextCh, S0, S1) :-
    read_digits_(Ch, nothing, SoFar, Base, Value, NextCh, S0, S1).

read_digits_(SoFar, Float, Base, Value, NextCh, [Ch|S0], S1) :-
    Ch =\= -1,
    read_digits_(Ch, Float, SoFar, Base, Value, NextCh, S0, S1).

read_digits_(0'', nothing, SoFar, _, IntVal, NextCh, S0, Sn) :- !,
    (   SoFar =:= 0 ->
        S0=[Ch|S1],
        read_char(Ch, -1, IntVal, NextCh, S1, Sn)
    ;   SoFar >= 2, SoFar =< 16 ->
        read_digits_(0, nothing, SoFar, IntVal, NextCh, S0, Sn)
    ;   format(user_error, '~N** ~d'' read as ~d ''~n', [SoFar,SoFar]),
        IntVal = SoFar, NextCh is "'",
        Sn=S1
    ).
read_digits_(0'_, Float, SoFar, Base, Value, NextCh, S0, S1) :- !,
    read_digits_(SoFar, Float, Base, Value, NextCh, S0, S1).
read_digits_(0'., nothing, SoFar, Base, Value, NextCh, [C|S0], S1) :-
    C >= "0", C =< "9",         % check for floating point
    !,                  % only allowed w/ float=nothing
    FSoFar is float(SoFar),         % must because of ARGH!
    read_digits_(C, float(0), FSoFar, Base, Value, NextCh, S0, S1).
read_digits_(Exp, float(N), SoFar, Base, Value, NextCh, [0'-,C|S0], S1) :-
    ( Exp =:= 0'e ; Exp =:= 0'E ),      % Is this it?
    C >= "0", C =< "9",         % check for exponent
    !,                  % only allowed w/ float(N)
    read_digits_(C, none, 0, Base, EValue, NextCh, S0, S1),
    !,
    Expo is EValue+N,
    compute_power(Expo, Base, Factor),
    Value is SoFar / Factor.
read_digits_(Exp, float(N), SoFar, Base, Value, NextCh, [0'+,C|S0], S1) :-
    ( Exp =:= 0'e ; Exp =:= 0'E ),      % Is this it?
    C >= "0", C =< "9",         % check for exponent
    !,                  % only allowed w/ float(N)
    read_digits_(C, none, 0, Base, EValue, NextCh, S0, S1),
    !,
    Expo is EValue-N,
    compute_power(Expo, Base, Factor),
    Value is SoFar * Factor.
read_digits_(Exp, float(N), SoFar, Base, Value, NextCh, [C|S0], S1) :-
    ( Exp =:= 0'e ; Exp =:= 0'E ),      % Is this it?
    C >= "0", C =< "9",         % check for exponent
    !,                  % only allowed w/ float(N)
    read_digits_(C, none, 0, Base, EValue, NextCh, S0, S1),
    !,
    Expo is EValue-N,
    compute_power(Expo, Base, Factor),
    Value is SoFar * Factor.
read_digits_(Ch, Float, SoFar, Base, Value, NextCh, S0, S1) :-
    (   Ch >= "0", Ch =< "9" -> Digit is Ch-"0"
    ;   Ch >= "a", Ch =< "z" -> Digit is Ch-("a"-10)
    ;   Ch >= "A", Ch =< "Z" -> Digit is Ch-("A"-10)
    ),
    Digit < Base,
    !,
    advance_digits(Float, Digit, SoFar, Base, Next, NextFloat),
    read_digits_(Next, NextFloat, Base, Value, NextCh, S0, S1).
read_digits_(LastCh, float(N), SoFar, Base, Value, LastCh, S, S) :- !,
    Exp is N,
    compute_power(Exp, Base, Power),
    Value is float(SoFar) / Power.
read_digits_(LastCh, _Float, Value, _Base, Value, LastCh, S, S).


compute_power(Power, Base, Value) :-
    Power < 0, !,
    NPower is -Power,
    compute_power_(NPower, Base, IValue),
    Value is 1/float(IValue).
compute_power(Power, Base, Value) :-
    compute_power_(Power, Base, IValue),
    Value is float(IValue).


compute_power_(Power, _Base, 1) :- Power =< 0, !.
compute_power_(Power, Base, Value) :-
    P1 is Power-1,
    compute_power(P1, Base, V1),
    Value is Base*V1.



advance_digits(float(N), Digit, SoFar, Base, Next, float(N1)) :- !,
    N1 is N+1,
    Next is SoFar*Base+Digit.
advance_digits(Float, Digit, SoFar, Base, Next, Float) :-
    Next is SoFar*Base+Digit.


%   read_lookup is identical to memberchk except for argument order and
%   mode declaration.

read_lookup([X|_], X) :- !.
read_lookup([_|T], X) :-
    read_lookup(T, X). 


