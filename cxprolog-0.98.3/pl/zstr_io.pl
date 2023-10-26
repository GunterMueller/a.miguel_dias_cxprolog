% $Id: str_io.pl,v 1.3 90/09/27 21:27:45 sources Exp $

% =============================================================================
%
% $Log:	str_io.pl,v $
% Revision 1.3  90/09/27  21:27:45  sources
% Made this more adapted to the Multi-Prolog scheme (eg using predicate
% `prolog_type/1'), and made it use the new string tokenizer.
% 
% Revision 1.2  90/04/23  20:43:10  spa
% Now using RCS `Id:' instead of `Header:', as it's more compact.
% 
% Revision 1.1  90/02/14  13:22:57  spa
% Initial revision
% 

%   File   : str_io.pl
%   Authors: David H. D. Warren + Richard A. O'Keefe
%   Updated: 11/2/88
%   Purpose: Read Prolog terms in Dec-10 syntax.

/*  This code was originally written at the University of Edinburgh.
    David H. D. Warren wrote the first version of the parser.
    Richard A. O'Keefe ripped it out of the Dec-10 Prolog system
    and made it use only user-visible operations.  He also added
    the feature whereby P(X,Y,Z) is read as call(P,X,Y,Z).
    Alan Mycroft reorganised the code to regularise the functor modes.
    This is both easier to understand (there are no more '?'s),
    and also fixes bugs concerning the curious interaction of cut with
    the state of parameter instantiation.
    Richard A. O'Keefe then took it over again and made a number of
    other changes.  There are three intentional differences between
    this and the Dec-10 Prolog parser:

	"predicate variables" as syntactic saccharine for call/N

	when there is a syntax error, DEC-10 Prolog will backtrack
	internally and read the next term.  This fails.  If you
	call portable_read/1 with an uninstantiated argument, 
	failure means a syntax error.  You can rely on it.

	", .." is not accepted in place of "|".  This was always a
	parser feature, not a tokeniser feature:  any amount of
	layout and commentary was allowed between the "," and the
	"..".  It wouldn't be hard to allow again.

    The tokeniser returns a list of the following tokens:

	var(_,Name)		a variable with name Name
	atom(AnAtom)		an atom
	number(Num)		an integer or float
	string(Chars)		a list of character codes

	'{' '[' '(' ' ('	punctuation marks
	'}' ']' '('		' (' is the usual "(", '(' is "("
	    '|' ','		coming directly after an atom.


:- prolog_type(quintus) ->
   module(str_io, [str_read/2,		% read a term from a string
	str_read/3		% same but with variable bindings
   ]) ; true.

:- prolog_type(quintus) ->
   use_module(tokens, [read_token_list/3]) ; true.
*/

%   str_read(+String, ?Answer)
%   reads a term from the current input stream and unifies it with Answer.
%   Dec-10 syntax is used.  If a syntax error is found, the parser FAILS.

str_read(String, Answer) :-
    str_read(String, Answer, _).


%   str_read(+String, ?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.
%   The variables are listed in the order in which they appeared, but
%   anonymous variables `_' do not appear at all.

str_read(String, Answer, Variables) :-
	append_u(String, ". ", NString),
	read_token_list(NString, Tokens, Variables),
	(   read(Tokens, 1200, Term, LeftOver) -> Answer = Term
	;   syntax_error(Tokens)
	).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax_error([Token,or,operator,expected], S0).


%   ambigop(Symbol, Precedence, L1, O1, R1, L2, O2)
%   is true when Symbol has an infix (L1,O1,R1) and a postfix (L2,O2)
%   definition both of which are compatible with Precedence.
%   I assume here that postfixop and infixop have been coded so that
%   they are determinate when called with a constant first argument.

ambigop(F, Precedence, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	O2 =< Precedence,
	infixop(F, L1, O1, R1),
	O1 =< Precedence.


%   read(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

read([Token|RestTokens], Precedence, Term, LeftOver) :-
	read(Token, RestTokens, Precedence, Term, LeftOver).
read([], _, _, _) :-
	syntax_error([expression,expected], []).


%   read(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

read('}', S0, _, _, _) :- re1('}', S0).
read(']', S0, _, _, _) :- re1(']', S0).
read(')', S0, _, _, _) :- re1(')', S0).
read(',', S0, _, _, _) :- re1(',', S0).
read('|', S0, _, _, _) :- re1('|', S0).

read(string(Chars), S0, Precedence, Answer, S) :-
	exprtl0(S0, Chars, Precedence, Answer, S).

read(number(Number), S0, Precedence, Answer, S) :-
	exprtl0(S0, Number, Precedence, Answer, S).

read('[', [']'|S1], Precedence, Answer, S) :- !,
	read_atom([], S1, Precedence, Answer, S).
read('[', S1, Precedence, Answer, S) :-
	read(S1, 999, Arg1, S2),		/* look for ",", "|", or "]" */
	read_list(S2, RestArgs, S3),
	!,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).

read('(', S1, Precedence, Answer, S) :-
	read(S1, 1200, Term, S2),		/* look for ")" */
	expect(')', S2, S3),
	!,
	exprtl0(S3, Term, Precedence, Answer, S).

read(' (', S1, Precedence, Answer, S) :-
	read(S1, 1200, Term, S2),		/* look for ")" */
	expect(')', S2, S3),
	!,
	exprtl0(S3, Term, Precedence, Answer, S).

read('{', ['}'|S1], Precedence, Answer, S) :- !,
	read_atom('{}', S1, Precedence, Answer, S).

read('{', S1, Precedence, Answer, S) :-
	read(S1, 1200, Term, S2),		/* look for "}" */
	expect('}', S2, S3),
	!,
	exprtl0(S3, '{}'(Term), Precedence, Answer, S).

read(var(Variable,_), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),		% look for "," or ")"
	read_args(S2, RestArgs, S3),
	!,
	Term =.. [call,Variable,Arg1|RestArgs],
	exprtl0(S3, Term, Precedence, Answer, S).

read(var(Variable,_), S0, Precedence, Answer, S) :-
	exprtl0(S0, Variable, Precedence, Answer, S).

read(atom(Atom), S0, Precedence, Answer, S) :-
	read_atom(Atom, S0, Precedence, Answer, S).


read_atom(-, [number(Number)|S1], Precedence, Answer, S) :- !,
	Negative is -Number,
	exprtl0(S1, Negative, Precedence, Answer, S).
read_atom(Functor, ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),		% look for "," or ")"
	read_args(S2, RestArgs, S3),
	!,
	Term =.. [Functor,Arg1|RestArgs],
	exprtl0(S3, Term, Precedence, Answer, S).
read_atom(Functor, S0, Precedence, Answer, S) :-
	prefixop(Functor, Prec, Right),
	!,
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).
read_atom(Atom, S0, Precedence, Answer, S) :-
	exprtl0(S0, Atom, Precedence, Answer, S).


re1(Token, S0) :-
	syntax_error([Token,cannot,start,an,expression], S0).


%   read_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

read_args([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2),
	!,
	read_args(S2, Rest, S).
read_args([')'|S], [], S) :- !.
read_args(S, _, _) :-
	syntax_error([', or )',expected,in,arguments], S).


%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list of terms.

read_list([], _, _) :-
	syntax_error([', | or ]',expected,in,list], []).
read_list([Token|S1], Rest, S) :-
	read_list(Token, S1, Rest, S).

read_list(',', S1, [Term|Rest], S) :- !,
	read(S1, 999, Term, S2),
	!,
	read_list(S2, Rest, S).
read_list('|', S1, Rest, S) :- !,
	read(S1, 999, Rest, S2),
	!,
	expect(']', S2, S).
read_list(']', S1, [], S1) :- !.
read_list(Token, S1, _, _) :-
	syntax_error([', | or ]',expected,in,list], [Token|S1]).


%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, -Ans, -LeftOver)

after_prefix_op(Op, Oprec, _, S0, Precedence, _, _) :-
	Precedence < Oprec,
	!,
	syntax_error([prefix,operator,Op,in,context,with,precedence,Precedence], S0).

after_prefix_op(Op, Oprec, _, S0, Precedence, Answer, S) :-
	peepop(S0, S1),
	prefix_is_atom(S1, Oprec), % can't cut but would like to
	exprtl(S1, Oprec, Op, Precedence, Answer, S).

after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	read(S1, Aprec, Arg, S2),
	Term =.. [Op,Arg],
	!,
	exprtl(S2, Oprec, Term, Precedence, Answer, S).


%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :-
	prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	ambigop(F, Precedence, L1, O1, R1, L2, O2),
	!,
	(   prefix_is_atom(S1, Precedence),
	    !,
	    exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S)
	;   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S)
	;   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S)
	).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	infixop(F, L1, O1, R1),
	!,
	exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	postfixop(F, L2, O2),
	!,
	exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).

exprtl0([','|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1000,
	!,
	read(S1, 1000, Next, S2),
	!,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1100,
	!,
	read(S1, 1100, Next, S2),
	!,
	exprtl(S2, 1100, (Term ; Next), Precedence, Answer, S).

exprtl0([atom(X)|S1], _, _, _, _) :- !,
	syntax_error([non-operator,X,follows,expression], [atom(X)|S1]).
exprtl0([Thing|S1], _, _, _, _) :-
	cant_follow_expr(Thing, Culprit),
	!,
	syntax_error([Culprit,follows,expression], [Thing|S1]).

exprtl0(S, Term, _, Term, S).


cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(number(_),	number).
cant_follow_expr(string(_),	string).
cant_follow_expr(' (',		bracket).
cant_follow_expr('(',		bracket).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		bracket).



exprtl([infixop(F,L,O,R)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L,
	!,
	read(S1, R, Other, S2),
	Expr =.. [F,Term,Other], /*!,*/
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([postfixop(F,L,O)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L,
	!,
	Expr =.. [F,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([','|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1000, C < 1000,
	!,
	read(S1, 1000, Next, S2), /*!,*/
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1100, C < 1100,
	!,
	read(S1, 1100, Next, S2), /*!,*/
	exprtl(S2, 1100, (Term ; Next), Precedence, Answer, S).

exprtl(S, _, Term, _, Term, S).


%   This business of syntax errors is tricky.  When an error is detected,
%   we have to write out a message.  We also have to note how far it was
%   to the end of the input, and for this we are obliged to use the data-
%   base.  Then we fail all the way back to read(), and that prints the
%   input list with a marker where the error was noticed.  If subgoal_of
%   were available in compiled code we could use that to find the input
%   list without hacking the data base.  The really hairy thing is that
%   the original code noted a possible error and backtracked on, so that
%   what looked at first sight like an error sometimes turned out to be
%   a wrong decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no backtracking
%   at all.  This goal has not yet been met, and it will still occasionally
%   report an error message and then decide that it is happy with the input
%   after all.  Sorry about that.


syntax_error(Message, List) :-
%	telling(CurrentOutput),
%	tell(user),
%	nl, write('**'), display_list(Message),
%	tell(CurrentOutput),
%	length(List, Length),
%	recorda(syntax_error, length(Length), _),
	!, fail.

display_list([]) :-
	nl.
display_list([Head|Tail]) :- !,
	put(" "),
	display_token(Head),
	display_list(Tail).

syntax_error(List) :-
%	recorded(syntax_error, length(AfterError), Ref),
%	erase(Ref),
%	length(List, Length),
%	BeforeError is Length-AfterError,
%	telling(CurrentOutput),
%	tell(user),
%	display_list(List, BeforeError),
%	tell(CurrentOutput),
	!, fail.

display_list(X, 0) :- !,
	write('<<here>> '),
	display_list(X, 99999).
display_list([Head|Tail], BeforeError) :- !,
	display_token(Head),
	put(32),
	Left is BeforeError-1,
	display_list(Tail, Left).
display_list([], _) :-
	nl.

display_token(atom(X))	 :- !,	writeq(X).
display_token(var(_,X))	 :- !,	write(X).
display_token(number(X)) :- !,	write(X).
display_token(string(X)) :- !,	put(34), display_string(X, 34).
display_token(X)	 :-	write(X).

display_string([], Quote) :-
	put(Quote).
display_string([Quote|Chars], Quote) :- !,
	put(Quote), put(Quote),		% later, put(92), put(Quote)
	display_string(Chars, Quote).
display_string([Char|Chars], Quote) :-
	put(Char),
	display_string(Chars, Quote).


%.  The original public-domain code was written to go with a similarly
%   public-domain version of op/3 and current_op/3 where the following
%   three tables were the primary reality.  Whether they are or aren't,
%   only current_op/3 is (currently) directly available to customers.

prefixop(F, O, Q) :-
	(   current_op(O, fx, F) -> Q is O-1
	;   current_op(O, fy, F) -> Q is O
	; fail
	).

postfixop(F, P, O) :-
	(   current_op(O, xf, F) -> P is O-1
	;   current_op(O, yf, F) -> P is O
	; fail
	).

infixop(F, P, O, Q) :-
	(   current_op(O, xfy, F) -> P is O-1, Q is O
	;   current_op(O, xfx, F) -> P is O-1, Q is P
	;   current_op(O, yfx, F) -> Q is O-1, P is O
	; fail
	).

