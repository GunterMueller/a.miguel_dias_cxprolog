% LPi interpreter - AMD Jan/2001

%oneOf(-term,+bag,-bag).		Member of bag.
oneOf(X,[X|Xs],Xs).
oneOf(X,[Y|Xs],[Y|Rs]) :- oneOf(X,Xs,Rs).

%alpha(+term,-term).			Refreshes all binding ocurrences in agent.
alpha(new(V,P),new(Vn,Pn)) :- !,
	alpha(P,Pi),
	copy_term([V], new(V,Pi), new(Vn,Pn)).
alpha(comm(V,A,G,P),comm(Vn,An,Gn,Pn)) :- !,
	alpha(A,Ai), alpha(G,Gi), alpha(P,Pi), 
	copy_term(V, comm(V,Ai,Gi,Pi), comm(Vn,An,Gn,Pn)).
alpha(def(V,A,G,P),def(Vn,An,Gn,Pn)) :- !,
	alpha(A,Ai), alpha(G,Gi), alpha(P,Pi), 
	copy_term(V, def(V,Ai,Gi,Pi), def(Vn,An,Gn,Pn)).
alpha([T|Ts],[Tn|Tsn]) :- !,
	alpha(T,Tn), alpha(Ts,Tsn).
alpha(M,M).

%defs(+bag,-bag).				Gets all defs from agent.
defs([],[]).
defs([def(V,A,G,P)|Ds], [def(V,A,G,P)|Rs]) :- !, defs(Ds,Rs).
defs([_|Ds], Rs) :- defs(Ds,Rs).

%unique(-term).					Generates unique term.
unique('$u'(X)) :- u ?:= 0, u =: X, Y is X+1, u := Y.

%match(+bag,+bag,-bag).			Matches bag of input patterns against agent.
match([],Ms,Ms).
match([A|As],Ms,Rs) :- oneOf(A,Ms,Mi), match(As,Mi,Rs).

%sucessState(+bag).				Checks success state.
sucessState([]).
sucessState([def(_,_,_,_)|Ds]) :- sucessState(Ds).

%reduceToSucess(+bag).			Reduces agent 'til success.
reduceToSucess(Ts) :- sucessState(Ts), !.
reduceToSucess(Ts) :- reduce(Ts,Rs), sucessState(Rs).

%reduce(+bag,-bag).				Reduces composite agent.
reduce(Ts,Rs) :- oneOf(T,Ts,Tother), reduceOne(T,Tother,Rs).

%reduceOne(+term,+bag,-bag).	Reduces elementary agent.
reduceOne(new(V0,P0),Ts,Rs) :-
	alpha(new(V0,P0), new(V,P)),
	unique(V),
	app(P,Ts,Rs).
reduceOne(comm(V0,A0,G0,P0),Ts,Rs) :-
	alpha(comm(V0,A0,G0,P0), comm(V,A,G,P)),
	match(A,Ts,Tother),
	defs(Tother,D), app(D,G,Tnew), reduceToSucess(Tnew),
	app(Tother,P,Rs).
reduceOne(def(V,A,G,P),Ts,Rs) :-
	reduceOne(comm(V,A,G,P),[def(V,A,G,P)|Ts],Rs).

%run(+bag,-bag).				Runs agent, showing intermediate steps.
run(Ts) :- reduce(Ts,Rs), writeln(Rs), get0(_), run(Rs).

/*
TESTING...

[main] ?- alpha(new(V,
                    [mesg(a,V,V),
                     new(V,[mesg(a,V,V)]),
                     comm([V,X],[new(V,[mesg(a,V,X)])],[mesg(a,V,X)],[mesg(b)]),
                     def([V],[mesg(a,V,V)],[mesg(a,V,V)],[mesg(a,V,V)])
                    ]),Q).
Q=new(_125,
     [mesg(a,_125,_125),
      new(_132,[mesg(a,_132,_132)]),
      comm([_203,_205],[new(_163,[mesg(a,_163,_205)])],[mesg(a,_203,_205)],[mesg(b)]),
      def([_258],[mesg(a,_258,_258)],[mesg(a,_258,_258)],[mesg(a,_258,_258)])
     ])
X=_36
V=_20;
no
[main] ?- oneOf(mesg(a,X,Y), [mesg(a,1,2),mesg(a,3,4),mesg(b,5,6)], Q).
Q=[mesg(a,3,4),mesg(b,5,6)]
Y=2
X=1;
Q=[mesg(a,1,2),mesg(b,5,6)]
Y=4
X=3;
no
[main] ?- match([mesg(a,X,Y),mesg(b,1),mesg(c,X)],
                [mesg(a,1,2),mesg(a,3,4),mesg(b,1),mesg(c,1),mesg(c,2),mesg(c,3),mesg(c,4)],
                Q).
Q=[mesg(a,3,4),mesg(c,2),mesg(c,3),mesg(c,4)]
Y=2
X=1;
Q=[mesg(a,1,2),mesg(c,1),mesg(c,2),mesg(c,4)]
Y=4
X=3;
no
[main] ?- reduce([def([],[],[],[a])], Q).
Q=[def([],[],[],[a]),a];
no
[main] ?- reduce([comm([X,Y],[mesg(a,X,Y)],[],[mesg(b,X,Y)]),
				  mesg(a,1,2),
				  mesg(a,3,4)],
				Q).
Q=[mesg(a,3,4),mesg(b,1,2)]
Y=_21
X=_20;
Q=[mesg(a,1,2),mesg(b,3,4)]
Y=_21
X=_20;
no
[main] ?- reduce([def([X,Y],[mesg(a,X,Y)],[],[mesg(b,X,Y)]),
				  mesg(a,1,2),
				  mesg(a,3,4)],
				Q).
Q=[def([_20,_21],[mesg(a,_20,_21)],[],[mesg(b,_20,_21)]),mesg(a,3,4),mesg(b,1,2)]
Y=_21
X=_20;
Q=[def([_20,_21],[mesg(a,_20,_21)],[],[mesg(b,_20,_21)]),mesg(a,1,2),mesg(b,3,4)]
Y=_21
X=_20;
no
[main] ?- reduce([
				def([Xs],[mesg(app,[],Xs,Xs)],[],[]),
				def([X,Xs,Ys,Rs],[mesg(app,[X|Xs],Ys,[X|Rs])],[mesg(app,Xs,Ys,Rs)],[]),
				mesg(app,[1,2,3],[4,5,6],Z)
			], Q).
Q=[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[mesg(app,_20,_36,_37)],[]),
   def([_20],[mesg(app,[],_20,_20)],[],[])
  ]
Z=[1,2,3,4,5,6]
Rs=_37
Ys=_36
X=_35
Xs=_20;
no
[main] ?- reduce([
				def([Xs],[mesg(app,[],Xs,Xs)],[],[]),
				def([X,Xs,Ys,Rs],[mesg(app,[X|Xs],Ys,[X|Rs])],[mesg(app,Xs,Ys,Rs)],[]),
				mesg(app,A,B,[1,2,3,4,5,6])
			], Q).
Q=[def([_20],[mesg(app,[],_20,_20)],[],[]),
   def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[mesg(app,_20,_36,_37)],[])
  ]
B=[1,2,3,4,5,6]
A=[]
Rs=_37
Ys=_36
X=_35
Xs=_20;
Q=[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[mesg(app,_20,_36,_37)],[]),
   def([_20],[mesg(app,[],_20,_20)],[],[])
  ]
B=[]
A=[1,2,3,4,5,6]
Rs=_37
Ys=_36
X=_35
Xs=_20;
Q=[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[mesg(app,_20,_36,_37)],[]),
   def([_20],[mesg(app,[],_20,_20)],[],[])
  ]
B=[6]
A=[1,2,3,4,5]
Rs=_37
Ys=_36
X=_35
Xs=_20;
Q=[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[mesg(app,_20,_36,_37)],[]),
   def([_20],[mesg(app,[],_20,_20)],[],[])
  ]
B=[5,6]
A=[1,2,3,4]
Rs=_37
Ys=_36
X=_35
Xs=_20;
Q=[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[mesg(app,_20,_36,_37)],[]),
   def([_20],[mesg(app,[],_20,_20)],[],[])
  ]
B=[4,5,6]
A=[1,2,3]
Rs=_37
Ys=_36
X=_35
Xs=_20;
Q=[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[mesg(app,_20,_36,_37)],[]),
   def([_20],[mesg(app,[],_20,_20)],[],[])
  ]
B=[3,4,5,6]
A=[1,2]
Rs=_37
Ys=_36
X=_35
Xs=_20;
Q=[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[mesg(app,_20,_36,_37)],[]),
   def([_20],[mesg(app,[],_20,_20)],[],[])
  ]
B=[2,3,4,5,6]
A=[1]
Rs=_37
Ys=_36
X=_35
Xs=_20;
no
[main] ?- run([def([Xs],[mesg(app,[],Xs,Xs)],[],[]),
			   def([X,Xs,Ys,Rs],[mesg(app,[X|Xs],Ys,[X|Rs])],[],[mesg(app,Xs,Ys,Rs)]),
			   mesg(app,[1,2,3],[4,5,6],Z),
			   mesg(res,Z)
			]).
[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[],[mesg(app,_20,_36,_37)]),
 def([_20],[mesg(app,[],_20,_20)],[],[]),
 mesg(res,[1|_175]),
 mesg(app,[2,3],[4,5,6],_175)]

[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[],[mesg(app,_20,_36,_37)]),
 def([_20],[mesg(app,[],_20,_20)],[],[]),
 mesg(res,[1,2|_271]),
 mesg(app,[3],[4,5,6],_271)]

[def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[],[mesg(app,_20,_36,_37)]),
 def([_20],[mesg(app,[],_20,_20)],[],[]),
 mesg(res,[1,2,3|_369]),
 mesg(app,[],[4,5,6],_369)]

[def([_20],[mesg(app,[],_20,_20)],[],[]),
 def([_35,_20,_36,_37],[mesg(app,[_35|_20],_36,[_35|_37])],[],[mesg(app,_20,_36,_37)]),
 mesg(res,[1,2,3,4,5,6])]

no
*/

r :- [zlpi].
