read_digits_(Exp, float(N), SoFar, Base, Value, NextCh, [C|S0], S1) :-
    ( Exp =:= 0'e ; Exp =:= 0'E ),      % Is this it?
    C >= "0", C =< "9",         % check for exponent
    !,                  % only allowed w/ float(N)
    read_digits_(C, none, 0, Base, EValue, NextCh, S0, S1),
    !,
    Expo is EValue-N,
    compute_power(Expo, Base, Factor),
    Value is SoFar * Factor.
