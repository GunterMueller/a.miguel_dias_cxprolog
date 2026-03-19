:- op(  1160,   xfx,    =>      ).
:- op(  1140,   xfy,    '|'     ).
:- op(   999,    fy,    try     ).
:- op(   960,   xfx,    &       ).
:- op(   100,    fx,    once    ).
:- op(   100,    fx,    while   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       generic                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op( 200, fx,  mrk_    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       dictionary                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op( 1, xfx, =*= ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       scheme                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op( 300, xfx, #  ).
%       :- op( 400, yfx, \  ).
:- op( 400, yfx, // ).
:- op( 400, yfx, \\ ).
:- op( 900, xfy, :  ).
:- op( 920, fx,  *  ).
:- op( 920, fx,  =  ).
:- op( 999, fx,  t  ).
:- op( 999, fx,  d  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       query / update                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%       inherits:       #       /       =       @

:- op( 20,  xfx, like  ).

:- op( 5,    fx, @@ ).
:- op( 9,    fx, $  ).
:- op( 9,   xfx, $  ).
:- op( 100,  fx, ?  ).
:- op( 100,  fx, ?? ).
:- op( 400, xfy, -< ).
:- op( 999, xfx, -+ ).
:- op( 999, xfx, +-+ ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       output                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op( 1,     fx, --  ).
:- op( 1,     fx, \-  ).
:- op( 1,     fx, -\  ).
:- op( 1,    xfx, :--  ).
:- op( 1,    xfx, :\-  ).
:- op( 1,    xfx, :-\  ).
:- op( 1,    xfx, separated  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       service                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op( 300,   fx, /   ).
:- op( 500,   fx, wrap ).
:- op( 500,   fx, wrap_ ).
:- op( 500,   fx, exec ).
:- op( 500,   fx, exec_ ).
:- op( 500,   fx, err  ).
:- op( 950,  xfx, <>  ).
:- op( 999,  yfx, <-  ).
:- op( 999,  xfx, <<- ).
:- op( 1199,  xfx, <=>  ).


%       C <<- O & X<>D   =   C <<- O & ( X<>D )


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Exec                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op( 100,  fx, ^@   ).
:- op( 100,  fx, ~@   ).
:- op( 100, xfx, @=   ).
:- op( 100, xfx, =@   ).
:- op( 999,  fx,  ^   ).
:- op( 999,  fx,  ~   ).
:- op( 999, xfx, <?>  ).
:- op( 999, xfx, <+>  ).
:- op( 999, xfx, <??> ).
:- op( 999, xfx, <=   ).
:- op( 999, xfx, <==  ).
:- op( 999, xfx, <<=  ).
:- op( 999, xfx, <<== ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Method                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op( 999, xfy, <<< ).
:- op( 999, xfy, --- ).
:- op( 999, xfy, >>> ).


escolha( no_ano, situa,
         situa / edsitua # ( @ ano_lectivo ),
         Lista <>
         ( init( nome_sit_cur ),
           ( in(S,Lista) => texto( situa, S, Nome ),
                           push( nome_sit_cur, ( Nome, S ) ) ),
           nome_sit_cur =: NS,
           sort( NS, compare_tuples( [ 1 : @< ] ), NSO ),
           center : ( NSO <- link_list( self_link( situa ) ) ) ) ).

/*
 15a1990 MakeIndex main:escolha/4

 1591bdc TryMeElse 0 4
 1591be8 GetAtomic no_ano X0
 1591bf4 GetAtomic situa X1
 1591c00 GetStructure //2 X2
 1591c0c UnifyAtomic situa
 1591c14 UnifyXVariable X4
 1591c1c GetStructure #/2 X4
 1591c28 UnifyAtomic edsitua
 1591c30 UnifyXVariable X4
 1591c38 GetStructure @/1 X4
 1591c44 UnifyAtomic ano_lectivo
 1591c4c GetStructure <>/2 X3
 1591c58 UnifyXVariable X4
 1591c60 UnifyXVariable X5
 1591c68 GetStructure ,/2 X5
 1591c74 UnifyXVariable X5
 1591c7c UnifyXVariable X6
 1591c84 GetStructure init/1 X5
 1591c90 UnifyAtomic nome_sit_cur
 1591c98 GetStructure ,/2 X6
 1591ca4 UnifyXVariable X5
 1591cac UnifyXVariable X6
 1591cb4 GetStructure =>/2 X5
 1591cc0 UnifyXVariable X5
 1591cc8 UnifyXVariable X7
 1591cd0 GetStructure in/2 X5
 1591cdc UnifyXVariable X5
 1591ce4 UnifyXValue X4
 1591cec GetStructure ,/2 X7
 1591cf8 UnifyXVariable X7
 1591d00 UnifyXVariable X8
 1591d08 GetStructure texto/3 X7
 1591d14 UnifyAtomic situa
 1591d1c UnifyXValue X5
 1591d24 UnifyXVariable X7
 1591d2c GetStructure push/2 X8
 1591d38 UnifyAtomic nome_sit_cur
 1591d40 UnifyXVariable X8
 1591d48 GetStructure ,/2 X8
 1591d54 UnifyXValue X7
 1591d5c UnifyXValue X5
 1591d64 GetStructure ,/2 X6
 1591d70 UnifyXVariable X6
 1591d78 UnifyXVariable X8
 1591d80 GetStructure =:/2 X6
 1591d8c UnifyAtomic nome_sit_cur
 1591d94 UnifyXVariable X6
 1591d9c GetStructure ,/2 X8
 1591da8 UnifyXVariable X8
 1591db0 UnifyXVariable X9
 1591db8 GetStructure sort/3 X8
 1591dc4 UnifyXValue X6
 1591dcc UnifyXVariable X8
 1591dd4 UnifyXVariable X10
 1591ddc GetStructure compare_tuples/1 X8
 1591de8 UnifyXVariable X8
 1591df0 GetList X8
 1591df8 UnifyXVariable X8
 1591e00 UnifyNil
 1591e04 GetStructure :/2 X8
 1591e10 UnifyAtomic 1
 1591e18 UnifyAtomic @<
 1591e20 GetStructure :/2 X9
 1591e2c UnifyAtomic center
 1591e34 UnifyXVariable X8
 1591e3c GetStructure <-/2 X8
 1591e48 UnifyXValue X10
 1591e50 UnifyXVariable X8
 1591e58 GetStructure link_list/1 X8
 1591e64 UnifyXVariable X8
 1591e6c GetStructure self_link/1 X8
 1591e78 UnifyAtomic situa
 1591e80 Proceed
*/