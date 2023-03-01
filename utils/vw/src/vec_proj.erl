%% @doc
%% Elliptic curve functions (projective version)
%% @end
-module(vec_proj).

-compile([export_all, nowarn_export_all]).


-record(ec,
        {a   :: integer(),
         b   :: integer(),
         mod :: integer()}).

-type ec() :: #ec{}.


-type ecpoint() :: {xyz, X :: integer(),
                         Y :: integer(),
                         Z :: integer()}.



-spec elem(Point, Curve) -> Result
    when Point  :: ecpoint(),
         Curve  :: ec(),
         Result :: boolean().
%% @doc
%% Test if the given point is on the given curve

elem({xyz, X, Y, Z}, #ec{a = A, b = B, mod = M}) ->
    modeq(Z*Y*Y, X*X*X + A*X*Z*Z + B*Z*Z*Z, M).



-spec modeq(A, B, Modulus) -> Result
    when A       :: integer(),
         B       :: integer(),
         Modulus :: integer(),
         Result  :: boolean().
%% @doc
%% Check if A === B mod M
%% @end

%% issue: -3 rem 5 = -3 (should be: 2)
%% shortcut in the case of comparisons:
%% -5 rem 5 = 0
%%
%% So only need to do one remainder operation in this case
modeq(A, B, M) ->
    0 =:= ((A - B) rem M).


%%% the 'rem' operator will return a negative result if X < 0
%%%
%modulo(X, M) when X < 0 ->
%    M - (X rem M);
%modulo(X, M) ->
%    X rem M.
