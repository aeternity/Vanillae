%% @doc
%% Elliptic curve functions (affine version)
%% @end
-module(vec).

-compile([export_all, nowarn_export_all]).


-record(ec,
        {a   :: integer(),
         b   :: integer(),
         mod :: integer()}).

-type ec() :: #ec{}.


-type ecpoint() :: {xy, X :: integer(),
                        Y :: integer()}.



-spec elem(Point, Curve) -> Result
    when Point  :: ecpoint(),
         Curve  :: ec(),
         Result :: boolean().
%% @doc
%% Test if the given point is on the given curve

elem({xy, X, Y}, #ec{a = A, b = B, mod = M}) ->
    modeq(Y*Y, X*X*X + A*X + B, M).


%
%-spec grop(Point, Point, Curve) -> NewPoint
%    when Point    :: ecpoint(),
%         Curve    :: ec(),
%         NewPoint :: Point.
%%% @doc
%%% Elliptic curve group operation
%%%
%%% See Paar & Pelzl, p. 244
%%% @end
%
%%% point doubling
%grop(Point = {xy, X, Y}, Point, Curve = #ec{a=A, b=B, mod=P}) ->
%    true     = elem(Point, Curve),
%    S        = modulo_divide(3*X*X + A,
%                             2*Y,
%                             P);
%    NewX     = modulo(S*S - 2*X,        P),
%    NewY     = modulo(S*(X - NewX) - Y, P),
%    NewPoint = {xy, NewX, NewY},
%    true     = elem(NewPoint, Curve),
%    NewPoint;
%%% point "addition" (stupid name)
%grop(Point1 = {xy, X1, Y1}, Point2 = {xy, X2, Y2}, Curve = #ec{a=A, b=B, mod=P}) ->
%    true     = elem(Point1, Curve),
%    true     = elem(Point2, Curve),
%    S        = modulo_divide(Y2 - Y1,
%                             X2 - X1,
%                             P);
%    NewX     = modulo(S*S - X1 - X2,      P),
%    NewY     = modulo(S*(X1 - NewX) - Y1, P),
%    NewPoint = {xy, NewX, NewY},
%    true     = elem(NewPoint, Curve),
%    NewPoint.


%% issue: -3 rem 5 = -3 (should be: 2)
%% shortcut in the case of comparisons:
%% -5 rem 5 = 0
%%
%% So only need to do one remainder operation in this case
modeq(A, B, M) ->
    0 =:= ((A - B) rem M).


%% the 'rem' operator will return a negative result if X < 0
%%
modulo(X, M) when X < 0 ->
    M - (X rem M);
modulo(X, M) ->
    X rem M.


modulo_divide(A, B, M) ->
    modulo(A * mod_inverse(B, M), M).


%% here we assume that B and M are relatively prime (i.e. their GCD is 1)
%% This means that there exist a pair of integers X and Y such that X*B + Y*M = 1
%% the inverse of B mod M is now X, because X*B == 1 mod M
mod_inverse(B, M) ->
    {1, QB, QM} = egcd4(B, M),
    % we know
    1 = QB*B + QM*M,
    % therefore
    true = modeq(QB*B, 1, M),
    % thus QB is the multiplicative inverse
    QB.



%egcd(A, B) when 0 < A, 0 =< B, B =< A ->
%    % QA*A + QB*B = R
%    Result = {GCD, QA, QB} = egcd4(A, B),
%    GCD = QA*A + QB*B,
%    Result.


% bottom case: egcd against 0
egcd4(GCD, 0) ->
    % A = GCD,
    % B = 0,
    % Result: {GCD, QA, QB} with the property
    % QA*A + QB*B = GCD
    % so in this bottom case
    % GCD = 1*GCD + 0*0,
    {GCD, 1, 0};
% general case, A, and B
egcd4(A, B) ->
    Q = A div B,
    R = A rem B,
    %% what combination of B and R gives me the gcd
    {GCD, QB, QR} = egcd4(B, R),
    %% assertions:
    R   = A - Q*B,
    GCD = QB*B + QR*R,
    GCD = QB*B + QR*(A - Q*B),      %% substitute R -> A - Q*B
    GCD = QR*A + (QB - QR*Q)*B,     %% regroup terms
    {GCD, QR, QB - QR*Q}.           %% result

%asserteq(A, B) ->
%    io:format("~tp : ~tp = ~tp~n", [A =:= B, A, B]).
