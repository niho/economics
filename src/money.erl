% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Lossless encoding and handling of monetary values.
-module(money).

-compile({no_auto_import,[abs/1,min/2,max/2]}).

-export([currency/1,
         dense_currency/1,
         dense/2,
         dense_unsafe/2,
         dense_from_discrete/1,
         dense_from_decimal/3,
         dense_to_decimal/3,
         discrete/3,
         discrete/2,
         discrete_currency/1,
         discrete_amount/1,
         discrete_from_dense/2,
         discrete_from_decimal/3,
         discrete_from_decimal/4,
         discrete_to_decimal/3,
         scale_from_rational/1,
         scale_to_rational/1,
         scale/1,
         default_decimal_conf/0,
         mk_separators/2,
         mk_separators/1,
         separators_comma/0,
         separators_comma_dot/0,
         separators_comma_narrownbsp/0,
         separators_comma_nbsp/0,
         separators_comma_space/0,
         separators_comma_thinsp/0,
         separators_dot/0,
         separators_dot_comma/0,
         separators_dot_narrownbsp/0,
         separators_dot_nbsp/0,
         separators_dot_space/0,
         separators_dot_thinsp/0,
         add/2,
         subtract/2,
         multiply/2,
         is_greater_than/2,
         is_less_than/2,
         is_equal_to/2,
         is_less_or_equal/2,
         is_greater_or_equal/2,
         neg/1,
         abs/1,
         min/2,
         max/2
        ]).

-export_type([dense/1,
              discrete/1,
              separators/0,
              scale/0,
              currency/0,
              unit/0
             ]).


%%%%%%%%%%%
%% TYPES %%
%%%%%%%%%%%

% The dense() type represents a dense monetary value for currency (usually a
% ISO-4217 currency code, but not necessarily) as a rational number.
-opaque dense(Currency) :: {rational(), Currency}.

% The discrete() type represents a discrete monetary value for a currency
% expresed as an integer amount of a particular unit. For example, with
% currency ~ "USD" and unit ~ "cent" you can represent United States Dollars
% to their full extent.
-opaque discrete(Currency) :: {integer(), {Currency, unit()} | {Currency, scale()}}.

% Config to use when rendering or parsing decimal numbers.
%
% - `separators` - Decimal and thousands separators to use when rendering the
%         decimal number. Construct one with mk_separators/1, or pick a ready
%         made one like separators_dot or separators_dot_narrownbsp.
% - `leading_plus` - Whether to render a leading '+' sign in case the amount
%         is positive
% - `digits` - Number of decimal numbers to render, if any.
% - `scale` - Scale used to when rendering the decimal number. This is useful
%         if, for example, you want to render a "number of cents" rather than
%         a "number of dollars" as the whole part of the decimal number when
%         rendering a USD amount. It's particularly useful when rendering
%         currencies such as XAU, where one might prefer to render amounts as
%         a number of grams, rather than as a number of troy-ounces.
-type decimal_conf() :: list({separators, separators()} |
                             {leading_plus, boolean()} |
                             {digits, integer()} |
                             {scale, scale()}
                            ).

% Decimal and thousands separators used when rendering or parsing a decimal number.
-opaque separators() :: {unicode:charlist(), unicode:charlist()} |
                        {unicode:charlist()}.

% Method for approximating a fractional number to an integer number.
%
% - `round` - Approximate x to the nearest integer, or to the nearest even
%         integer if x is equidistant between two integers.
% - `floor` - Approximate x to the nearest integer less than or equal to x.
% - `ceiling` - Approximate x to the nearest integer greater than or equal to x.
% - `truncate` - Approximate x to the nearest integer betwen 0 and x, inclusive.
% - `half_even` - Approximate x to the nearest even integer, when equidistant
%         from the nearest two integers. This is also known as "Bankers
%         Rounding".
-type approximation() :: round |
                         floor |
                         ceiling |
                         truncate |
                         half_even.


% Representation of a scale as two positive integers.
-opaque scale() :: {pos_integer(), pos_integer()}.

-type currency() :: atom().
-type unit() :: atom().

% A rational value.
-type rational() :: rationals:fraction().


%%%%%%%%%%%%
%% SCALES %%
%%%%%%%%%%%%

%% TODO: Add more unit scales.
-define(UNIT_SCALES,
    [{{'EUR',euro}, {1,1}},
     {{'EUR',cent}, {100,1}},
     {{'USD',dollar}, {1,1}},
     {{'USD',cent}, {100,1}},
     {{'SEK',krona}, {1,1}},
     {{'SEK',ore}, {100,1}},
     {{'BTC',bitcoin}, {1,1}},
     {{'BTC',millibitcoin}, {1000,1}},
     {{'BTC',satoshi}, {100000000,1}}
    ]).

%% TODO: Add more currencies.
-define(CURRENCY_SCALES,
    [{'USD', cent},
     {'SEK', ore},
     {'EUR', cent},
     {'BTC', satoshi}
    ]).


%%%%%%%%%
%% API %%
%%%%%%%%%

% @doc Convert a ISO 4127 currency code into a currency atom.
%
% If the currency is not supported, {error, invalid_currency} will be returned.
-spec currency(binary() | atom()) ->
          {ok, currency()} |
          {error, invalid_currency}.
currency(Bin) when is_binary(Bin) ->
    try
        currency(binary_to_existing_atom(Bin))
    catch
        error:badarg -> {error, invalid_currency}
    end;
currency(Symbol) when is_atom(Symbol) ->
    case proplists:is_defined(Symbol, ?CURRENCY_SCALES) of
        true -> {ok, Symbol};
        false -> {error, invalid_currency}
    end.

% @doc dense() currency identifier.
-spec dense_currency(dense(currency())) -> binary().
dense_currency({{fraction,_,_},Currency}) ->
    atom_to_binary(Currency).

% @doc Build a dense() monetary value from a rational() value.
%
% Notice that dense returns {error, invalid} in case the given rational()'s
% denominator is zero, which although unlikely, it is possible if the
% rational() was unsafely constructed. When dealing with hardcoded or trusted
% rational()  values, you can use dense_unsafe/2 instead of dense/2 which
% unsafely constructs a dense().
-spec dense(rational(), Currency) -> {ok, dense(Currency)} | {error, invalid}.
dense(Rational, Currency) ->
    case rationals:denominator(Rational) of
        I when I < 1, I > -1 -> {error, invalid};
        _ -> {ok, dense_unsafe(Rational, Currency)}
    end.
    
% @doc Unsafely build a dense() monetary value from a rational() value.
%
% Contrary to dense, this function *crashes* if the given rational() has zero
% as a denominator, which is something very unlikely to happen unless the given
% rational() was itself unsafely constructed. Other than that, dense/2 and
% dense_unsafe/2 behave the same.
%
% Prefer to use dense/2 when dealing with rational() inputs from untrusted sources.
-spec dense_unsafe(rational(), Currency) -> dense(Currency).
dense_unsafe(Rational, Currency) when is_atom(Currency) ->
    case rationals:denominator(Rational) of
        I when I >= 1; I =< -1 ->
            {Rational, Currency}
    end.

% @doc Convert currency discrete() monetary value into a dense() monetary value.
-spec dense_from_discrete(discrete(Currency)) -> dense(Currency).
dense_from_discrete({Amount,{Currency,Unit}}) when is_atom(Unit) ->
    Scale = scale({Currency,Unit}),
    dense_from_discrete({Amount,{Currency,Scale}});
dense_from_discrete({Amount,{Currency,{A,B}}}) ->
    dense_unsafe(
      rationals:simplify(
        rationals:divide(
          rationals:new(Amount, 1),
          rationals:new(A, B)
         )), Currency).

% @doc Parses a decimal representation of a dense().
%
% @param DecimalConf Config to use for parsing the decimal number. Notice that
%                    a leading '-' or '+' will always be correctly interpreted,
%                    notwithstanding what the "leading '+'" policy is on the
%                    given DecimalConf.
% @param Decimal The raw string containing the decimal representation 
%                (e.g., "-1,234.56789").
% @param Currency The currency to use.
-spec dense_from_decimal(decimal_conf(), binary(), Currency) ->
          {ok, dense(Currency)} | {error, invalid}.
dense_from_decimal(DecimalConf, Decimal, Currency) ->
    case float_from_decimal(DecimalConf, Decimal) of
        {ok, F} ->
            dense(rationals:simplify(rationals:from_float(F)), Currency);
        {error,invalid} ->
            {error,invalid}
    end.

% @doc Render a dense() monetary amount as a decimal number in a potentially lossy manner.
%
% @param DecimalConf Config to use for rendering the decimal number.
% @param Approximation Approximation to use if necessary in order to fit the
%                      dense() amount in as many decimal numbers as requested.
% @param Dense The monetary amount to render.
-spec dense_to_decimal(decimal_conf(), approximation(), dense(currency())) -> binary().
dense_to_decimal(DecimalConf, Approximation, Dense) ->
    {R,_} = Dense,
    F = rationals:to_float(R),
    Digits = proplists:get_value(digits, DecimalConf),
    {I,_} = approximate(Approximation, rationals:from_float(F * math:pow(10,Digits))),
    Separators = proplists:get_value(separators, DecimalConf),
    LeadingPlus = proplists:get_value(leading_plus, DecimalConf),
    {_,Str} = lists:foldr(
                fun(X,{N,Acc}) ->
                        Acc2 =
                            if
                                N == Digits ->
                                    case Separators of
                                        {_,DecimalSep} ->
                                            DecimalSep ++ Acc;
                                        {DecimalSep} ->
                                            DecimalSep ++ Acc
                                    end;
                                N > Digits, (N - Digits) rem 3 == 0 ->
                                    case Separators of
                                        {ThousandSep,_} ->
                                            ThousandSep ++ Acc;
                                        {_} ->
                                            Acc
                                    end;
                                true -> Acc
                            end,
                        {N+1,[X | Acc2]}
                end, {0,[]}, integer_to_list(I)),
    Bin = list_to_binary(Str),
    case {LeadingPlus,F} of
        {true,F} when F >= 0 -> unicode:characters_to_binary([<<"+">>, Bin]);
        _ -> Bin
    end.

% @doc Construct a discrete() value.
-spec discrete(integer(), Currency, unit() | scale()) -> discrete(Currency).
discrete(Amount, Currency, Unit) when is_integer(Amount), is_atom(Currency), is_atom(Unit) ->
    {_,_} = scale({Currency,Unit}),
    {Amount, {Currency, Unit}};
discrete(Amount, Currency, Scale) when is_integer(Amount), is_atom(Currency) ->
    {Amount, {Currency, scale(Scale)}}.

% @doc Construct a discrete() value with the default unit scale for the currency.
%
% Note that some currencies do not have a default unit scale. This function will
% crash if you try to use a currency without a default unit scale.
%
% @see discrete/3
-spec discrete(integer(), Currency) -> discrete(Currency).
discrete(Amount, Currency) ->
    {ok, Symbol} = currency(Currency),
    Unit = proplists:get_value(Symbol, ?CURRENCY_SCALES),
    discrete(Amount, Symbol, Unit).

% @doc discrete() currency identifier.
-spec discrete_currency(discrete(currency())) -> binary().
discrete_currency({_,{Currency,_}}) ->
    atom_to_binary(Currency).

% @doc discrete() amount as integer value.
-spec discrete_amount(discrete(currency())) -> integer().
discrete_amount({Amount,_}) ->
    Amount.

% @doc Approximate a dense() value x to the nearest value fully representable a
% given scale.
%
% If the given dense() doesn't fit entirely in the scale, then a non-zero
% dense() remainder is returned alongside the discrete() approximation.
%
% @param Approximation Approximation to use if necessary in order to fit
%                      the dense() amount in the requested scale.
% @param Dense The dense() value.
-spec discrete_from_dense(approximation(), dense(Currency)) ->
          {discrete(Currency), dense(Currency)}.
discrete_from_dense(Approximation, {Amount,Currency}) ->
    {N,D} = scale(Currency),
    Scale = rationals:new(N,D),
    ScaledAmount = rationals:multiply(Amount, Scale),
    {Int,_Remainder} = approximate(Approximation, ScaledAmount),
    Remainder = rationals:subtract(ScaledAmount, rationals:new(Int)),
    ScaledRemainder = rationals:simplify(
                        rationals:divide(Remainder, Scale)),
    {discrete(Int, Currency), dense_unsafe(ScaledRemainder, Currency)}.

% @doc Parses a decimal representation of a discrete().
%
% Notice that parsing will fail unless the entire precision of the decimal
% number can be represented in the desired scale.
%
% @param DecimalConf Config to use for parsing the decimal number.
%                    Notice that a leading '-' or '+' will always be correctly
%                    interpreted, notwithstanding what the "leading '+'" policy
%                    is on the given DecimalConf.
% @param Decimal The raw string containing the decimal representation
%                (e.g., "-1,234.56789").
-spec discrete_from_decimal(decimal_conf(), binary(), Currency) ->
          {ok, discrete(Currency)} | {error, invalid} | {error, precision}.
discrete_from_decimal(DecimalConf, Decimal, Currency) ->
    discrete_from_decimal(DecimalConf, Decimal, Currency, scale(Currency)).

% @doc Parses a decimal representation of a discrete().
%
% @see discrete_from_decimal/3
-spec discrete_from_decimal(decimal_conf(), binary(), Currency, unit() | scale()) ->
          {ok, discrete(Currency)} | {error, invalid}.
discrete_from_decimal(DecimalConf, Decimal, Currency, Unit) when is_atom(Unit) ->
    discrete_from_decimal(DecimalConf, Decimal, Currency, scale(Unit));
discrete_from_decimal(DecimalConf, Decimal, Currency, Scale) ->
    case float_from_decimal(DecimalConf, Decimal) of
        {ok, F} ->
            {N,D} = Scale,
            FScaled = F * (N / D),
            I = round(FScaled),
            if
                (I - FScaled) == 0 ->
                    {ok, discrete(I, Currency, Scale)};
                true ->
                    {error, precision}
            end;
        {error, invalid} -> {error, invalid}
    end.

% @doc Render a discrete() monetary amount as a decimal number in a potentially
% lossy manner.
%
% @param DecimalConf Config to use for rendering the decimal number.
% @param Approximation Approximation to use if necessary in order to fit the
%                      discrete() amount in as many decimal numbers as
%                      requested.
% @param Discrete The monetary amount to render.
-spec discrete_to_decimal(decimal_conf(), approximation(), discrete(currency())) -> binary().
discrete_to_decimal(DecimalConf, Approximation, Discrete) ->
    dense_to_decimal(DecimalConf, Approximation, dense_from_discrete(Discrete)).

% @doc Construct a scale() from a positive, non-zero rational number.
-spec scale_from_rational(rational()) -> {ok, scale()} | {error, invalid}.
scale_from_rational(Rational) ->
    case rationals:denominator(Rational) of
        I when I < 1, I > -1 -> {error, invalid};
        _ -> {ok, rationals:ratio(Rational)}
    end.
    
% @doc Obtain the rational() representation of a scale.
-spec scale_to_rational(scale()) -> rational().
scale_to_rational({A,B}) ->
    rationals:new(A,B).

% @doc Obtain scale() representation of a unit or currrency scale.
-spec scale({currency(),unit()} | currency() | scale() | discrete(currency())) -> scale().
scale({Currency,Unit}) when is_atom(Currency), is_atom(Unit) ->
    unit_scale({Currency,Unit});
scale(Currency) when is_atom(Currency) ->
    currency_scale(Currency);
scale({N,D}) when is_integer(N), is_integer(D), N > 0, D > 0 ->
    {N,D};
scale({_,{_Currency,{N,D}}}) ->
    scale({N,D});
scale({_,{Currency,Unit}}) ->
    scale({Currency,Unit}).

% @doc The unit_scale/1 function returns a scale as a rational number (expressed
% as {Numerator,Denominator}) indicating how many pieces of unit fit in currency.
-spec unit_scale({currency(),unit()}) -> scale() | undefined.
unit_scale({Currency,Unit}) ->
    proplists:get_value({Currency,Unit}, ?UNIT_SCALES).

% @doc If there exists a canonical smallest scale() that can fully represent the
% currency in all its denominations, then currency_scale(Currency) will
% return such scale(). For example, currency_scale('USD') evaluates to
% unit_scale({'USD',cent}).
-spec currency_scale(currency()) -> scale() | undefined.
currency_scale(Currency) ->
    unit_scale({Currency, proplists:get_value(Currency, ?CURRENCY_SCALES)}).

% @doc Default DecimalConf.
%
% - No leading '+' sign
% - No thousands separator
% - Decimal separator is '.'
% - 2 decimal digits
% - A scale of 1
%
% That is, something like 1.23 or -1234567.89.
-spec default_decimal_conf() -> decimal_conf().
default_decimal_conf() ->
    [{separators, {"."}},
     {leading_plus, false},
     {digits, 2},
     {scale, {1,1}}
    ].

% @doc Construct separators() to use with in decimal_conf().
%
% The separators can't be an ASCII digit nor control character, and they must
% be different from each other.
%
% @param DecimalSep Decimal separator (i.e., the '.' in 1,234.56789)
% @param ThousandSep Thousands separator for the integer part, if any
%                    (i.e., the ',' in 1,234.56789).
-spec mk_separators(unicode:charlist(), unicode:charlist()) ->
          {ok, separators()} | {error, invalid_separator}.
mk_separators(DecimalSep, ThousandSep) when DecimalSep /= ThousandSep ->
    case {is_valid_sep(DecimalSep), is_valid_sep(ThousandSep)} of
        {true, true} -> {ok, {DecimalSep, ThousandSep}};
        _ -> {error, invalid_separator}
    end.

% @doc Like mk_separators/2 but without thousands separator.
-spec mk_separators(unicode:charlist()) -> {ok, separators()} | {error, invalid_separator}.
mk_separators(DecimalSep) ->
    case is_valid_sep(DecimalSep) of
        true -> {ok, {DecimalSep}};
        false -> {error, invalid_separator}
    end.

separators_comma() ->
    {","}.

separators_comma_dot() ->
    {".",","}.

separators_comma_narrownbsp() ->
    {"\x{202f}", ","}.

separators_comma_nbsp() ->
    {"\xa0", ","}.

separators_comma_thinsp() ->
    {"\x{2009}", ","}.

separators_comma_space() ->
    {"\20", ","}.

separators_dot() ->
    {","}.

separators_dot_comma() ->
    {",", "."}.

separators_dot_narrownbsp() ->
    {"\x{202f}", "."}.

separators_dot_nbsp() ->
    {"\xa0", "."}.

separators_dot_thinsp() ->
    {"\x{2009}", "."}.

separators_dot_space() ->
    {"\x20", "."}.

%% ARITHMETIC %%

-spec add(discrete(Currency), discrete(Currency)) -> discrete(Currency);
         (dense(Currency), dense(Currency)) -> dense(Currency).
add({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2 ->
    discrete(A + B, C1, S1);
add({A,C1}, {B,C2}) when C1 == C2 ->
    dense_unsafe(rationals:add(A,B),C1).

-spec subtract(discrete(Currency), discrete(Currency)) -> discrete(Currency);
              (dense(Currency), dense(Currency)) -> dense(Currency).
subtract({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2 ->
    discrete(A - B, C1, S1);
subtract({A,C1}, {B,C2}) when C1 == C2 ->
    dense_unsafe(rationals:subtract(A,B),C1).

-spec multiply(discrete(Currency), integer()) -> discrete(Currency);
              (dense(Currency), rational()) -> dense(Currency).
multiply({A,{C,S}}, ScaleFactor) ->
    discrete(A * ScaleFactor, C, S);
multiply({A,C}, ScaleFactor) ->
    dense_unsafe(rationals:multiply(A,ScaleFactor),C).

%% COMPARISION %%

-spec is_greater_than(discrete(Currency), discrete(Currency)) -> boolean();
                     (dense(Currency), dense(Currency)) -> boolean().
is_greater_than({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2 ->
    A > B;
is_greater_than({A,C1}, {B,C2}) when C1 == C2 ->
    rationals:is_greater_than(A,B).

-spec is_less_than(discrete(Currency), discrete(Currency)) -> boolean();
                  (dense(Currency), dense(Currency)) -> boolean().
is_less_than({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2 ->
    A < B;
is_less_than({A,C1}, {B,C2}) when C1 == C2 ->
    rationals:is_less_than(A,B).

-spec is_equal_to(discrete(Currency), discrete(Currency)) -> boolean();
                 (dense(Currency), dense(Currency)) -> boolean().
is_equal_to({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2 ->
    A == B;
is_equal_to({A,C1}, {B,C2}) when C1 == C2 ->
    rationals:is_equal_to(A,B).

-spec is_greater_or_equal(discrete(Currency), discrete(Currency)) -> boolean();
                         (dense(Currency), dense(Currency)) -> boolean().
is_greater_or_equal({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2 ->
    A >= B;
is_greater_or_equal({A,C1}, {B,C2}) when C1 == C2 ->
    rationals:is_greater_or_equal(A,B).

-spec is_less_or_equal(discrete(Currency), discrete(Currency)) -> boolean();
                      (dense(Currency), dense(Currency)) -> boolean().
is_less_or_equal({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2 ->
    A =< B;
is_less_or_equal({A,C1}, {B,C2}) when C1 == C2 ->
    rationals:is_less_or_equal(A,B).

%% UNARY OPERATIONS %%

-spec neg(discrete(Currency)) -> discrete(Currency);
         (dense(Currency)) -> dense(Currency).
neg({A,{C,S}}) ->
    discrete(0 - A,C,S);
neg({A,C}) ->
    dense_unsafe(rationals:subtract(rationals:new(0), A),C).

-spec abs(discrete(Currency)) -> discrete(Currency);
         (dense(Currency)) -> dense(Currency).
abs({A,{C,S}}) ->
    discrete(erlang:abs(A),C,S);
abs({A,C}) ->
    dense_unsafe(rational_abs(A),C).

%% BINARY OPERATIONS %%

-spec max(discrete(Currency), discrete(Currency)) -> discrete(Currency);
         (dense(Currency), dense(Currency)) -> dense(Currency).
max({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2, A > B -> discrete(A,C1,S1);
max({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2, B > A -> discrete(B,C2,S2);
max({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2, A == B -> discrete(A,C1,S1);
max({A,C1}, {B,C2}) when C1 == C2 ->
    case rationals:is_greater_or_equal(A,B) of
        true -> dense_unsafe(A,C1);
        false -> dense_unsafe(B,C2)
    end.

-spec min(discrete(Currency), discrete(Currency)) -> discrete(Currency);
         (dense(Currency), dense(Currency)) -> dense(Currency).
min({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2, A < B -> discrete(A,C1,S1);
min({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2, B < A -> discrete(B,C2,S2);
min({A,{C1,S1}}, {B,{C2,S2}}) when C1 == C2, S1 == S2, A == B -> discrete(A,C1,S1);
min({A,C1}, {B,C2}) when C1 == C2 ->
    case rationals:is_less_or_equal(A,B) of
        true -> dense_unsafe(A,C1);
        false -> dense_unsafe(B,C2)
    end.


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

-spec float_from_decimal(decimal_conf(), binary()) -> {ok, float()} | {error, invalid}.
float_from_decimal(DecimalConf, Decimal) ->
    Str = case proplists:get_value(separators, DecimalConf) of
              {ThousandSep,DecimalSep} ->
                  iolist_to_binary(
                    string:replace(
                      string:replace(Decimal, [ThousandSep], "", all),
                      [DecimalSep], ".", trailing
                     ));
              {DecimalSep} ->
                  iolist_to_binary(
                    string:replace(Decimal, [DecimalSep], ".", trailing))
          end,
    try
        case string:find(Str, ".") of
            nomatch -> {ok, float(binary_to_integer(Str))};
            _ -> {ok, binary_to_float(Str)}
        end
    catch
        error:badarg ->
            {error,invalid}
    end.

-spec approximate(approximation(), rational()) -> {integer(), rational()}.
approximate(round, Rational) ->
    Round = round(rationals:to_float(Rational)),
    {Round, rationals:simplify(
              rationals:subtract(Rational, rationals:new(Round)))};
approximate(floor, Rational) ->
    Round = floor(rationals:to_float(Rational)),
    {Round, rationals:simplify(
              rationals:subtract(Rational, rationals:new(Round)))};
approximate(ceiling, Rational) ->
    Round = ceil(rationals:to_float(Rational)),
    {Round, rationals:simplify(
              rationals:subtract(Rational, rationals:new(Round)))};
approximate(truncate, Rational) ->
    Round = trunc(rationals:to_float(Rational)),
    {Round, rationals:simplify(
              rationals:subtract(Rational, rationals:new(Round)))};
approximate(half_even, Rational) ->
    _X = signum(-1), %% Force Dialyzer to accept negative numbers
    Tr = trunc(rationals:to_float(Rational)),
    Rr = rationals:simplify(
              rationals:subtract(Rational,rationals:new(Tr))),
    case rationals:ratio(rational_abs(Rr)) of
        {1,2} ->
            case even(Tr) of
                true -> {Tr,Rr};
                false -> {Tr + signum(Tr), Rr}
            end;
        _ -> approximate(round, Rational)
    end.

-spec even(integer()) -> boolean().
even(X) when X >= 0 ->
    (X band 1) == 0;
even(X) when X < 0 ->
    even(erlang:abs(X)).

-spec signum(integer()) -> integer().
signum(X) when X < 0 -> -1;
signum(X) when X > 0 -> 1;
signum(0) -> 0.

-spec is_valid_sep(unicode:charlist()) -> boolean().
is_valid_sep(Sep) when is_list(Sep) ->
    not lists:member(Sep,
                     ["0","1","2","3","4",
                      "5","6","7","8","9"]).

rational_abs(F) ->
    Float = rationals:to_float(F),
    if
        Float >= 0 -> F;
        true ->
            {Numerator,Denominator} = rationals:ratio(F),
            rationals:new(erlang:abs(Numerator), erlang:abs(Denominator))
    end.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

currency_test() ->
    ?assertEqual({ok,'SEK'}, currency(<<"SEK">>)),
    ?assertEqual({ok,'SEK'}, currency('SEK')),
    ?assertEqual({error, invalid_currency}, currency(<<"XXX">>)).

dense_currency_test() ->
    {ok,Dense} = dense(rationals:new(10), 'SEK'),
    ?assertEqual(<<"SEK">>, dense_currency(Dense)).

dense_test() ->
    ?assertEqual({ok,{{fraction,10,1},'SEK'}}, dense(rationals:new(10), 'SEK')),
    ?assertEqual({error,invalid}, dense(rationals:new(10,0), 'SEK')).

dense_unsafe_test() ->
    ?assertError({case_clause,0}, dense_unsafe(rationals:new(10,0), 'SEK')),
    ?assertEqual({{fraction,10,1},'SEK'}, dense_unsafe(rationals:new(10,1), 'SEK')).

dense_from_discrete_test() ->
    ?assertEqual({{fraction,10,1},'SEK'},
                 dense_from_discrete(discrete(10,'SEK',{1,1}))),
    ?assertEqual({{fraction,10,1},'SEK'},
                 dense_from_discrete(discrete(1000,'SEK',{100,1}))),
    ?assertEqual({{fraction,1000,1},'XAU'},
                 dense_from_discrete(discrete(31103477,'XAU',{31103477,1000}))),
    ?assertEqual({{fraction,10,1},'USD'},
                 dense_from_discrete(discrete(1000,'USD',cent))),
    ?assertEqual({{fraction,21,2},'USD'},
                 dense_from_discrete(discrete(1050,'USD',cent))).

dense_from_decimal_test() ->
    ?assertEqual({ok, {{fraction,10,1},'SEK'}},
                 dense_from_decimal(default_decimal_conf(), <<"10">>, 'SEK')),
    {ok, {A,'SEK'}} = dense_from_decimal(default_decimal_conf(), <<"-1234.56789">>, 'SEK'),
    ?assertEqual(rationals:from_float(-1234.56789), A),
    {ok, {B,'SEK'}} = dense_from_decimal([{separators, separators_comma_dot()}],
                                         <<"-1.234,56789">>, 'SEK'),
    ?assertEqual(rationals:from_float(-1234.56789), B),
    ?assertEqual({error, invalid},
                 dense_from_decimal(default_decimal_conf(), <<"">>, 'SEK')).

dense_to_decimal_test() ->
    ?assertEqual(<<"10.00">>,
                 dense_to_decimal(default_decimal_conf(), round,
                                  {{fraction,10,1},'SEK'})),
    ?assertEqual(<<"-1234.57">>,
                 dense_to_decimal(default_decimal_conf(), round,
                                  {{fraction,-123456789,100000},'SEK'})),
    ?assertEqual(<<"+1,234.5678">>,
                 dense_to_decimal([{separators, separators_dot_comma()},
                                   {leading_plus, true},
                                   {digits, 4}
                                  ],
                                  floor,
                                  {{fraction,123456789,100000},'SEK'})),
    ?assertEqual(<<"1,234,567.89">>,
                 dense_to_decimal([{separators, separators_dot_comma()}, {digits, 2}], round,
                                 {{fraction,123456789,100},'SEK'})).

discrete_test() ->
    ?assertEqual({10,{'SEK',{1,1}}}, discrete(10, 'SEK', {1,1})),
    ?assertEqual({1000,{'SEK',ore}}, discrete(1000, 'SEK', ore)),
    ?assertEqual({1000,{'SEK',ore}}, discrete(1000, 'SEK')),
    ?assertEqual({1000,{'SEK',ore}}, discrete(1000, <<"SEK">>)).

discrete_currency_test() ->
    ?assertEqual(<<"SEK">>, discrete_currency(discrete(10,'SEK',{1,1}))).

discrete_amount_test() ->
    ?assertEqual(10, discrete_amount(discrete(10,'SEK',{1,1}))).

discrete_from_dense_test() ->
    ?assertEqual({{1000,{'SEK',ore}}, {{fraction,0,1},'SEK'}},
                 discrete_from_dense(round,
                                     dense_unsafe(rationals:new(10), 'SEK'))),
    ?assertEqual({{333,{'SEK',ore}}, {{fraction,1,300},'SEK'}},
                 discrete_from_dense(round,
                                     dense_unsafe(rationals:new(10,3), 'SEK'))),
    ?assertEqual({{375,{'SEK',ore}}, {{fraction,0,1},'SEK'}},
                 discrete_from_dense(floor,
                                     dense_unsafe(rationals:new(15,4), 'SEK'))),
    ?assertEqual({{334,{'SEK',ore}}, {{fraction,1,-150},'SEK'}},
                 discrete_from_dense(ceiling,
                                     dense_unsafe(rationals:new(10,3), 'SEK'))).

discrete_from_dense_lossless_test() ->
    Numbers = [rand:uniform(10) + rand:uniform() || _ <- lists:seq(1, 1000)],
    lists:map(
      fun(N) ->
              lists:map(
                fun(Approximation) ->
                        Dense = dense_unsafe(rationals:from_float(N), 'SEK'),
                        {I,R} = discrete_from_dense(Approximation, Dense),
                        {A,B} = {Dense, add(dense_from_discrete(I), R)},
                        ?assertEqual({A,Approximation,I},
                                     {B,Approximation,I})
                end, [round,floor,ceiling,truncate,half_even])
      end, Numbers).

discrete_from_decimal_test() ->
    ?assertEqual({ok, {1000,{'SEK',{100,1}}}},
                 discrete_from_decimal(default_decimal_conf(), <<"10">>, 'SEK')),
    ?assertEqual({ok, {-123456,{'SEK',{100,1}}}},
                 discrete_from_decimal(default_decimal_conf(), <<"-1234.56">>, 'SEK')),
    ?assertEqual({ok, {-123456789,{'SEK',{100000,1}}}},
                 discrete_from_decimal([{separators,separators_comma_dot()}],
                                       <<"-1.234,56789">>, 'SEK', {100000,1})),
    ?assertEqual({error, invalid},
                 discrete_from_decimal(default_decimal_conf(), <<"-1,234.56789">>, 'SEK')),
    ?assertEqual({error, invalid},
                 dense_from_decimal(default_decimal_conf(), <<"">>, 'SEK')).

discrete_to_decimal_test() ->
    ?assertEqual(<<"10.00">>,
                 discrete_to_decimal(default_decimal_conf(), round,
                                     discrete(1000,'SEK',ore))),
    ?assertEqual(<<"-1234.56">>,
                 discrete_to_decimal(default_decimal_conf(), round,
                                       discrete(-123456,'SEK',ore))),
    ?assertEqual(<<"+1,234.567">>,
                 discrete_to_decimal([{separators, separators_dot_comma()},
                                      {leading_plus, true},
                                      {digits, 3},
                                      {scale, {1000,1}}
                                     ],
                                     floor,
                                     discrete(123456789, 'SEK', {100000,1}))).

scale_from_rational_test() ->
    ?assertEqual({ok,{1,2}}, scale_from_rational(rationals:from_float(0.5))).

scale_to_rational_test() ->
    ?assertEqual({fraction,1,2}, scale_to_rational({1,2})).

scale_test() ->
    ?assertEqual({100,1}, scale({'SEK',ore})),
    ?assertEqual({1,1}, scale({'SEK',krona})),
    ?assertEqual({100,1}, scale('SEK')),
    ?assertEqual({100,1}, scale({100,1})),
    ?assertEqual({100,1}, scale(discrete(1000,'SEK',ore))).

add_test() ->
    ?assertEqual({20,{'SEK',{1,1}}},
                 add(discrete(10,'SEK',{1,1}),
                     discrete(10,'SEK',{1,1}))),
    ?assertEqual({{fraction,20,1},'SEK'},
                 add(dense_unsafe({fraction,10,1},'SEK'),
                     dense_unsafe({fraction,10,1},'SEK'))).

subtract_test() ->
    ?assertEqual({20,{'SEK',{1,1}}},
                 subtract(discrete(30,'SEK',{1,1}),
                          discrete(10,'SEK',{1,1}))),
    ?assertEqual({{fraction,20,1},'SEK'},
                 subtract(dense_unsafe({fraction,30,1},'SEK'),
                          dense_unsafe({fraction,10,1},'SEK'))).

multiply_test() ->
    ?assertEqual({8,{'SEK',{1,1}}},
                 multiply(discrete(2,'SEK',{1,1}), 4)),
    ?assertEqual({{fraction,8,1},'SEK'},
                 multiply(dense_unsafe({fraction,2,1},'SEK'), {fraction,4,1})).

is_greater_than_test() ->
    ?assertEqual(true, is_greater_than(dense_unsafe({fraction,10,1},'SEK'),
                                       dense_unsafe({fraction,10,3},'SEK'))),
    ?assertEqual(false, is_greater_than(dense_unsafe({fraction,10,3},'SEK'),
                                        dense_unsafe({fraction,10,1},'SEK'))),
    ?assertEqual(true, is_greater_than(discrete(1000,'SEK',ore),
                                       discrete(500,'SEK',ore))),
    ?assertEqual(false, is_greater_than(discrete(500,'SEK',ore),
                                        discrete(1000,'SEK',ore))).

is_less_than_test() ->
    ?assertEqual(false, is_less_than(dense_unsafe({fraction,10,1},'SEK'),
                                     dense_unsafe({fraction,10,3},'SEK'))),
    ?assertEqual(true, is_less_than(dense_unsafe({fraction,10,3},'SEK'),
                                    dense_unsafe({fraction,10,1},'SEK'))),
    ?assertEqual(false, is_less_than(discrete(1000,'SEK',ore),
                                     discrete(500,'SEK',ore))),
    ?assertEqual(true, is_less_than(discrete(500,'SEK',ore),
                                    discrete(1000,'SEK',ore))).

is_equal_to_test() ->
    ?assertEqual(false, is_equal_to(dense_unsafe({fraction,10,1},'SEK'),
                                    dense_unsafe({fraction,10,3},'SEK'))),
    ?assertEqual(true, is_equal_to(dense_unsafe({fraction,10,3},'SEK'),
                                   dense_unsafe({fraction,10,3},'SEK'))),
    ?assertEqual(false, is_equal_to(discrete(1000,'SEK',ore),
                                    discrete(500,'SEK',ore))),
    ?assertEqual(true, is_equal_to(discrete(500,'SEK',ore),
                                   discrete(500,'SEK',ore))).

is_greater_or_equal_test() ->
    ?assertEqual(true, is_greater_or_equal(dense_unsafe({fraction,10,1},'SEK'),
                                           dense_unsafe({fraction,10,3},'SEK'))),
    ?assertEqual(true, is_greater_or_equal(dense_unsafe({fraction,10,3},'SEK'),
                                           dense_unsafe({fraction,10,3},'SEK'))),
    ?assertEqual(false, is_greater_or_equal(dense_unsafe({fraction,10,3},'SEK'),
                                            dense_unsafe({fraction,10,1},'SEK'))),
    ?assertEqual(true, is_greater_or_equal(discrete(1000,'SEK',ore),
                                           discrete(500,'SEK',ore))),
    ?assertEqual(true, is_greater_or_equal(discrete(500,'SEK',ore),
                                           discrete(500,'SEK',ore))),
    ?assertEqual(false, is_greater_or_equal(discrete(500,'SEK',ore),
                                            discrete(1000,'SEK',ore))).

is_less_or_equal_test() ->
    ?assertEqual(false, is_less_or_equal(dense_unsafe({fraction,10,1},'SEK'),
                                         dense_unsafe({fraction,10,3},'SEK'))),
    ?assertEqual(true, is_less_or_equal(dense_unsafe({fraction,10,3},'SEK'),
                                        dense_unsafe({fraction,10,1},'SEK'))),
    ?assertEqual(true, is_less_or_equal(dense_unsafe({fraction,10,3},'SEK'),
                                        dense_unsafe({fraction,10,3},'SEK'))),
    ?assertEqual(false, is_less_or_equal(discrete(1000,'SEK',ore),
                                         discrete(500,'SEK',ore))),
    ?assertEqual(true, is_less_or_equal(discrete(500,'SEK',ore),
                                        discrete(1000,'SEK',ore))),
    ?assertEqual(true, is_less_or_equal(discrete(500,'SEK',ore),
                                        discrete(500,'SEK',ore))).

neg_test() ->
    ?assertEqual({{fraction,-10,1},'SEK'}, neg(dense_unsafe({fraction,10,1},'SEK'))),
    ?assertEqual({-1000,{'SEK',ore}}, neg(discrete(1000,'SEK',ore))).

abs_test() ->
    ?assertEqual({{fraction,10,1},'SEK'}, abs(dense_unsafe({fraction,-10,1},'SEK'))),
    ?assertEqual({{fraction,10,1},'SEK'}, abs(dense_unsafe({fraction,10,1},'SEK'))),
    ?assertEqual({1000,{'SEK',ore}}, abs(discrete(-1000,'SEK',ore))),
    ?assertEqual({1000,{'SEK',ore}}, abs(discrete(1000,'SEK',ore))).

max_test() ->
    ?assertEqual({{fraction,10,1},'SEK'}, max({{fraction,10,1},'SEK'},
                                              {{fraction,1,1},'SEK'})),
    ?assertEqual({1000,{'SEK',{1,1}}}, max({1000,{'SEK',{1,1}}},
                                           {100,{'SEK',{1,1}}})).

min_test() ->
    ?assertEqual({{fraction,1,1},'SEK'}, min({{fraction,10,1},'SEK'},
                                             {{fraction,1,1},'SEK'})),
    ?assertEqual({100,{'SEK',{1,1}}}, min({1000,{'SEK',{1,1}}},
                                          {100,{'SEK',{1,1}}})).

approximate_test() ->
    lists:foreach(fun({Approx,R,ExpectedNum,ExpectedRest}) ->
                          {Num,Rest} = approximate(Approx, R),
                          ?assertEqual({Approx,R,ExpectedNum,Rest,true},
                                       {Approx,R,Num,Rest,
                                        rationals:is_equal_to(ExpectedRest, Rest)
                                       })
                  end,
                  [{round, rationals:new(33,10), 3, rationals:new(3,10)},
                   {round, rationals:new(35,10), 4, rationals:new(-1,2)},
                   {round, rationals:new(25,10), 3, rationals:new(-1,2)},
                   {round, rationals:new(36,10), 4, rationals:new(-2,5)},
                   {round, rationals:new(-33,10), -3, rationals:new(-3,10)},
                   {round, rationals:new(-35,10), -4, rationals:new(1,2)},
                   {round, rationals:new(-25,10), -3, rationals:new(1,2)},
                   {round, rationals:new(-36,10), -4, rationals:new(2,5)},

                   {floor, rationals:new(33,10), 3, rationals:new(3,10)},
                   {floor, rationals:new(35,10), 3, rationals:new(1,2)},
                   {floor, rationals:new(25,10), 2, rationals:new(1,2)},
                   {floor, rationals:new(36,10), 3, rationals:new(3,5)},
                   {floor, rationals:new(-33,10), -4, rationals:new(7,10)},
                   {floor, rationals:new(-35,10), -4, rationals:new(1,2)},
                   {floor, rationals:new(-25,10), -3, rationals:new(1,2)},
                   {floor, rationals:new(-36,10), -4, rationals:new(2,5)},

                   {ceiling, rationals:new(33,10), 4, rationals:new(-7,10)},
                   {ceiling, rationals:new(35,10), 4, rationals:new(-1,2)},
                   {ceiling, rationals:new(25,10), 3, rationals:new(-1,2)},
                   {ceiling, rationals:new(36,10), 4, rationals:new(-2,5)},
                   {ceiling, rationals:new(-33,10), -3, rationals:new(-3,10)},
                   {ceiling, rationals:new(-35,10), -3, rationals:new(-1,2)},
                   {ceiling, rationals:new(-25,10), -2, rationals:new(-1,2)},
                   {ceiling, rationals:new(-36,10), -3, rationals:new(-3,5)},

                   {truncate, rationals:new(33,10), 3, rationals:new(3,10)},
                   {truncate, rationals:new(35,10), 3, rationals:new(1,2)},
                   {truncate, rationals:new(25,10), 2, rationals:new(1,2)},
                   {truncate, rationals:new(36,10), 3, rationals:new(3,5)},
                   {truncate, rationals:new(-33,10), -3, rationals:new(-3,10)},
                   {truncate, rationals:new(-35,10), -3, rationals:new(-1,2)},
                   {truncate, rationals:new(-25,10), -2, rationals:new(-1,2)},
                   {truncate, rationals:new(-36,10), -3, rationals:new(-3,5)},

                   {half_even, rationals:new(33,10), 3, rationals:new(3,10)},
                   {half_even, rationals:new(35,10), 4, rationals:new(1,2)},
                   {half_even, rationals:new(25,10), 2, rationals:new(1,2)},
                   {half_even, rationals:new(36,10), 4, rationals:new(-2,5)},
                   {half_even, rationals:new(-33,10), -3, rationals:new(-3,10)},
                   {half_even, rationals:new(-35,10), -4, rationals:new(-1,2)},
                   {half_even, rationals:new(-25,10), -2, rationals:new(-1,2)},
                   {half_even, rationals:new(-36,10), -4, rationals:new(2,5)}
                  ]).

-endif.
