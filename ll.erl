-module(ll).
-author('baryluk@smp.if.uj.edu.pl').

-export([
	seq/2,
	seq/3,
	map/2,
	filter/2,
	skip/2,
	interleave/2,
	zip/2,
	unzip/1,
	is_empty/1,
	tl/1,
	hd/1,
	nth/2,
	nthtail/2,
	first/2,
	length/1,
	from_list/1,
	foldl/3,
	to_list/1,
	print/1,
	append/2
]).

-compile({no_auto_import, [hd/1]}).
-compile({no_auto_import, [tl/1]}).
-compile({no_auto_import, [length/1]}).

-define(m(H, T), [H | fun() -> T end ]).


seq(I, J) ->
	seq(I, J, 1).

seq(I, J, Inc) when I =< J, Inc > 0 ->
	seq_pos(I, J, Inc);
seq(I, J, Inc) when I >= J, Inc < 0 ->
	seq_neg(I, J, Inc).

seq_pos(I, J, Inc) when I =< J ->
	?m(I, seq_pos(I+Inc, J, Inc));
seq_pos(I, J, _Inc) when I >= J+1 ->
	[].

seq_neg(I, J, Inc) when I >= J ->
	?m(I, seq_neg(I+Inc, J, Inc));
seq_neg(I, J, _Inc) when I =< J-1 ->
	[].

map(Fun, [X | Rest]) ->
	?m(Fun(X), map(Fun, Rest()));
map(_Fun, []) ->
	[].

filter(Fun, [X | Rest]) ->
	case Fun(X) of
		true ->
			?m(X, filter(Fun, Rest()));
		false ->
			filter(Fun, Rest())
	end;
filter(_Fun, []) ->
	[].

skip(N, [_X | Rest]) when N > 0 ->
	skip(N-1, Rest());
skip(0, LL) ->
	LL.

interleave(L1, L2) ->
	interleave1(L1, L2).

interleave1([X1 | Rest1], L2) ->
	?m(X1, interleave2(Rest1(), L2));
interleave1([], L2) ->
	L2.

interleave2(L1, [X2 | Rest2]) ->
	?m(X2, interleave1(L1, Rest2()));
interleave2([], L2) ->
	L2.

zip([X1 | Rest1], [X2 | Rest2]) ->
	?m({X1, X2}, zip(Rest1(), Rest2()));
zip([], []) ->
	[].

unzip(L) ->
	{unzipL(L), unzipR(L)}.


unzipL([{X1, _X2} | Rest]) ->
	?m(X1, unzipL(Rest()));
unzipL([]) ->
	[].

unzipR([{_X1, X2} | Rest]) ->
	?m(X2, unzipR(Rest()));
unzipR([]) ->
	[].

% todo: transformations,
%    unzip(zip(L1, L2)) => {L1, L2}
%    filter(Filter, map(Map, L)) => ??
%    skip(N, map(Map, L)) => map(Map, skip(N, L))

is_empty([_X | _Rest]) ->
	false;
is_empty([]) ->
	true.


tl([_ | Rest]) ->
	Rest().

hd([X | _Rest]) ->
	X.


nth(N, L) ->
	hd(skip(N-1, L)).

nthtail(N, L) when N > 0 ->
	tl(skip(N-1, L));
nthtail(0, L) ->
	L.

first(N, [X | Rest]) when N > 0 ->
	?m(X, first(N-1, Rest()));
first(_N, _) ->
	[].

length(L) ->
	foldl(fun(_,C) -> C+1 end, 0, L).

from_list([H | Tail]) ->
	?m(H, from_list(Tail));
from_list([]) ->
	[].


foldl(Fun, Acc, [X | Rest]) ->
	foldl(Fun, Fun(X, Acc), Rest());
foldl(_Fun, Acc, []) ->
	Acc.

to_list(LL) ->
	L = foldl(fun(X, Acc) -> [X | Acc] end, [], LL),
	lists:reverse(L).

print(LL) ->
	foldl(fun(X, _Acc) -> io:format("~p~n", [X]) end, ok, LL).


append([X | Rest], L2) ->
	?m(X, append(Rest(), L2));
append([], L2) ->
	L2.
