-module(ll_wrap, [L]).
-author('baryluk@smp.if.uj.edu.pl').

-export([
	map/1,
	filter/1,
	skip/1,
	unzip/0,
	is_empty/0,
	tl/0,
	hd/0,
	nth/1,
	nthtail/1,
	first/1,
	length/0,
	foldl/2,
	to_list/0,
	print/0,
	append/1
]).

-define(WRAP(X), {ll_wrap, X}).


map(Fun) -> ?WRAP(ll:map(Fun, L)).
filter(Fun) -> ?WRAP(ll:filter(Fun, L)).
skip(N) -> ?WRAP(ll:skip(N, L)).
unzip() -> ll:unzip(L).
foldl(Fun, Acc) -> ll:foldl(Fun, Acc, L).
to_list() -> ll:to_list(L).
print() -> ll:print(L).

is_empty() -> ll:is_empty(L).
tl() -> ?WRAP(ll:tl(L)).
hd() -> ll:hd(L).

nth(N) -> ll:nth(N, L).
nthtail(N) -> ?WRAP(ll:nthtail(N, L)).
first(N) -> ?WRAP(ll:first(N, L)).
length() -> ll:length(L).

append({ll_wrap, L2}) ->
	?WRAP(ll:append(L, L2));
append(L2) ->
	?WRAP(ll:append(L, L2)).
