%% Author: Jean-Lou Dupont
%% Created: 2009-09-05
%% Description: Command-line tools
%%
%%
-module(mswitch_clt).
-compile(export_all).


parse([]) ->
	[];

parse(Atom) when is_atom(Atom) ->
	[Atom];

parse(Tuple) when is_tuple(Tuple) ->
	[Tuple()];

parse(List) when is_list(List) ->
	do_parse(List, []);

parse(Other) ->
	{error, {unknown_term, Other}}.



do_parse([], Acc) ->
	Acc;

do_parse([Item|Items], Acc) ->
	case starts_with_hyphen(Item) of
		true ->
			{List, Rest}=accumulate(Item, Items, []),
			do_parse(Rest, Acc++List);
		false ->
			do_parse(Items, Acc++[{invalid, Item}])
	end.


	
	
starts_with_hyphen(E) when is_atom(E) ->
	[H|_T]=erlang:atom_to_list(E),
	if
		H == $- ->	true;
		true    -> 	false
	end.

accumulate(HeadItem, [], Acc) ->
	{[{HeadItem, Acc}], []};

%% @doc Accumulates Items from the list until
%%		a) an hyphen is found
%%		b) end of list
accumulate(HeadItem, [Item|Items], Acc) ->
	case starts_with_hyphen(Item) of
		
		true ->	{[{HeadItem, Acc}], [Item|Items]};
		false -> accumulate(HeadItem, Items, Acc++[Item])
	end.

			


	
	
has_pattern(Patterns, Key) when is_atom(Key) ->
	has_pattern(Patterns, erlang:atom_to_list(Key));

has_pattern(Patterns, Key) when is_list(Patterns) ->
	do_has_pattern(false, Patterns, Key).

do_has_pattern(true, _, _) ->
	true;

do_has_pattern(false, [], _Key) ->
	false;
	
do_has_pattern(false, Patterns, Key) ->
	[Pattern|Rest] = Patterns,
	Result=check_pattern(Pattern, Key),
	do_has_pattern(Result, Rest, Key).


check_pattern(Pattern, Key) ->
	Str=erlang:atom_to_list(Pattern),
	case string:str(Key, Str) of
		0 -> false;
		_ -> true
	end.



%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------

t1() ->
	L=['-a', a1, a2, '-b', b1, b2, b3],
	parse(L).

t2() ->
	L2=['w1','w2','-a', a1, a2, '-b', b1, b2, b3],
	parse(L2).
