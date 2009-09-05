%% Author: Jean-Lou Dupont
%% Created: 2009-09-05
%% Description: Command-line tools
%%
%%
-module(mswitch_clt).
-compile(export_all).


parse([]) ->
	[];

%% @doc Parses a command line argument list
%%
%% @spec parse(Arguments) -> ParsedList | {error, Reason}
%% where
%%	Arguments = atom() | [term()] | tuple()
%%	@type ParsedList = [Item]
%%	@type Item = {term, term()} | {Option, [term()]}
%%
parse(List) when is_list(List) ->
	do_parse(List, []);

parse(Atom) when is_atom(Atom) ->
	[Atom];

parse(Tuple) when is_tuple(Tuple) ->
	[Tuple()];

parse(Other) ->
	{error, {unknown_term, Other}}.



do_parse([], Acc) ->
	Acc;

%% @private
do_parse([Item|Items], Acc) ->
	case starts_with_hyphen(Item) of
		true ->
			{List, Rest}=accumulate(Item, Items, []),
			do_parse(Rest, Acc++List);
		false ->
			do_parse(Items, Acc++[{term, Item}])
	end.


	
%% @doc Verifies if the atom starts with an hyphen
%%
%% @spec starts_with_hyphen(atom()) -> true | false
starts_with_hyphen(E) when is_atom(E) ->
	[H|_T]=erlang:atom_to_list(E),
	if
		H == $- ->	true;
		true    -> 	false
	end.

accumulate(HeadItem, [], Acc) ->
	{[{HeadItem, Acc}], []};

%% @doc Accumulates Items from the list until
%%		a) an hyphen is found OR
%%		b) end of list
%%
%% @private
accumulate(HeadItem, [Item|Items], Acc) ->
	case starts_with_hyphen(Item) of
		true ->	{[{HeadItem, Acc}], [Item|Items]};
		false -> accumulate(HeadItem, Items, Acc++[Item])
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
