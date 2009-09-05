%% Author: Jean-Lou Dupont
%% Created: 2009-09-05
%% Description: Command-line tools
%%
%%
-module(mswitch_clt).
-compile(export_all).


parse(_, []) ->
	[];

%% @doc Parses a command line argument list.
%%		The parameter 'C' denotes a char() which
%%		serves as 'option' list start.
%%
%% @spec parse(C, Arguments) -> ParsedList | {error, Reason}
%% where
%%	C = char()
%%	Arguments = atom() | [term()] | tuple()
%%	@type ParsedList = [Item]
%%	@type Item = {term, term()} | {Option, [term()]}
%%
parse(C, List) when is_list(List) ->
	do_parse(C, List, []);

parse(_C, Atom) when is_atom(Atom) ->
	[Atom];

parse(_C, Tuple) when is_tuple(Tuple) ->
	[Tuple()];

parse(_, Other) ->
	{error, {unknown_term, Other}}.



do_parse(_C, [], Acc) ->
	Acc;

%% @private
do_parse(C, [Item|Items], Acc) ->
	case starts_with_char(C, Item) of
		true ->
			{List, Rest}=accumulate(C, Item, Items, []),
			do_parse(C, Rest, Acc++List);
		false ->
			do_parse(C, Items, Acc++[{term, Item}])
	end.


	
%% @doc Verifies if the atom starts with a specific character
%%
%% @spec starts_with_char(Char, atom()) -> true | false
%% where 
%%	Char = char()
%%
starts_with_char(C, E) when is_atom(E) ->
	[H|_T]=erlang:atom_to_list(E),
	if
		H == C ->	true;
		true   -> 	false
	end.

accumulate(_, HeadItem, [], Acc) ->
	{[{HeadItem, Acc}], []};

%% @doc Accumulates Items from the list until
%%		a) an C is found OR
%%		b) end of list
%%
%% @private
accumulate(C, HeadItem, [Item|Items], Acc) ->
	case starts_with_char(C, Item) of
		true ->	{[{HeadItem, Acc}], [Item|Items]};
		false -> accumulate(C, HeadItem, Items, Acc++[Item])
	end.

			


	
%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------

t1() ->
	L=['#a', a1, a2, '#b', b1, b2, b3],
	parse($#, L).

t2() ->
	L2=['w1','w2','#a', a1, a2, '#b', b1, b2, b3],
	parse($#, L2).
