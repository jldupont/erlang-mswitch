%% Author: Jean-Lou Dupont
%% Created: 2009-08-19
%% Description: Common function tools
-module(mswitch_tools).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 tern/4,
		 isdebug/0,
		 getvar/2, getvar/3,
		 add_to_var/2, rem_from_var/2,
		 msg/1, msg/2,
		 extract_host/0,
		 make_node/1
		,kfind/2, kfind/3
		 ]).


%% @private
tern(Var, Value, True, False) ->
	case Var of
		Value -> True;
		_     -> False
	end.


%% @private
isdebug() ->
	Params=getvar(params, []),
	lists:member("debug", Params)
	or
	lists:member(debug, Params).


%% Retrieves the Value of a VarName in the process dictionary
%% and returns Default is not found.
%%
%% @spec getvar(VarName, Default) -> Value | Default
%% Value = atom() | string() | integer() | float()
getvar(VarName, Default) ->
	VarValue=get(VarName),
	getvar(VarName, VarValue, Default).

getvar(VarName, undefined, Default) ->
	put(VarName, Default),
	Default;

getvar(_VarName, VarValue, _Default) ->
	VarValue.


%% @private
add_to_var(VarName, VarValue) when is_list(VarValue)->
	List=getvar(VarName, []),
	FilteredList=List--VarValue,
	NewList=FilteredList++VarValue,
	put(VarName, NewList);
	
	
add_to_var(VarName, VarValue) ->
	List=getvar(VarName, []),
	FilteredList=List--[VarValue],
	NewList=FilteredList++[VarValue],
	put(VarName, NewList).


%% @private
rem_from_var(VarName, VarValue) when is_list(VarValue)->
	List=getvar(VarName, []),
	FilteredList=List--VarValue,
	put(VarName, FilteredList);

rem_from_var(VarName, VarValue) ->
	List=getvar(VarName, []),
	FilteredList=List--[VarValue],
	put(VarName, FilteredList).


%% @private
%% @spec msg(Message) -> void()
msg(Message) ->
	msg(Message, []).

%% @private
%% @spec msg(Message, Params) -> void()
msg(Message, Params) ->
	Debug=isdebug(),
	domsg(Debug, Message, Params).

%% @private
domsg(false, _, _) ->
	ok;

domsg(true, Message, Params) ->
	Msg="~s:",
	io:format(Msg++Message++"~n", ["mswitch"]++Params).


%% Extracts the "host" part of a node
%% i.e.  host@node
%%
%% @spec extract_host() -> string()
%%
extract_host() ->
	extract_host(node()).

extract_host(Node) when is_atom(Node) ->
	extract_host(atom_to_list(Node));

extract_host(Node) when is_list(Node) ->
	Tokens = string:tokens(Node, "@"),
	lists:last(Tokens).
	

%% Makes a "short name" node from a name
%%
%% @spec make_node(Name) -> string()
%%
make_node(Name) ->
	make_node(Name, node()).

%% @private
make_node(Name, Node) when is_atom(Name) ->
	make_node(erlang:atom_to_list(Name), Node);

make_node(Name , Node) when is_list(Name) ->
	Host=extract_host(Node),
	PartialName=string:concat(Name, "@"),
	CompleteName=string:concat(PartialName, Host),
	erlang:list_to_atom(CompleteName).



kfind(_Key, []) ->	{};

%% @doc Searches through a tuple list for Key
%%
%% @spec kfind(Key, List) -> {} | {Key, Value}
%% where
%%	Key = atom()
%%	List = [tuple()]
%%	Value = term()
kfind(Key, List) ->
	case is_list(List) of
		true  -> TheList=List;
		false -> TheList=[List]
	end,
	
	case erlang:is_builtin(lists, keyfind, 3) of
		true  ->
			case lists:keyfind(Key,1,TheList) of
				false -> {};
				Tuple -> Tuple
			end;

		false ->
			case lists:keysearch(Key,1,TheList) of
				{value, Value} -> Value;
				_              -> {}
			end
	end.

kfind(Key, [], Default) ->
	{Key, Default};

%% @doc Returns {Key, Default} if not found or {Key, Value} otherwise
%%
%% @spec kfind(Key, List, Default) -> {Key, Value} | {}
%% where
%%	Key = atom()
%%	List = [tuple()]
%%	Value = term()
%%	Default = term()
kfind(Key, List, Default) ->
	case kfind(Key, List) of
		false        -> {Key, Default};
		{Key, Value} ->	{Key, Value}
	end.
