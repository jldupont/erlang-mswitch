%% Author: Jean-Lou Dupont
%% Created: 2009-08-19
%% Description: TODO: Add description to tools
-module(tools).

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
		 ]).


%% @private
tern(Var, Value, True, False) ->
	case Var of
		Value -> True;
		_     -> False
	end.


%% @private
isdebug() ->
	Params=mng:getvar(params, []),
	lists:member("debug", Params)
	or
	lists:member(debug, Params).



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
%% @spec msg(Message) -> _
msg(Message) ->
	msg(Message, []).

%% @private
%% @spec msg(Message, Params) -> _
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
	Host=tools:extract_host(Node),
	PartialName=string:concat(Name, "@"),
	CompleteName=string:concat(PartialName, Host),
	erlang:list_to_atom(CompleteName).


