%% Author: Jean-Lou Dupont
%% Created: 2009-08-17
%% Description: Management functions
-module(mng).

%%
%% MACROS
%%


%%
%% Exported Functions
%%
-export([
		 getsubs/1,
		 getbusses/0,
		 getvar/2, getvar/3,
		 add_to_var/2,
		 rem_from_var/2,
		 add_sub/2,
		 rem_sub/2,
		 delete_sub/1,
		 remove_sub_from_busses/2,
		 clean_bus_table/0,
		 clean_busses/1
		 ]).

-export([
		 msg/1, msg/2,
		 tern/4,
		 isdebug/0
		 ]).

-export([
		 extract_host/0, extract_host/1,
		 make_node/1, make_node/2
		 ]).

%%
%% Local Functions
%%

%% @private
getsubs(Bus) ->
	getvar({mswitch, subscribers, Bus}, []).

getbusses() ->
	getvar({mswitch, subs, self()}, []).





%% @private
add_sub(Bus, Sub) ->
	add_to_var({mswitch, busses}, Bus),
	add_to_var({mswitch, subscribers, Bus}, Sub),
	add_to_var({mswitch, subs, Sub}, Bus),
	ok.

%% @private
rem_sub(Bus, Sub) ->
	rem_from_var({mswitch, subscribers, Bus}, Sub),
	rem_from_var({mswitch, subs, Sub}, Bus),
	clean_bus_table(),
	ok.
	


%% Remove a subscriber from the tables
%% @private
delete_sub(Sub) ->
	mng:msg("Deleting sub[~p]", [Sub]),
	Busses=getvar({mswitch, subs, Sub}, []),
	remove_sub_from_busses(Busses, Sub).


%% @private
remove_sub_from_busses([], _Sub) ->
	ok;

remove_sub_from_busses([H|T], Sub) ->
	rem_sub(H, Sub),
	remove_sub_from_busses(T, Sub);

remove_sub_from_busses(Bus, Sub) ->
	rem_sub(Bus, Sub),
	ok.


%% @private
clean_bus_table() ->
	Busses=getvar({mswitch, busses}, []),
	clean_busses(Busses).

%% @private
clean_busses([]) ->
	ok;

clean_busses([Bus|T]) ->
	Subs=getvar({mswitch, subscribers, Bus}, []),
	clean_bus(Bus, Subs),
	clean_busses(T).

%% If empty, remove bus from bus table
%% @private
clean_bus(Bus, []) ->
	rem_from_var({mswitch, busses}, Bus);

clean_bus(_Bus, _) ->
	ok.





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
add_to_var(VarName, VarValue) ->
	List=getvar(VarName, []),
	FilteredList=List--[VarValue],
	NewList=FilteredList++[VarValue],
	put(VarName, NewList).


%% @private
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

make_node(Name, Node) when is_atom(Name) ->
	make_node(erlang:atom_to_list(Name), Node);

make_node(Name , Node) when is_list(Name) ->
	Host=tools:extract_host(Node),
	PartialName=string:concat(Name, "@"),
	CompleteName=string:concat(PartialName, Host),
	erlang:list_to_atom(CompleteName).


