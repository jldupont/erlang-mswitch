%% Author: Jean-Lou Dupont
%% Created: 2009-08-17
%% Description: Management functions
-module(mng).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 getsubs/1,
		 getvar/2, getvar/3,
		 add_to_var/2,
		 rem_from_var/2,
		 add_sub/2,
		 rem_sub/2,
		 delete_sub/1,
		 remove_sub_from_busses/2,
		 clean_bus_table/0,
		 clean_busses/1,
		 reply/2
		 ]).

-export([
		 tern/4,
		 isdebug/0
		 ]).


%%
%% Local Functions
%%

%% @private
getsubs(Bus) ->
	getvar({mswitch, subscribers, Bus}, []).



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
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;

%% @private
reply({From, Context}, Message) ->
	From ! {Context, Message}.




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


