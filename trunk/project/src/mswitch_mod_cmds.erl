%% Author: Jean-Lou Dupont
%% Created: 2009-09-18
%% Description:
%%
%% @doc
%%
%%  = Commands =
%%  /                    Display list of commands
%%  /add bn              Adds bus 'bn' to current active list
%%  /add b1 b2 ...       Adds busses [b1, b2, ...] to current active list
%%  /sub                 Display current list subscriptions (lists the bus/busses) 
%%  /create X            Create list X
%%  /del X               Unsubscribe & delete list X 
%%	/sel                 Display current active list
%%	/sel X               Select list X as current
%%
%% @TODO
%%  /lists               Display the available lists
%%
%%  = Data Model =
%%
%%	{userlists, User}       -> [Lists]
%%	{userlist,  User, List}	-> [Busses]
%%	{selection, User}       -> List
%%

-module(mswitch_mod_cmds).
-compile(export_all).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(TOOLS, mswitch_mod_tools).
-define(LOG,   ?TOOLS:log).

-define(CMDS, ["-", "-add", "-sub", "-create", "-del", "-sel", "-lists" ]).


get_cmds() ->
	?CMDS.


handle_message(ThisBot, From, Body) ->
	Stripped=string:strip(Body),
	Tokens=string:tokens(Stripped, " "),
	%?LOG(tokens, "~p~n", [Tokens]),
	?INFO_MSG("MOD_MSWITCH: Tokens: ~p", [Tokens]),
	dispatch_cmd(ThisBot, From, Tokens).


dispatch_cmd(_, _, []) ->
	ok;

dispatch_cmd(ThisBot, User, ["-"|_T]) ->
	send_command_reply(ThisBot, User, {ok, "Commands: ~p", [get_cmds()]});

dispatch_cmd(ThisBot, User, ["-sub"|Rest]) ->
	{Status, Msg, Params}=do_sub(ThisBot, User, Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, ["-add"|Rest]) ->
	{Status, Msg, Params}=do_add(ThisBot, User, Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, ["-create"|Rest]) ->
	{Status, Msg, Params}=do_create(ThisBot, User, Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, ["-del"|Rest]) ->
	{Status, Msg, Params}=do_del(ThisBot, User, Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, ["-sel"|Rest]) ->
	{Status, Msg, Params}=do_sel(ThisBot, User, Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, ["-lists"|_Rest]) ->
	{Status, Msg, Params}=do_lists(ThisBot, User),
	send_command_reply(ThisBot, User, {Status, Msg, Params});


dispatch_cmd(ThisBot, User, _) ->
	send_command_reply(ThisBot, User, {error, "Eh?"}).




send_command_reply(From, To, {Status, Fmt, Args}) ->
    send_command_reply(From, To, {Status, io_lib:format(Fmt, Args)});
send_command_reply(From, To, {ok, ResponseIoList}) ->
    send_chat(From, To, ResponseIoList);
send_command_reply(From, To, {error, ResponseIoList}) ->
    send_chat(From, To, ResponseIoList);
send_command_reply(_From, _To, noreply) ->
    ok.



send_chat(From, To, {Fmt, Args}) ->
    send_chat(From, To, io_lib:format(Fmt, Args));
send_chat(From, To, IoList) ->
    send_message(From, To, "chat", lists:flatten(IoList)).


send_message(From, To, TypeStr, BodyStr) ->
    XmlBody = {xmlelement, "message",
	       [{"type", TypeStr},
		{"from", jlib:jid_to_string(From)},
		{"to", jlib:jid_to_string(To)}],
	       [{xmlelement, "body", [],
		 [{xmlcdata, BodyStr}]}]},
    %?LOG(msg, "Delivering ~p -> ~p~n~p", [From, To, XmlBody]),
    ejabberd_router:route(From, To, XmlBody).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HANDLERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


do_sub(_ThisBot, User, _) ->
	SelList=?TOOLS:cget(selection, User),
	Busses=?TOOLS:cget(busses, {User, SelList}),
	{ok, "Busses defined <~p>", [Busses]}.


do_add(_ThisBot, _User, []) ->
	{error, "Nothing to add", []};

%% Add bus/busses to the current selection
%%
do_add(_ThisBot, User, Params) ->
	Sel=?TOOLS:cget(selection, User),
	Busses=?TOOLS:cget(busses, {User, Sel}),
	NewBusses=add_unique(Busses, Params),
	?TOOLS:set_list(User, Sel, NewBusses),
	do_sync(User),
	{ok, "Selection<~p> Busses<~p>", [Sel, NewBusses]}.


%% Creates a list if not already existing
%%
do_create(_ThisBot, User, Lists) ->
	DoneList=iter_do_create(User, Lists, []),
	{ok, "Lists created <~p>", [DoneList]}.


do_del(_ThisBot, _User, []) ->
	{error, "Specify a list to delete", []};
	
do_del(_ThisBot, User, List) when is_list(List) ->
	DoneList=iter_do_del(User, List, []),
	{ok, "Deleted the following lists: ~p", [DoneList]}.

%% Returns the current Selection
%%
do_sel(_ThisBot, User, []) ->
	Sel=?TOOLS:cget(selection, User),
	{ok, "Current Selection <~p>", [Sel]};
	

do_sel(_ThisBot, User, [Sel|_]) ->
	Lists=?TOOLS:cget(lists,  User),
	maybe_do_sel(User, Sel, Lists).


do_lists(_ThisBot, User) ->
	Lists=?TOOLS:cget(lists,  User),
	{ok, "Defined lists<~p>", [Lists]}.



do_sync(UserJID) ->
	Pid = ?TOOLS:get_consumer_pid(UserJID),
	safe_msg(UserJID, Pid, reload).


safe_msg(UserJID, Pid, Msg) ->
	try
		Pid ! Msg,
		ok
	catch
		X:Y ->
			?INFO_MSG("safe_msg: exception whilst sending to UserJID: ~p  PID: ~p", [UserJID, Pid])
	end.
	

%% No lists exist... but the DEFAULT one is implicit...
%% Lets create it!
%%
maybe_do_sel(User, _Sel, undefined) ->
	?TOOLS:set_lists(User, ["default"]),
	?TOOLS:set_selection(User, "default"),
	do_sync(User),
	{ok, "Changed to Selection<~p>", ["default"]};
	
maybe_do_sel(User, Sel, Lists) ->
	?INFO_MSG("MAYBE_DO_SEL: Sel<~p> Lists<~p>",[Sel, Lists]),
	case lists:member(Sel, Lists) of
		true ->
			?TOOLS:set_selection(User, Sel),
			do_sync(User),
			{ok, "Changed to Selection<~p>", [Sel]};
		_ ->
			{error, "List does not exist", []}
	end.
	

iter_do_create(_User, [], Acc) -> Acc;

iter_do_create(User, [List|Rest], Acc) ->
	case maybe_create(User, List) of 
		created ->
			iter_do_create(User, Rest, Acc++[List]);
		_ ->
			iter_do_create(User, Rest, Acc)
	end.
	
	
maybe_create(User, List) ->
	Lists=?TOOLS:cget(lists, User),
	maybe_create(User, List, Lists).

maybe_create(User, List, undefined) ->
	?TOOLS:set_lists(User, ["default", List]),
	List;

maybe_create(User, List, Lists) ->
	case lists:member(List, Lists) of
		true ->
			already_exists;
		false ->
			?TOOLS:set_lists(User, Lists++[List]),
			created
	end.



iter_do_del(_User, [], Acc) -> Acc;

iter_do_del(User, [El|Rest], Acc) ->
	?TOOLS:set_list(User, El, []),
	iter_do_del(User, Rest, Acc++[El]).

	


add_unique(undefined, Elements) ->
	add_unique([], Elements);

add_unique(List, Elements) when is_list(Elements) ->
	Filtered=List--Elements,
	Filtered++Elements;

add_unique(List, Element) ->
	Filtered=List--[Element],
	Filtered++[Element].

